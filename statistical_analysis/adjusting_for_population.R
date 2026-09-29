library(haven)
library(tidyverse)
library(foreign)
library(sf)
library(purrr)
##### Load Datasets Once #####
vslcoordtab <- read.spss("VSLCOORDTAB2024V1.sav", to.data.frame = TRUE)
municipal_maps <- st_read("municipalities_2024.gpkg")

nb_types <- st_read("neighbourhood_types.gpkg") 

GBAPERSOONKTAB <- read.spss("GBAPERSOONKTAB2013V1.sav", to.data.frame = TRUE)
GBAadressobjectbus <- read.spss("GBAADRESOBJECT2013BUSV1.sav", to.data.frame = TRUE)
InkomenBestedingen <- read_sav("INHA2013TABV3.sav", col_select= c("RINPERSOONHKW","INHBESTINKH", "INHGESTINKH", "INHEHALGR", "INHAHL"))
koppel <- read.spss("KOPPELPERSOONHUISHOUDEN2013.sav",  to.data.frame = TRUE)

##### Linking Addresses #####
polycentric_cities <- municipal_maps %>%
  filter(statnaam %in% c("Amsterdam")) # "Amsterdam", Rotterdam", "Utrecht", "'s-Gravenhage"

vsl <- st_as_sf(vslcoordtab, coords = c("VRLXCOORDADRES", "VRLYCOORDADRES"), crs = 28992) 

  pop_vsl <- st_intersection(vsl, polycentric_cities)
  
  GBA_per <- pop_vsl %>%
    inner_join(GBAadressobjectbus, by ="RINOBJECTNUMMER") 
  
  GBA_per_ind <- GBA_per %>%
    inner_join(GBAPERSOONKTAB, by = "RINPERSOON")
  
  GBA_per_ind$GBADATUMAANVANGADRESHOUDING <- as.Date(GBA_per_ind$GBADATUMAANVANGADRESHOUDING , format = "%Y%m%d")
  
  GBA <- GBA_per_ind %>%
    filter(GBADATUMAANVANGADRESHOUDING <= "2013-12-31") %>% ###CHANGEEEEE
    group_by(RINPERSOON) %>%
    slice_max(order_by = GBADATUMAANVANGADRESHOUDING, n = 1) %>%
    ungroup
  
   inkomen <- koppel %>%
     inner_join(InkomenBestedingen, by = "RINPERSOONHKW")
   
   GBA_inkomen <- GBA %>%
     left_join(inkomen, by = "RINPERSOON") %>%
     distinct(RINPERSOONHKW, .keep_all = TRUE)

  nb_types_filtered <- nb_types %>%  ###CHANGE 2016/2019
    select(nbtype_2016, crs28992res100m) %>% 
    rename(
      nbtype = nbtype_2016
    ) %>%
    mutate(
      nbtype = case_when(
        nbtype == "2" ~ "compact mid-rise",
        nbtype == "9"  ~ "compact high-rise",
        nbtype == "3" |  nbtype == "4" ~ "open mid-high",
        nbtype == "5"  ~ "compact low",
        nbtype == "6"   ~ "open low 1",
        nbtype == "8"  ~ "open low 2",
        nbtype == "1" |  nbtype == "7" ~ "sparsely built",
        TRUE ~ nbtype
      )) %>%
    drop_na()
  
  GBA_nb <- st_intersection(GBA_inkomen, nb_types_filtered) 
  
  GBA_nb <- GBA_nb %>%
    distinct(RINPERSOON, .keep_all = TRUE) 
  
  GBA_nb_missing <- GBA_inkomen %>%
    filter(!(RINPERSOON %in% GBA_nb$RINPERSOON))
  
  GBA_nb__matched <- st_join(
    GBA_nb_missing,
    nb_types_filtered, 
    join = st_nearest_feature
  )
  
  GBA_nbtype <- rbind(GBA_nb, GBA_nb__matched)
  
  rm(GBA_nb__matched,GBA_nb_missing,GBA_nb)
  
  GBA_nbtype_ng <- GBA_nbtype
  GBA_nbtype_ng$geom <- NULL
  table(GBA_nbtype$nbtype)
  
output_path <- ""
#write.csv(GBA_nbtype_ng, output_path)

##### Configuration #####
BASE_PATH <- ""
CITIES <- c(Amsterdam = "AMS", Rotterdam = "ROT", Hague = "HAG", Utrecht = "UTR")
YEARS <- 2013:2019

# Income 20th/60th percentile obtained from STATLINE CBS (incl. students)

INCOME_THRESHOLDS <- c(
  '2013' = 15600, 
  '2014' = 16000, 
  '2015' = 16300, 
  '2016' = 16800, 
  '2017' = 17200, 
  '2018' = 17600, 
  '2019' = 18400  
)

INCOME_THRESHOLDS_MAX <- c(
  '2013' = 29700, 
  '2014' = 30600, 
  '2015' = 31200, 
  '2016' = 32300, 
  '2017' = 33000, 
  '2018' = 33500, 
  '2019' = 35100  
)

# --- PROCESS SINGLE FILE ---
process_file <- function(city, code, yr) {
  path <- file.path(BASE_PATH, paste0(yr, "_", code, ".csv"))
  if(!file.exists(path)) {
    warning(paste("File not found:", path))
    return(NULL)
  }
  read_csv(path, show_col_types = FALSE) %>%
     mutate(GBAGEBOORTEJAAR = as.numeric(GBAGEBOORTEJAAR)) %>%
     filter(INHGESTINKH < 999999999) %>%
     mutate(city = city, 
           year = yr) %>%
     mutate(
        income = as.factor(case_when(
        INHGESTINKH <= INCOME_THRESHOLDS[as.character(yr)] ~ "low SES",
        INHGESTINKH > INCOME_THRESHOLDS[as.character(yr)] & INHGESTINKH < INCOME_THRESHOLDS_MAX[as.character(yr)] ~ "mid SES",
        INHGESTINKH >= INCOME_THRESHOLDS_MAX[as.character(yr)] ~ "high SES")),
        age = case_when(
        GBAGEBOORTEJAAR <  (yr - 65) ~ "age_65+",
        GBAGEBOORTEJAAR >= (yr - 5) ~ "age_0-5",
        GBAGEBOORTEJAAR <= (yr - 6) & GBAGEBOORTEJAAR >= (yr - 35) ~ "age_6-35",
        TRUE  ~ "age_36-65")
     )
}

# --- PROCESS ALL FILES ---
all_data <- do.call(rbind, lapply(names(CITIES), function(city) {
  do.call(rbind, lapply(YEARS, function(yr){
    process_file(city, CITIES[city], yr)
  }))
}))

###
subgroup_population <- all_data %>%
  group_by(city, nbtype, year, income, age, GBAGESLACHT) %>%
  summarise(
    pop_offset = n(),
    .groups = "drop") %>%
  arrange(city, nbtype) %>%
  rename(
    statnaam = city
  ) 

subgroup_population_collapse <- subgroup_population %>%
   mutate(
   nbtype = case_when(
   nbtype == "compact high-rise" ~ "compact mid-rise",
   TRUE ~ nbtype)
   ) %>%
  group_by(statnaam, nbtype, year, income, age, GBAGESLACHT) %>%
  summarise(
    pop_offset = sum(pop_offset, na.rm = TRUE), 
    .groups = "drop"
  )

subgroup_population_collapse$statnaam[subgroup_population_collapse$statnaam == "Hague"] <- "'s-Gravenhage"

##### Descriptive Stat #####
# Pool all years and compute cell-level socio-demogrpahics counts
cell_demo <- all_data %>%
  mutate(
    nbtype = case_when(
      nbtype == "compact high-rise" ~ "compact mid-rise",
      TRUE ~ nbtype)) %>%
  group_by(crs28992res100m, statnaam, nbtype, income) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(n_avg = ceiling(n / length(unique(all_data$year)))) %>%
  group_by(crs28992res100m, statnaam, nbtype) %>%
  mutate(cell_total = sum(n_avg)) %>%
  ungroup()

# Income proportions per cell (crs28992res100m)
cell_SES <- cell_demo %>%
  group_by(crs28992res100m, statnaam, nbtype, income) %>%
  summarise(n_income = sum(n_avg), 
            cell_total = first(cell_total),
            .groups = "drop") %>%
  mutate(pct_inc = n_income / cell_total) 


###### Weighted Population Risk Mapping ###### 

joint_low  <- results_lowSES %>%
  filter(exposure == "Compound", percentile == "99th") %>%
  select(nbtype, RR) %>% mutate(income = "low SES")

joint_mid <- results_midSES %>%
  filter(exposure == "Compound", percentile == "99th") %>%
  select(nbtype, RR) %>% mutate(income = "mid SES")

joint_high  <- results_highSES %>%
  filter(exposure == "Compound", percentile == "99th") %>%
  select(nbtype, RR) %>% mutate(income = "high SES")

joint_SES <- rbind(joint_low, joint_mid, joint_high)

joint_SES <- joint_SES %>%
  mutate(exposure = "Q + Ozone")

weighted_risks <- cell_SES %>%
  inner_join(joint_SES, by = c("nbtype", "income")) %>%
  mutate(weighted_RR = exp(pct_inc*log(RR))) %>%
  mutate(
    weighted_RR_Q = case_when(
      weighted_RR > 0.70 & weighted_RR <= 0.95  ~ "<= 0.95",
      weighted_RR > 0.95 & weighted_RR <= 1.05  ~ ">0.95, <= 1.05",
      weighted_RR > 1.05 & weighted_RR <= 1.15  ~ ">1.05 <= 1.15",
      weighted_RR > 1.15 & weighted_RR <= 1.25  ~ ">1.15, <= 1.25",
      weighted_RR > 1.25 ~ "> 1.25")) %>%
  filter(n_income >= 5)

output_path <- ""
write.csv2(weighted_risks, output_path, row.names = FALSE)

################ Concentration index ################ 
compute_ses_lq <- function(data, period_label) {
  
  n_years <- length(unique(data$year))
  
  # Raw counts: city x LCZ x income 
  cell_counts<- data  %>%
   # filter(age == "age_0-5") %>%
    mutate(
      nbtype = case_when(
        nbtype == "compact high-rise" ~ "compact mid-rise",
        TRUE ~ nbtype)) %>%
    group_by(statnaam, nbtype, income) %>% ### CHANGE 
    summarise(n_raw = n(), .groups = "drop") %>%
    mutate(n_avg = if(n_years > 1) n_raw/n_years else n_raw)
  
  # City x income totals
  city_age_totals <- cell_counts %>%
    group_by(statnaam, income) %>% ### CHANGE 
    summarise(n_city_income = sum(n_raw),
              n_avg_city_income = sum(n_avg), .groups = "drop") 
    
  # City x lcz totals
  lcz_totals <- cell_counts %>%
    group_by(statnaam, nbtype) %>%
    summarise(n_lcz_total= sum(n_raw),
              n_avg_lcz_total = sum(n_avg), .groups = "drop") 
    
  # City 
  city_totals <- cell_counts %>%
    group_by(statnaam) %>%
    summarise(n_city_total = sum(n_raw),
              n_avg_city_total = sum(n_avg), .groups = "drop") 
  
  #Assemble
  cell_counts %>%
    left_join(city_age_totals, by = c("statnaam", "income")) %>% ### CHANGE 
    left_join(lcz_totals, by = c("statnaam", "nbtype")) %>%
    left_join(city_totals, by = "statnaam") %>%
    mutate(
      pct_lcz_of_ses = n_avg / n_avg_city_income * 100, # pct_lcz_of_ses
      pct_pop_in_lcz = n_avg_lcz_total / n_avg_city_total * 100,
      LQ = pct_lcz_of_ses / pct_pop_in_lcz, # pct_lcz_of_ses
      period = period_label,
      n_years = n_years
    ) %>%
    mutate(
      abr_city = case_when(
        statnaam == "Amsterdam" ~ "AMS",
        statnaam == "Utrecht" ~ "UTR",
        statnaam == "Rotterdam" ~ "ROT",
        statnaam == "'s-Gravenhage" ~ "HAG"
      )
    )
}

ses_lq_2013_income <- compute_ses_lq(all_data %>% filter(year == 2013), "2013")
ses_lq_2019_income <- compute_ses_lq(all_data %>% filter(year == 2019), "2019")
ses_lq_pooled <- compute_ses_lq(all_data, "2013-2019")

ses_all <- rbind(ses_lq_2013_income, ses_lq_2019_income, ses_lq_pooled)


output_path <- ""
write.csv(ses_all, output_path)

################ Concentration index ################ 
ses_lq_stat <- ses_lq_2019_income %>%
  group_by(nbtype, income) %>%
  summarise(
    LQ_mean = mean(LQ),
    LQ_sd = sd(LQ),
    n_cities = n(),
    .groups = "drop"
  ) %>%
  mutate(statnaam ="Pooled")
    
lcz_order <- c(
  "sparsely built",
  "open low 1",
  "open low 2",
  "compact low",
  "open mid-high",
  "compact mid-rise"
)

ses_lq_2019_income$nbtype <- factor(ses_lq_2019_income$nbtype, levels = lcz_order)
ses_lq_2019_income$income <- factor(ses_lq_2019_income$income, levels = c("low SES", "mid SES", "high SES"))
ses_lq_2019_income$statnaam <- factor(ses_lq_2019_income$statnaam, levels = c("Amsterdam", "Rotterdam", "'s-Gravenhage", "Utrecht"))

ggplot(ses_lq_2019_income, aes(x = LQ, y = nbtype, shape = statnaam,
                   fill = statnaam)) +
  annotate("rect", xmin = 0.95, xmax = 1.05,
           ymin = -Inf, ymax = Inf,
           fill = "grey89", alpha = 0.5) +
  geom_vline(xintercept = 1, linetype = "solid", color = "grey60", linewidth = 0.3) +
  geom_point(size = 3, stroke = 0.8, alpha = 0.7) +
  geom_point(data = ses_lq_stat,
    aes(x = LQ_mean, y = nbtype),
    shape = 4, size = 3, stroke = 2,
    color = "red", inherit.aes = FALSE
  ) +
  geom_errorbar(data = ses_lq_stat,
              aes(xmin = LQ_mean - LQ_sd,
                  xmax = LQ_mean +LQ_sd, 
                  y = nbtype),
              height = 0.2, linewidth = 0.5,
              color = "black", inherit.aes = FALSE) +
  facet_wrap(~ income, ncol =1) +
  # geom_text(aes(label = abr_city),
  #           size = 2.5, fontface = "bold", alpha = 0.8,
  #           angle = 45, vjust = 0, hjust = -0.3) +
  scale_shape_manual(values = c(
    "Amsterdam" = 21,
    "Rotterdam" = 21,
    "Utrecht" = 21,
    "'s-Gravenhage" = 21
  )) +
  scale_fill_manual(values = c(
    "Amsterdam" = "black",
    "Rotterdam" = "grey30",
    "Utrecht" = "grey60",
    "'s-Gravenhage" = "white"
  )) +
  scale_x_continuous(
    breaks = seq(0.7, 1.4, by= 0.1),
    labels = function(x) ifelse(x == 1.0, "1.0", format(x, nsmall =1))) +
  guides(fill = guide_legend(override.aes = list(override.aes = list(shape = c(21, 24, 22, 23))))) +
  labs(
    x = "Concentration Index (1.0 = proportional)",
    y = NULL,
    fill = "statnaam",
    title = "Fig.1b - Spatial concentration of SES groups (age_65+) across LCZ types 2019") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.title = element_text( hjust =1)
  )

#### Concentration INDEX
# pop_shift <- all_data %>%
#   filter(income == "high SES") %>%
#   mutate(
#     nbtype = case_when(
#       nbtype == "compact high-rise" ~ "compact mid-rise",
#       TRUE ~ nbtype)) %>%
#   group_by(statnaam, year, nbtype) %>%
#   summarise(n =n(), .groups = "drop") %>%
#   pivot_wider(names_from = year, values_from = n,
#               names_prefix =  "y") %>%
#   mutate(abs_change = y2019 - y2013, 
#          pct_chnage = (y2019 / y2013 - 1) * 100) %>%
#   arrange(statnaam, desc(abs_change))
# 
