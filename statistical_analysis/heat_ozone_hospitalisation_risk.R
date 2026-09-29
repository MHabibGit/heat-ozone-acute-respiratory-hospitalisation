# ============================================================
# DHD/CBS Hospitalization Linking Pipeline
# Acute respiratory/cardiovascular/mental health admissions (ICD-10
# J/I/F codes) linked to residence, income, and polycentric city area.
# ============================================================
# RUN THIS SCRIPT ONCE PER YEAR (2013-2019). Set TARGET_YEAR below.
#
# IMPORTANT: CBS microdata file versions (V1/V2/V3 suffixes) are NOT
# consistent across years and can change if CBS reissues a table.
# Before each run, confirm the current filenames against the CBS
# microdata catalogue (https://www.cbs.nl/microdata) or your
# institution's remote-access portal — do not assume the filenames
# below are still current, especially for years beyond what's been
# verified here.

library(haven)
library(tidyverse)
library(foreign)
library(sf)
library(car)
library(fastDummies)
library(dlnm)
library(splines)
library(AER)
library(gnm)
library(broom)
library(MuMIn)

##### Load Datasets Once #####  
vslcoordtab <- read.spss("VSLCOORDTAB2024V1.sav", to.data.frame = TRUE)
municipal_maps <- st_read("https://service.pdok.nl/cbs/gebiedsindelingen/2024/wfs/v1_0?request=GetFeature&service=WFS&version=1.1.0&outputFormat=application%2Fjson&typeName=gebiedsindelingen:gemeente_gegeneraliseerd")

polycentric_cities <- municipal_maps %>%
  filter(statnaam %in% c("Amsterdam", "Rotterdam", "Utrecht", "'s-Gravenhage"))

### REFERENCES 
# LBZbasis2014TABV2  # INHA2014TABV1
# LBZbasis2015TABV2  # INHA2015TABV1 
# LBZbasis2016TABV1  # INHA2016TABV2
# LBZbasis2017TABV1  # INHA2017TABV2
# LBZBASIS2018TABV2  # INHA2018TABV2
# LBZBASIS2019TABV1  # INHA2019TABV2

##### Re-Load Datasets ##### CHANGE DATES - RUN AGAIN
LBZBASISTABV1 <- read.spss("INHA2019TABV1.sav", to.data.frame = TRUE)
GBAPERSOONKTAB <- read.spss("GBAPERSOONKTAB2019V1.sav", to.data.frame = TRUE)
GBAadressobjectbus <- read.spss("GBAADRESOBJECT2019BUSV1.sav", to.data.frame = TRUE)
InkomenBestedingen <- read_sav("INHA2019TABV2.sav", col_select= c("RINPERSOONHKW","INHBESTINKH", "INHGESTINKH", "INHEHALGR", "INHAHL"))
koppel <- read.spss("KOPPELPERSOONHUISHOUDEN2019V2.sav",  to.data.frame = TRUE)

##### DHD Cleaning ##### 
LBZBASISTABV1$LBZOpnamedatum <- as.Date(LBZBASISTABV1$LBZOpnamedatum , format = "%Y%m%d")

LBZBASISTABV1$LBZIcd10hoofddiagnose <- factor(trimws(as.character(LBZBASISTABV1$LBZIcd10hoofddiagnose)))
LBZBASISTABV1$LBZHerkomst <- factor(trimws(as.character(LBZBASISTABV1$LBZHerkomst)))

LBZBASISTABV1_summer <- LBZBASISTABV1 %>%
  filter(LBZOpnamedatum >= as.Date("2019-05-01"), ## CHANGE DATES
         LBZOpnamedatum <= as.Date("2019-09-30")) %>% ## CHANGE DATES
  filter(LBZHerkomst == "Eigen woonomgeving" | 
         LBZHerkomst == "Overige instellingen" | 
         LBZHerkomst == "Instelling voor verpleging/verzorging") %>%
filter(str_detect(LBZIcd10hoofddiagnose, "^(J[0-9][0-9]|J[0-9][0-9][0-9]|J[0-9][0-9][0-9][0-9]|
                    I[0-9][0-9]|I[0-9][0-9][0-9]|I[0-9][0-9][0-9][0-9]|
                    F[0-9][0-9]|F[0-9][0-9][0-9]|F[0-9][0-9][0-9][0-9])$"))

hist(LBZBASISTABV1_summer$LBZOpnamedatum, breaks = "days")

##### Linking Addresses #####
LBZ_GB <- LBZBASISTABV1_summer %>%
  inner_join(GBAPERSOONKTAB, by = c("RINPERSOON"))

LBZ_GBA <- LBZ_GB %>%
  inner_join(GBAadressobjectbus, by = c("RINPERSOON")) 

##### Obtaining first admission #####
LBZ_GBA_sliced <- LBZ_GBA %>%
  mutate(
    GBADATUMAANVANGADRESHOUDING = as.Date(GBADATUMAANVANGADRESHOUDING , format = "%Y%m%d"),
    LBZOpnamedatum = as.Date(LBZOpnamedatum , format = "%Y%m%d"),
         ) %>%
  filter(GBADATUMAANVANGADRESHOUDING <= LBZOpnamedatum) %>%
  group_by(RINPERSOON, LBZOpnamedatum) %>%
  slice_max(order_by = GBADATUMAANVANGADRESHOUDING, n = 1, with_ties = FALSE) %>%
  ungroup()
  # group_by(RINPERSOON, LBZOpnamedatum) %>%
  # slice_min(order_by = LBZOpnamedatum, n = 1, with_ties = FALSE) %>%
  # ungroup() 

####
LBZ_VSL <- LBZ_GBA_sliced %>%
  inner_join(vslcoordtab, by = c("RINOBJECTNUMMER")) 

LBZ <- st_as_sf(LBZ_VSL, coords = c("VRLXCOORDADRES", "VRLYCOORDADRES"), crs = 28992)


LBZ_PCC <- st_intersection(LBZ, polycentric_cities)

###
inkomen <- koppel %>%
  inner_join(InkomenBestedingen, by = "RINPERSOONHKW")

LBZ_inkomen <- LBZ_PCC %>%
  left_join(inkomen, by = "RINPERSOON")
  
LBZ_filtered <- LBZ_inkomen %>%
  dplyr::select(RINPERSOON, RINOBJECTNUMMER, RINPERSOONHKW, statnaam, GBAGESLACHT, 
                GBAGEBOORTEJAAR, GBAGEBOORTEMAAND, GBAGEBOORTEDAG, INHBESTINKH, INHGESTINKH, 
                INHEHALGR, INHAHL, LBZUrgentie, LBZIcd10hoofddiagnose, LBZOpnamedatum, 
                LBZHerkomst, GBADATUMAANVANGADRESHOUDING, LBZOpnameuur) 

output_path <- "" # e.g. LBZPC_2019_n.gpkg, LBZPC_2018_n.gpkg etc...
st_write(LBZ_filtered, output_path, delete_dsn = TRUE)

##### Imputing Inkomen ##### 
# income is imputted by looking 2 years back and 2 year ahead

LBZ_2019 <- st_read("LBZPC_2019_n.gpkg")
LBZ_2018 <- st_read("LBZPC_2018_n.gpkg")
LBZ_2017 <- st_read("LBZPC_2017_n.gpkg")
LBZ_2016 <- st_read("LBZPC_2016_n.gpkg")
LBZ_2015 <- st_read("LBZPC_2015_n.gpkg")
LBZ_2014 <- st_read("LBZPC_2014_n.gpkg")
LBZ_2013 <- st_read("LBZPC_2013_n.gpkg")

InkomenBestedingen_20 <- read_sav("INHA2020TABV2.sav", col_select= c("RINPERSOONHKW","INHBESTINKH", "INHGESTINKH", "INHEHALGR", "INHAHL"))
InkomenBestedingen_19 <- read_sav("INHA2019TABV2.sav", col_select= c("RINPERSOONHKW","INHBESTINKH", "INHGESTINKH", "INHEHALGR", "INHAHL"))
InkomenBestedingen_18 <- read_sav("INHA2018TABV2.sav", col_select= c("RINPERSOONHKW","INHBESTINKH", "INHGESTINKH", "INHEHALGR", "INHAHL"))
InkomenBestedingen_17 <- read_sav("INHA2017TABV2.sav", col_select= c("RINPERSOONHKW","INHBESTINKH", "INHGESTINKH", "INHEHALGR", "INHAHL"))
InkomenBestedingen_16 <- read_sav("INHA2016TABV2.sav", col_select= c("RINPERSOONHKW","INHBESTINKH", "INHGESTINKH", "INHEHALGR", "INHAHL"))
InkomenBestedingen_15 <- read_sav("INHATAB/INHA2015TABV1.sav", col_select= c("RINPERSOONHKW","INHBESTINKH", "INHGESTINKH", "INHEHALGR", "INHAHL"))
InkomenBestedingen_14 <- read_sav("INHA2014TABV1.sav", col_select= c("RINPERSOONHKW","INHBESTINKH", "INHGESTINKH", "INHEHALGR", "INHAHL"))
InkomenBestedingen_13 <- read_sav("INHA2013TABV3.sav", col_select= c("RINPERSOONHKW","INHBESTINKH", "INHGESTINKH", "INHEHALGR", "INHAHL"))
InkomenBestedingen_12 <- read_sav("INHA2012TABV3.sav", col_select= c("RINPERSOONHKW","INHBESTINKH", "INHGESTINKH", "INHEHALGR", "INHAHL"))
InkomenBestedingen_11 <- read_sav("INHA2011TABV3.sav", col_select= c("RINPERSOONHKW","INHBESTINKH", "INHGESTINKH", "INHEHALGR", "INHAHL"))

koppel_20 <- read.spss("KOPPELPERSOONHUISHOUDEN2020V2.sav",  to.data.frame = TRUE)
koppel_19 <- read.spss("KOPPELPERSOONHUISHOUDEN2019V2.sav",  to.data.frame = TRUE)
koppel_18 <- read.spss("KOPPELPERSOONHUISHOUDEN2018.sav",  to.data.frame = TRUE)
koppel_17 <- read.spss("KOPPELPERSOONHUISHOUDEN2017.sav",  to.data.frame = TRUE)
koppel_16 <- read.spss("KOPPELPERSOONHUISHOUDEN2016.sav",  to.data.frame = TRUE)
koppel_15 <- read.spss("KOPPELPERSOONHUISHOUDEN2015.sav",  to.data.frame = TRUE)
koppel_14 <- read.spss("KOPPELPERSOONHUISHOUDEN2014.sav",  to.data.frame = TRUE)
koppel_13 <- read.spss("KOPPELPERSOONHUISHOUDEN2013.sav",  to.data.frame = TRUE)
koppel_12 <- read.spss("KOPPELPERSOONHUISHOUDEN2012.sav",  to.data.frame = TRUE)
koppel_11 <- read.spss("KOPPELPERSOONHUISHOUDEN2011.sav",  to.data.frame = TRUE)

InkomenBestedingen20 <- InkomenBestedingen_20 %>%
  inner_join(koppel_20, by = "RINPERSOONHKW") %>%
  distinct(RINPERSOONHKW, .keep_all = TRUE)

InkomenBestedingen19 <- InkomenBestedingen_19 %>%
  inner_join(koppel_19, by = "RINPERSOONHKW") %>%
  distinct(RINPERSOONHKW, .keep_all = TRUE)

InkomenBestedingen18 <- InkomenBestedingen_18 %>%
  inner_join(koppel_18, by = "RINPERSOONHKW") %>%
  distinct(RINPERSOONHKW, .keep_all = TRUE)

InkomenBestedingen17 <- InkomenBestedingen_17 %>%
  inner_join(koppel_17, by = "RINPERSOONHKW") %>%
  distinct(RINPERSOONHKW, .keep_all = TRUE)

InkomenBestedingen16 <- InkomenBestedingen_16 %>%
  inner_join(koppel_16, by = "RINPERSOONHKW") %>%
  distinct(RINPERSOONHKW, .keep_all = TRUE)   

InkomenBestedingen15 <- InkomenBestedingen_15 %>%
  inner_join(koppel_15, by = "RINPERSOONHKW") %>%
  distinct(RINPERSOONHKW, .keep_all = TRUE)  

InkomenBestedingen14 <- InkomenBestedingen_14 %>%
  inner_join(koppel_14, by = "RINPERSOONHKW") %>%
  distinct(RINPERSOONHKW, .keep_all = TRUE)  

InkomenBestedingen13 <- InkomenBestedingen_13 %>%
  inner_join(koppel_13, by = "RINPERSOONHKW") %>%
  distinct(RINPERSOONHKW, .keep_all = TRUE)  

InkomenBestedingen12 <- InkomenBestedingen_12 %>%
  inner_join(koppel_12, by = "RINPERSOONHKW") %>%
  distinct(RINPERSOONHKW, .keep_all = TRUE) 

InkomenBestedingen11 <- InkomenBestedingen_11 %>%
  inner_join(koppel_11, by = "RINPERSOONHKW") %>%
  distinct(RINPERSOONHKW, .keep_all = TRUE)  

# Variables to impute
vars <- c("INHBESTINKH", "INHGESTINKH", "INHEHALGR", "INHAHL")

# Convert sentinel to NA
for(v in vars) {
  LBZ_2013[[v]] <- as.numeric(zap_labels(LBZ_2013[[v]])) #CHANGE DATES
  InkomenBestedingen11[[v]] <- as.numeric(zap_labels(InkomenBestedingen11[[v]])) #CHANGE DATES
  InkomenBestedingen12[[v]] <- as.numeric(zap_labels(InkomenBestedingen12[[v]])) #CHANGE DATES
  InkomenBestedingen14[[v]] <- as.numeric(zap_labels(InkomenBestedingen14[[v]])) #CHANGE DATES
  InkomenBestedingen15[[v]] <- as.numeric(zap_labels(InkomenBestedingen15[[v]])) #CHANGE DATES
  
  LBZ_2013[[v]][LBZ_2013[[v]]== 9999999999] <- NA #CHANGE DATES
  InkomenBestedingen11[[v]][InkomenBestedingen11[[v]]== 9999999999] <- NA #CHANGE DATES
  InkomenBestedingen12[[v]][InkomenBestedingen12[[v]]== 9999999999] <- NA #CHANGE DATES
  InkomenBestedingen14[[v]][InkomenBestedingen14[[v]]== 9999999999] <- NA #CHANGE DATES
  InkomenBestedingen15[[v]][InkomenBestedingen15[[v]]== 9999999999] <- NA #CHANGE DATES
}

# Merge lookback 
result <- LBZ_2013 %>% #CHANGE DATES
  left_join(InkomenBestedingen12 %>% select(RINPERSOON, all_of(vars), RINPERSOONHKW) %>% #CHANGE 
              rename_with(~paste0(.,"_12"), all_of(c(vars, "RINPERSOONHKW"))), #CHANGE 
            by = "RINPERSOON") %>%
  left_join(InkomenBestedingen11 %>% select(RINPERSOON, all_of(vars), RINPERSOONHKW) %>% #CHANGE 
              rename_with(~paste0(.,"_11"), all_of(c(vars, "RINPERSOONHKW"))), #CHANGE 
            by = "RINPERSOON") %>%
  left_join(InkomenBestedingen14 %>% select(RINPERSOON, all_of(vars), RINPERSOONHKW) %>% #CHANGE 
              rename_with(~paste0(.,"_14"), all_of(c(vars, "RINPERSOONHKW"))), #CHANGE 
            by = "RINPERSOON") %>%
  left_join(InkomenBestedingen15 %>% select(RINPERSOON, all_of(vars), RINPERSOONHKW) %>% #CHANGE 
              rename_with(~paste0(.,"_15"), all_of(c(vars, "RINPERSOONHKW"))), #CHANGE 
            by = "RINPERSOON")

for(v in vars) {
  result[[v]] <- coalesce(result[[v]], result[[paste0(v,"_12")]], result[[paste0(v, "_11")]], #CHANGE 
                          result[[paste0(v, "_14")]], result[[paste0(v, "_15")]]) #CHANGE 
}
result$RINPERSOONHKW <- coalesce(result$RINPERSOONHKW, 
                                 result$RINPERSOONHKW_12, result$RINPERSOONHKW_11, #CHANGE 
                                 result$RINPERSOONHKW_14, result$RINPERSOONHKW_15) #CHANGE 

# Drop temp columns
result <- result %>%
  select(-ends_with("_12"),-ends_with("_11"), -ends_with("_14"), -ends_with("_15")) #CHANGE 

# Quick check 
cat("Missing before/after:\n")
sapply(vars, function(v) c(before = sum(is.na(LBZ_2013[[v]])), after = sum(is.na(result[[v]])))) #CHANGE 

# Save (repeat)
output_path <- "LBZPC_2013_imputed_n.gpkg" #CHANGE e.g. LBZPC_2013_imputed_n.gpkg, LBZPC_2014_imputed_n.gpkg etc..
st_write(result, output_path, delete_dsn = TRUE)

##### Geo-coding NB_types ##### 
LBZ_2019 <- st_read("LBZPC_2019_imputed_n.gpkg")
LBZ_2018 <- st_read("LBZPC_2018_imputed_n.gpkg")
LBZ_2017 <- st_read("LBZPC_2017_imputed_n.gpkg")
LBZ_2016 <- st_read("LBZPC_2016_imputed_n.gpkg")
LBZ_2015 <- st_read("LBZPC_2015_imputed_n.gpkg")
LBZ_2014 <- st_read("LBZPC_2014_imputed_n.gpkg")
LBZ_2013 <- st_read("LBZPC_2013_imputed_n.gpkg")

# Neighborhood types
nb_types <- st_read("neighbourhood_types.gpkg") 

nb_types_filter <- nb_types%>%
  select(nbtype_2016) %>%
  rename(
    nbtype = nbtype_2016 #change nbtype_2016
  ) %>%
  drop_na()

nb_types_filter <- nb_types_filter %>%  ###CHANGE 2016/2019
  select(nbtype) %>% 
  mutate(
    nbtype = case_when(
      nbtype == "2" ~ "compact mid-rise",
      nbtype == "9" ~ "compact high-rise",
      nbtype == "3" |  nbtype == "4" ~ "open mid-high",
      nbtype == "5"  ~ "compact low",
      nbtype == "6"   ~ "open low 1",
      nbtype == "8"  ~ "open low 2",
      nbtype == "1" |  nbtype == "7" ~ "sparsely built",
      TRUE ~ nbtype
    )) %>%
  drop_na()

LBZ_NB <- st_intersection(LBZ_2013, nb_types_filter) ###CHANGE LBZ_2014, LBZ_2015 etc..

LBZ_NB <- LBZ_NB %>%
  distinct(RINPERSOON, .keep_all = TRUE)

LBZ_NB_missing <- LBZ_2013 %>% ###CHANGE LBZ_2014, LBZ_2015 etc..
  filter(!(RINPERSOON %in% LBZ_NB$RINPERSOON))
  
LBZ_NB_missing_matched <- st_join(
  LBZ_NB_missing,
  nb_types_filter, 
  join = st_nearest_feature
)

LBZ_nbtype <- rbind(LBZ_NB, LBZ_NB_missing_matched)

# Save (repeat)
output_path <- "" ###CHANGE DATES
st_write(LBZ_nbtype, output_path, delete_dsn = TRUE)

##### linking KNMI data ##### 
LBZ_2019 <- st_read("LBZ_nbtype_2019.gpkg")
LBZ_2018 <- st_read("LBZ_nbtype_2018.gpkg")
LBZ_2017 <- st_read("LBZ_nbtype_2017.gpkg")
LBZ_2016 <- st_read("LBZ_nbtype_2016.gpkg")
LBZ_2015 <- st_read("LBZ_nbtype_2015.gpkg")
LBZ_2014 <- st_read("LBZ_nbtype_2014.gpkg")
LBZ_2013 <- st_read("LBZ_nbtype_2013.gpkg")

# KNMI and RIVM datasets
knmi <- read_csv("KNMI_rural_met_2013_2020.csv") 
knmi_new <- read_csv("KNMI_rural2013_2020_new.csv") 

rivm <- read_csv("daily_avg_wide_revised.csv") 

knm_4_cities <- knmi %>%
  filter(STN %in% c(240, 260, 330, 344)) %>%
  mutate(
    statnaam = case_when(
      STN == 240 ~ "Utrecht",
      STN == 260 ~ "Amsterdam",
      STN == 330 ~ "Rotterdam",
      STN == 344 ~ "'s-Gravenhage",
    )
  ) %>%
  select(Date, statnaam, avg_T, max_T, total_SQ,
         avg_humidity, avg_wind_speed, rural ) %>%
  arrange(statnaam, Date) %>%
  inner_join(knmi_new, by = c("Date", "statnaam"))

rivm <- rivm %>%
  rename(Date = date,
         statnaam = city) %>%
  mutate(
    statnaam = case_when(
      statnaam == "Den Haag" ~ "'s-Gravenhage",
      TRUE ~ statnaam)) %>%
  arrange(statnaam, Date)

# obtained from STATLINE CBS - bottom 20 percentile household income and top 30 percentile  household income
income_perc_20_2013 = 15600 
income_perc_20_2014 = 16000 
income_perc_20_2015 = 16300 
income_perc_20_2016 = 16300 
income_perc_20_2017 = 17200 
income_perc_20_2018 = 17600 
income_perc_20_2019 = 18400 

income_perc_80_2013 = 29300 
income_perc_80_2014 = 30300 
income_perc_80_2015 = 30800 
income_perc_80_2016 = 32000 
income_perc_80_2017 = 32700 
income_perc_80_2018 = 33100 
income_perc_80_2019 = 34800 

icd10 <- "^(J[0-9][0-9]|J[0-9][0-9][0-9]|J[0-9][0-9][0-9][0-9]$)"

years <- 2013:2019

for(yr in years) {
  df <- get(paste0("LBZ_", yr))
  df <- df %>%
    filter(str_detect(LBZIcd10hoofddiagnose, icd10)) %>%
    filter(LBZUrgentie =="Acute opname") %>%
    filter(!is.na(INHGESTINKH)) %>%
    filter(LBZHerkomst != "Overige instellingen") %>%
    mutate(
          GBAGEBOORTEJAAR = as.numeric(GBAGEBOORTEJAAR)) 
    assign(paste0("LBZ_", yr), df)
}

admission <- rbind(LBZ_2019, LBZ_2018, LBZ_2017, LBZ_2016, LBZ_2015, LBZ_2014, LBZ_2013)

## CHANGE Dates
all_dates <- seq(
  from = as.Date("2019-05-01"), #CHANGE
  to   = as.Date("2019-09-30"), #CHANGE
  by = "day" )

DHD_final2019 <- LBZ_2019 %>% #CHANGE
  rename(
    Date = LBZOpnamedatum 
   ) %>%
   mutate(
        income = as.factor(case_when(
        INHGESTINKH <= income_perc_20_2019 ~ "low SES", #CHANGE
        INHGESTINKH >  income_perc_20_2019 & INHGESTINKH < income_perc_80_2019 ~ "mid SES", #CHANGE
        INHGESTINKH >= income_perc_80_2019 ~ "high SES")), #CHANGE
     age = as.factor(case_when(
      GBAGEBOORTEJAAR < (year(Date) - 65) ~ "age_65+",
      GBAGEBOORTEJAAR >= (year(Date) - 5) ~ "age_0-5",
      GBAGEBOORTEJAAR <= (year(Date) - 6) & GBAGEBOORTEJAAR >= (year(Date) - 35) ~ "age_6-35",
      TRUE  ~ "age_36-65")),
      nbtype = case_when(
      nbtype == "compact high-rise" ~ "compact mid-rise",
        TRUE ~ nbtype),
     GBAGESLACHT = factor(GBAGESLACHT)
   ) %>%
  group_by(Date, statnaam, nbtype, income, age, GBAGESLACHT) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(
    Date = as.Date(all_dates),
    statnaam,
    nbtype,
    income,
    age,
    GBAGESLACHT,
    fill = list(count = 0)
  ) %>%
  arrange(statnaam, Date) %>%
  left_join(knm_4_cities, by = c("statnaam", "Date")) 

DHD_final <- rbind(DHD_final2019, DHD_final2018, DHD_final2017, DHD_final2016, 
                   DHD_final2015, DHD_final2014, DHD_final2013)

DHD_final_rivm <- DHD_final %>%
  left_join(rivm, by = c("statnaam", "Date")) 

# Before running the code below, run adjusting_for_population.R 
# through line 171 to incorporate the population offset. 
# Note that adjusting_for_population.R may take some time to run.

DHD_final_all <- DHD_final_rivm %>%
  arrange(Date) %>%
  mutate(
    year = year(Date),
    month = format(Date, "%m"),
    day_of_season = as.numeric(Date - as.Date(paste0(year, "-05-01"))) + 1,
    dow = factor(weekdays(Date)),
        stratum_model1 =  interaction(
         dow, month, year, statnaam,
           drop = TRUE)
) %>%
  left_join(
    subgroup_population_collapse, ## here add the population offset
    by = c("nbtype", "statnaam", "year", "income", "age", "GBAGESLACHT")
  )

####### Diagnostic ####### 
# DHD_final_all_VIF <-  fastDummies::dummy_cols(DHD_final_all, select_columns = "statnaam", remove_selected_columns =  FALSE)
# 
# model <- glm(
#   count ~  nbtype + income + GBAGESLACHT + max_T + avg_wind_speed + total_Q + total_RH + avg_humidity +  PM10 +  NO2 + O3 + dow + month + year +
#     statnaam,
# 
#   family =  quasipoisson(link ="log"),
#   subset = age == "age_0-5",
#   data = DHD_final_all_VIF
# )
# summary(model)
# vif(model)

####### Descriptive Statistics ####### 
cor.test(DHD_final_all$PM25, DHD_final_all$PM25)
DHD_final_all_filtered <- DHD_final_all %>%
  filter(statnaam == "Rotterdam")

quantile(DHD_final_all_filtered$O3_mda8, probs = c(0.95, 0.97, 0.99), na.rm = TRUE)
quantile(DHD_final_all_filtered$total_Q, probs = c(0.95, 0.97, 0.99), na.rm = TRUE)
quantile(DHD_final_all_filtered$max_T, probs = c(0.95, 0.97, 0.99), na.rm = TRUE)

quantile(DHD_final_all_filtered$avg_wind_speed, probs = c(0.95, 0.97, 0.99), na.rm = TRUE)
quantile(DHD_final_all_filtered$total_RH, probs = c(0.95, 0.97, 0.99), na.rm = TRUE)
quantile(DHD_final_all_filtered$avg_humidity, probs = c(0.95, 0.97, 0.99), na.rm = TRUE)
quantile(DHD_final_all_filtered$PM25, probs = c(0.95, 0.97, 0.99), na.rm = TRUE)
quantile(DHD_final_all_filtered$NO2, probs = c(0.95, 0.97, 0.99), na.rm = TRUE)

xtabs(count ~ nbtype + income, data = DHD_final_all)
xtabs(count ~ nbtype + age, data = DHD_final_all)
xtabs(count ~ statnaam, data = DHD_final_all)

select_DHD <- DHD_final_all %>%
  select(statnaam, O3_mda8, max_T, total_Q, avg_wind_speed, total_RH , avg_humidity,  PM25, NO2 )

summary_table <- select_DHD %>%
  group_by(statnaam) %>%
  summarise(
    across(everything(),
           list(
             mean = mean,
             sd = sd),
           na.rm = TRUE
    ))

####### Model Specifications ####### 
DHD_final_all$O3_daily <-DHD_final_all$O3
DHD_final_all$O3 <-DHD_final_all$O3_mda8
DHD_final_all$total_Q <-(DHD_final_all$total_Q)/1000 

  
DHD_final_all$income <- relevel(factor(DHD_final_all$income), ref = "high SES") 
DHD_final_all$nbtype <- as.factor(DHD_final_all$nbtype)
DHD_final_all$nbtype <- relevel(factor(DHD_final_all$nbtype), ref = "compact mid-rise")  #
DHD_final_all$age <- relevel(factor(DHD_final_all$age), ref = "age_0-5") #

lcz_order <- c(
  "compact mid-rise",
  "compact low",
  "open mid-high",
  "open low 2",
  "open low 1",
  "sparsely built"
)

DHD_final_all$nbtype <- factor(DHD_final_all$nbtype, levels = lcz_order)

DHD_final_all$ind_model1 <- with(DHD_final_all,
                                 tapply(count, stratum_model1, sum)[stratum_model1])

####### Model ####### 

# FOR SES, control for age
model1_int <- gnm(
  count ~    nbtype*O3 + nbtype*total_Q + 
  age + GBAGESLACHT + avg_wind_speed + avg_humidity + total_RH + PM25 + NO2 + offset(log(pop_offset)), 
  eliminate = stratum_model1,
  subset = income %in% c("low SES") & ind_model1 > 0, #CHANGE to mid SES, high SES
  family = quasipoisson(link = "log"),
  data = DHD_final_all,
  verbose = FALSE)

# FOR AGE GROUPS, control for income
# model1_int <- gnm(
#   count ~    nbtype*O3 + nbtype*total_Q + 
#     income + GBAGESLACHT + avg_wind_speed + avg_humidity + total_RH + PM25 + NO2 + offset(log(pop_offset)), 
#   eliminate = stratum_model1,
#   subset = age %in% c("age_0-5") & ind_model1 > 0, #CHANGE  age_6-35, age_36-65, age_65+
#   family = quasipoisson(link = "log"),
#   data = DHD_final_all,
#   verbose = FALSE)

summary(model1_int)

# Quasi AIC
# dev <- deviance(model1_int)
# chat <- summary(model1_int)$dispersion
# k <- length(coef(model1_int))
# QAIC <- dev /(chat + 2*k)
# QAIC
# summary(model1_int)$dispersion

# extract_polr_results <- function(model){
#   coef_table_full <- coef(summary(model))
#   
#   is_intercept <- grepl("\\|", rownames(coef_table_full))
#   
#   coef_table <- coef_table_full[!is_intercept, , drop =FALSE]
#   
#   p_values <- pnorm(abs(coef_table[,"t value"]), lower.tail = FALSE)*2
#   ci_all <- confint(model, level = 0.95)
#   
#   predictor_names <- rownames(coef_table)
#   ci <- ci_all[predictor_names, , drop = FALSE]
#   
#   # Results from Model 1
#   results <- data.frame(
#     Variable = predictor_names,
#     Log_Odds = coef_table[, "Value"],
#     Std_Error = coef_table[, "Std. Error"],
#     t_value =  coef_table[, "t value"],
#     p_value =  p_values,
#     CI_lower_log = ci[,1],
#     CI_upper_log = ci[,2],
#     Odds_Ratio = exp(coef_table[, "Value"]),
#     OR_CI_lower = exp(ci[,1]),
#     OR_CI_upper = exp(ci[,2]),
#     stringsAsFactors = FALSE
#   )
#   return(results)
#   
# }
# 
# results <- extract_polr_results(model1_int)
# # coef_table <- tidy(model1_int)
# output_path <- ""
# write.csv(results, output_path)

#cat(names(coef(model1_int)), sep = "\n")

#### JOINT HEAT-OZONE RR estimates ####
# replace total_Q for max_T

coefs <- coef(model1_int)
coefs[is.na(coefs)] <- 0
V <- vcov(model1_int)
V[is.na(V)] <- 0 

cen_temp <- mean(DHD_final_all$total_Q, na.rm = TRUE)  #CHANGE
cen_o3 <- mean(DHD_final_all$O3, na.rm = TRUE) 

percs_temp <- quantile(DHD_final_all$total_Q,  #CHANGE
                  probs = seq(0.75, 1.00, by = 0.01), na.rm = TRUE)
percs_o3 <- quantile(DHD_final_all$O3,
                       probs = seq(0.75, 1.00, by = 0.01), na.rm = TRUE)

nbtype_levels <- levels(DHD_final_all$nbtype)
ref_nbtype <- nbtype_levels[1]


get_RR <- function(exposure_type, nb, delta, coefs, V){
  contrast <- rep(0, length(coefs))
  names(contrast) <- names(coefs)
  
  contrast[exposure_type] <- delta
  int_term <- paste0("nbtype", nb, ":", exposure_type)
  int_term_alt <- paste0(exposure_type, ":nbtype", nb)
  
  if(int_term %in% names(coefs)) {
    contrast[int_term] <- delta
  } else if (int_term_alt %in% names(coefs)) {
    contrast[int_term_alt] <- delta
  } 
  
  lp <- sum(contrast*coefs)
  se_lp <- sqrt(as.numeric(t(contrast) %*% V %*% contrast))
  
  RR <- exp(lp)
  CI_lower <- exp(lp - 1.96 * se_lp)
  CI_upper <- exp(lp+1.96 * se_lp)
  
  return(c(RR = RR,CI_lower = CI_lower, CI_upper = CI_upper, 
           lp = lp, se_lp = se_lp))
  
}

get_compound_RR <- function(nb, delta_o3, delta_temp, coefs, V) {
  
  contrast <- rep(0, length(coefs))
  names(contrast) <- names(coefs)
  
  contrast["O3"] <- delta_o3
  
  int_o3 <-  paste0("nbtype", nb, ":O3")
  int_o3_alt <- paste0("O3:nbtype", nb)
  
  
  if(int_o3 %in% names(coefs)) {
    contrast[int_o3] <- delta_o3
  } else if (int_o3_alt %in% names(coefs)) {
    contrast[int_o3_alt] <- delta_o3
  } 
  
  contrast["total_Q"] <- delta_temp  #CHANGE
  
  int_t <-  paste0("nbtype", nb, ":total_Q")  #CHANGE
  int_t_alt <- paste0("total_Q:nbtype", nb)  #CHANGE
  
  if(int_t %in% names(coefs)) {
    contrast[int_t] <- delta_temp
  } else if (int_t_alt %in% names(coefs)) {
    contrast[int_t_alt] <- delta_temp
  } 
  
  lp <- sum(contrast*coefs)
  se_lp <- sqrt(as.numeric(t(contrast) %*% V %*% contrast))
  
  RR <- exp(lp)
  CI_lower <- exp(lp - 1.96 * se_lp)
  CI_upper <- exp(lp+1.96 * se_lp)
  
  return(c(RR = RR,CI_lower = CI_lower, CI_upper = CI_upper, 
           lp = lp, se_lp = se_lp))
}
  
target_pcs <- c(95, 97, 99)
results <- data.frame()

for (nb in nbtype_levels){
  for(pct in target_pcs ) {
    
    pct_label <- paste0(pct, "th")
    pct_prob <- pct / 100
    
    delta_temp <- as.numeric(quantile(DHD_final_all$total_Q, pct_prob, na.rm = TRUE)) - cen_temp #CHANGE
    delta_o3 <- as.numeric(quantile(DHD_final_all$O3, pct_prob, na.rm = TRUE)) - cen_o3
    
    res_o3 <- get_RR("O3", nb, delta_o3, coefs, V)
    results <- rbind(results, data.frame(
      nbtype = nb, percentile = pct_label, exposure = "O3",
      RR = res_o3["RR"], CI_lower = res_o3["CI_lower"],  CI_upper = res_o3["CI_upper"], 
      stringsAsFactors = FALSE
    ))
    
    res_t <- get_RR("total_Q", nb, delta_temp, coefs, V) #CHANGE
    results <- rbind(results, data.frame(
      nbtype = nb, percentile = pct_label, exposure = "total_Q",  #CHANGE
      RR = res_t["RR"], CI_lower = res_t["CI_lower"],  CI_upper = res_t["CI_upper"], 
      stringsAsFactors = FALSE
    ))
    
    res_c <- get_compound_RR(nb, delta_o3, delta_temp, coefs, V)
    results <- rbind(results, data.frame(
      nbtype = nb, percentile = pct_label, exposure = "Compound",
      RR = res_c["RR"], CI_lower = res_c["CI_lower"],  CI_upper = res_c["CI_upper"], 
      stringsAsFactors = FALSE
    ))
  }
}

#rownames(results)

results$nbtype <- factor(results$nbtype, levels = nbtype_levels)
results$percentile <- factor(results$percentile, levels = c("95th", "97th", "99th"))
results$exposure <- factor(results$exposure, levels = c("O3", "total_Q", "Compound")) #CHANGE: total_Q for max_T

print(results, digits =3)

output_path <- ""
write.csv(results, output_path)

#results_lowSES <- results
#results_midSES <- results
#results_highSES <- results

### 
#nbtype_levels <- as.numeric(as.factor(nbtype_levels))
results <- results %>%
  mutate(
    nb_num = as.numeric(factor(nbtype, levels = nbtype_levels)),
    pct_offset = case_when(
      percentile == "95th" ~ -0.3,
      percentile == "97th" ~  0.0,
      percentile == "99th" ~  0.3,
    ),
    exp_offset = case_when(
      exposure == "total_Q" ~  -0.08,
      exposure == "O3" ~   0.0,
      exposure == "Compound" ~ 0.08,
  ),
  x_pos = nb_num + pct_offset + exp_offset )

p <- ggplot(results, aes(x = x_pos, y = RR, color = exposure, shape = exposure)) +
geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.8, color = "grey40") +
geom_pointrange(
  aes(ymin = CI_lower, ymax = CI_upper),
  size = 0.4, linewidth = 0.65
) +
  scale_x_continuous(
    breaks = 1:length(nbtype_levels),
    labels = nbtype_levels,
    limits = c(0.5, length(nbtype_levels) + 0.5)
  ) +
scale_color_manual(
  values = c("O3" =  "grey50", "total_Q" = "black" , Compound = "firebrick"),
  labels = c("O3" = expression(O[3]), "total_Q" = "total_Q", "Compound" = "Compound") 
) +
  scale_shape_manual(
    values = c("O3" = 15, "total_Q" = 16, "Compound" = 17),
    labels = c("O3" = expression(O[3]), "total_Q" = "total_Q", "Compound" = "Compound") 
  ) +
  labs(x = NULL, y = "RR (95% CI)", 
       color = "Exposure", shape = "Exposure", title = "low SES") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size =9)
  ) 

#print(p)

pct_labels <- results %>%
  distinct(nbtype, nb_num, percentile, pct_offset) %>%
  mutate(x_pos = nb_num + pct_offset)

p + annotate("text", x = pct_labels$x_pos, 
             y = max(results$CI_upper, na.rm = TRUE) * 1.05,
             label = pct_labels$percentile, size = 2.5, angle = 90)

#ggsave("O3_age_65+.png", p, width = 8, height = 9, dpi =300 )

#cat(names(coef(model1_int)), sep = "\n")

####### Diagnostic ####### 
# model1_int <- gnm(
#   count ~   nbtype*O3  + nbtype*total_SQ + max_T + age + GBAGESLACHT  +
#     avg_wind_speed + avg_humidity + PM10 + NO2  + offset(log(pop_offset)), 
#   eliminate = stratum_model4,
#   subset = income == "low SES" & ind_model1 > 0,
#   family = quasipoisson(link = "log"),
#   #family = poisson(),
#   data = DHD_final_all,
#   verbose = FALSE)
# 
# # Quasi AIC
# dev <- deviance(model1_int)
# chat <- summary(model1_int)$dispersion
# k <- length(coef(model1_int))
# QAIC <- dev /(chat + 2*k)
# QAIC # 952.8109  -- 801.6106

#### F-test - Baseline differences ####
DHD_final_all$income <- relevel(factor(DHD_final_all$income), ref = "high SES") 
DHD_final_all$nbtype <- as.factor(DHD_final_all$nbtype)
DHD_final_all$nbtype <- relevel(factor(DHD_final_all$nbtype), ref = "compact mid-rise")  #
DHD_final_all$age <- relevel(factor(DHD_final_all$age), ref = "age_65+")

model1_null <- gnm(
  count ~  max_T*nbtype + O3*nbtype + age + GBAGESLACHT +
    avg_wind_speed + avg_humidity + total_RH + PM25 + NO2 + offset(log(pop_offset)),
  eliminate = stratum_model4,
  subset = income %in% c("low SES") & ind_model1 > 0,
  family = quasipoisson(link = "log"),
  data = DHD_final_all,
  verbose = FALSE)

model1_int <- gnm(
  count ~  max_T*O3*nbtype + age + GBAGESLACHT +
    avg_wind_speed + avg_humidity + total_RH + PM25 + NO2 + offset(log(pop_offset)), 
  eliminate = stratum_model4,
  subset = income %in% c("low SES") & ind_model1 > 0,
  family = quasipoisson(link = "log"),
  data = DHD_final_all,
  verbose = FALSE)

ftest_m1 <- anova(model1_null, model1_int, test ="F")
print(ftest_m1)
cat("F statistic:", round(ftest_m1[2, "F"], 3), "\n")
cat("p-vale:", round(ftest_m1[2, "Pr(>F)"], 4), "\n")
if (ftest_m1[2, "Pr(>F)"] < 0.05) {
  cat("Significant\n")
} else {
  cat("Not Significant\n")
}

#alter when swapping btw groups
grep("age", names(coefs), value= TRUE)

income_main <- "ageage_6-35"

interactions <- c(
  "compact mid-rise" = NA,
  "compact low" = "nbtypecompact low:ageage_6-35",
  "open mid-high" = "nbtypeopen mid-high:ageage_6-35",
  "open low 2" = "nbtypeopen low 2:ageage_6-35",
  "open low 1" = "nbtypeopen low 1:ageage_6-35",
  "sparsely built" = "nbtypesparsely built:ageage_6-35"
)

group_table <- lapply(names(interactions), function(lcz) {
 cv <- rep(0, length(coefs))
 names(cv) <- names(coefs)
 cv[income_main] <- 1
 
 if(!is.na(interactions[lcz])) {
   cv[interactions[lcz]] <- 1
 }
 
 beta <- sum(cv * coefs)
 se <- as.numeric(sqrt(t(cv) %*% V %*% cv))
 
 data.frame(
   lcz = lcz,
   beta = beta,
   se = se,
   RR = exp(beta),
   CI_lower = exp(beta - 1.96 * se),
   CI_upper = exp(beta + 1.96 * se)
 )
}) %>% do.call(rbind, .)

rownames(group_table) <- NULL
group_table

output_path <- ""
write.csv(group_table, output_path)

####### Synergy ####### 
delta_Q <- quantile(DHD_final_all$total_Q, 0.95) - mean(DHD_final_all$total_Q)
delta_O3 <- quantile(DHD_final_all$O3, 0.95, na.rm = TRUE) - mean(DHD_final_all$O3,  na.rm = TRUE)

lcz_types <- levels(factor(DHD_final_all$nbtype))
synergy_results <- lapply(lcz_types, function(ref_lcz) {
  
  dat <- DHD_final_all %>%
    mutate(nbtype = relevel(factor(nbtype), ref =  ref_lcz))
  
  mod <- gnm(
    count ~    nbtype*O3*total_Q + age + GBAGESLACHT +
      avg_wind_speed + avg_humidity + total_RH + PM25 + NO2 + offset(log(pop_offset)), 
    eliminate = stratum_model4,
    subset = income %in% c("low SES") & ind_model1 > 0,
    family = quasipoisson(link = "log"),
    data = dat, verbose = FALSE)
  
  s <- summary(mod)$coefficients
  
  # Main effects 
  beta_O3 <- s["O3", "Estimate"]
  beta_Q <- s["total_Q", "Estimate"]
  beta_syn <- s["O3:total_Q", "Estimate"]
  
  # Compund at 99th percentile
  lp <- beta_O3 * delta_O3 + beta_Q + delta_Q + beta_syn * delta_O3 * delta_Q
  
  # SE via vcov 
  cv <- rep(0, length(coef(mod)))
  names(cv) <- names(coef(mod))
  cv_keep <- rownames(vcov(mod))
  cv <- cv[cv_keep]
  cv["O3"] <- delta_O3
  cv["total_Q"] <- delta_Q
  syn_name <- grep("^O3:total_Q$|^total_Q:O3$", names(cv), value = TRUE)
  cv[syn_name] <- delta_O3 * delta_Q
  
  se <- sqrt(as.numeric(t(cv) %*% vcov(mod) %*% cv))
  
  data.frame(
    nbtype = ref_lcz,
    RR = exp(lp),
    CI_lower = exp(lp - 1.96 *se),
    CI_upper = exp(lp + 1.96 *se)
  )
  
}) %>% do.call(rbind, .)


synergy_results

####### Synergy ####### 
delta_Q <- quantile(DHD_final_all$total_Q, 0.9) - mean(DHD_final_all$total_Q)
delta_O3 <- quantile(DHD_final_all$O3, 0.9, na.rm = TRUE) - mean(DHD_final_all$O3,  na.rm = TRUE)

quantile(DHD_final_all$total_Q, 0.9)
quantile(DHD_final_all$O3, 0.9, na.rm = TRUE)


lcz_types <- levels(factor(DHD_final_all$nbtype))
synergy_results <- lapply(lcz_types, function(ref_lcz) {
  
  dat <- DHD_final_all %>%
    mutate(nbtype = relevel(factor(nbtype), ref =  ref_lcz))
  
  mod <- gnm(
    count ~    nbtype*O3*total_Q + age + GBAGESLACHT +
      avg_wind_speed + avg_humidity + total_RH + PM25 + NO2 + offset(log(pop_offset)), 
    eliminate = stratum_model4,
    subset = income %in% c("low SES") & ind_model1 > 0,
    family = quasipoisson(link = "log"),
    data = dat, verbose = FALSE)
  
  s <- summary(mod)$coefficients
  
  # Main effects 
  beta_O3 <- s["O3", "Estimate"]
  beta_Q <- s["total_Q", "Estimate"]
  beta_syn <- s["O3:total_Q", "Estimate"]
  
  # Compund at 99th percentile
  lp <- beta_O3 * delta_O3 + beta_Q + delta_Q + beta_syn * delta_O3 * delta_Q
  
  # SE via vcov 
  cv <- rep(0, length(coef(mod)))
  names(cv) <- names(coef(mod))
  cv_keep <- rownames(vcov(mod))
  cv <- cv[cv_keep]
  cv["O3"] <- delta_O3
  cv["total_Q"] <- delta_Q
  syn_name <- grep("^O3:total_Q$|^total_Q:O3$", names(cv), value = TRUE)
  cv[syn_name] <- delta_O3 * delta_Q
  
  se <- sqrt(as.numeric(t(cv) %*% vcov(mod) %*% cv))
  
  data.frame(
    nbtype = ref_lcz,
    RR = exp(lp),
    CI_lower = exp(lp - 1.96 *se),
    CI_upper = exp(lp + 1.96 *se)
  )
  
}) %>% do.call(rbind, .)


synergy_results

low_SES_syn_results_90 <- synergy_results %>% mutate(percentile = "90th")
mid_SES_syn_results_90 <- synergy_results %>% mutate(percentile = "90th")
high_SES_syn_results_90 <- synergy_results %>% mutate(percentile = "90th")

synergy_results_merged <- rbind( low_SES_syn_results_90, mid_SES_syn_results_90, high_SES_syn_results_90)

output_path <- ""
write.csv2(synergy_results_merged, output_path, row.names = FALSE)






