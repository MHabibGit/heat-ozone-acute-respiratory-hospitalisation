library(tidyverse)
library(gridExtra)

# ── Configuration ─────────────────────────────────────────────
INPUT_DIR  <- "/Users/mmoustafahabib/Library/CloudStorage/OneDrive-DelftUniversityofTechnology/TUDelft-PhD/11_Data Repository/Paper4/Github/output_datasets/"  # folder containing all input files listed below

knmi <- read_csv(file.path(INPUT_DIR, "knmi_rural.csv"))
rivm <- read_csv(file.path(INPUT_DIR, "daily_air_quality.csv"))

merged_df <- rivm %>%
  rename(Date = date, statnaam = city) %>%
  mutate(
    statnaam = case_when(
      statnaam == "Den Haag" ~ "'s-Gravenhage",
      TRUE ~ statnaam
    )
  ) %>%
  inner_join(knmi, by = c("Date", "statnaam"))

theme_pub_1 <- theme(
  # Panel border (box around plot)
  panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
  panel.background = element_blank(),
  
  # Remove grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  # Tick marks
  axis.ticks = element_line(color = "black", linewidth = 0.4),
  axis.ticks.length = unit(0.15, "cm"),
  
  # Axis text
  axis.text = element_text(color = "black", size = 10),
  axis.title = element_text(size = 11),
  
  # Color-coded y-axis titles
  axis.title.y.left = element_text(color = "darkred", size = 11),
  axis.title.y.right = element_text(color = "darkblue", size = 11),
  axis.text.y.left = element_text(color = "darkred"),
  axis.text.y.right = element_text(color = "darkblue"),
  
  # Title
  plot.title = element_text(size = 13, face = "bold"),
  plot.subtitle = element_text(size = 10, color = "grey30"),
  
  # Legend
  legend.position = "right",
  legend.background = element_blank(),
  legend.key = element_blank()
)

theme_pub_2 <- theme(
  # Panel border (box around plot)
  panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
  panel.background = element_blank(),
  
  # Remove grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  # Tick marks
  axis.ticks = element_line(color = "black", linewidth = 0.4),
  axis.ticks.length = unit(0.15, "cm"),
  
  # Axis text
  axis.text = element_text(color = "black", size = 10),
  axis.title = element_text(size = 11),
  
  # Color-coded y-axis titles
  axis.title.y.left = element_text(color = "#A85820", size = 11),
  axis.title.y.right = element_text(color = "darkblue", size = 11),
  axis.text.y.left = element_text(color = "#A85820"),
  axis.text.y.right = element_text(color = "darkblue"),
  
  # Title
  plot.title = element_text(size = 13, face = "bold"),
  plot.subtitle = element_text(size = 10, color = "grey30"),
  
  # Legend
  legend.position = "right",
  legend.background = element_blank(),
  legend.key = element_blank()
)

### MERGED 
### Plot 1: T_max vs O3
df <- merged_df %>%
  mutate(Date = as.Date(Date),
         year = as.numeric(format(Date, "%Y")),
         month_day = as.Date(format(Date, "2000-%m-%d")))

scale_factor <- max(df$O3_mda8, na.rm = TRUE) / max(df$max_T, na.rm = TRUE)

avg <- df %>%
  group_by(month_day) %>%
  summarise(max_T = mean(max_T, na.rm = TRUE),
            O3 = mean(O3_mda8, na.rm = TRUE), .groups = "drop")

df <- df %>%
  mutate(stagnation_index = scale(-avg_wind_speed) + 
           scale(-avg_humidity) + scale(-total_RH))

stag_extreme <- df %>%
  filter(stagnation_index > quantile(stagnation_index, 0.95, na.rm = TRUE)) 

cor.test(df$stagnation_index, df$max_T, method = "spearman")

p1 <- ggplot() +
  geom_line(data = df, aes(x = month_day, y = max_T, group = interaction(year, statnaam), alpha = year),
            color = "#C44E52", linewidth = 0.2) +
  geom_line(data = df, aes(x = month_day, y = O3_mda8 / scale_factor, group = interaction(year, statnaam), alpha = year),
            color = "#4C72B0", linewidth = 0.2) +
  geom_segment(data = stag_extreme,
               aes(x = month_day, xend = month_day,
                   y = -2, yend = -2 + (stagnation_index / max(df$stagnation_index, na.rm = TRUE)) * 2),
               color = "darkgrey", alpha = 0.4, linewidth = 0.5,
               inherit.aes = FALSE) +
  coord_cartesian(ylim = c(-2, 40), clip = "off") +
  geom_smooth(data = avg, aes(x = month_day, y = max_T),
              color = "darkred", linewidth = 1.5, method = "loess",
              span = 0.3, se = FALSE, linetype = "solid") +
  geom_smooth(data = avg, aes(x = month_day, y = O3 / scale_factor),
              color = "darkblue", linewidth = 1.5, method = "loess",
              span = 0.3, se = FALSE, linetype = "11") +
  scale_alpha_continuous(range = c(0.05, 0.3), breaks = 2013:2019,
                         guide = "none") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(
    name = expression(T[max] ~ "(°C)"),
    sec.axis = sec_axis(~ . * scale_factor, 
                        name = expression("MDA8" ~ O[3] ~ "(µg m"^-3*")"))) +
  labs(x = NULL) +
  theme_pub_1

### Plot 3: Q vs O3
df <- df %>%
  mutate(total_Q = total_Q / 100)

avg3 <- df %>%
  group_by(month_day) %>%
  summarise(O3 = mean(O3_mda8, na.rm = TRUE),
            total_Q = mean(total_Q, na.rm = TRUE), .groups = "drop")

scale_factor <- max(df$O3_mda8, na.rm = TRUE) / max(df$total_Q, na.rm = TRUE)

p2 <- ggplot() +
  geom_line(data = df, aes(x = month_day, y = total_Q, group = interaction(year, statnaam), alpha = year),
            color = "#DD8452", linewidth = 0.2) +
  geom_line(data = df, aes(x = month_day, y = O3_mda8 / scale_factor, group = interaction(year, statnaam), alpha = year),
            color = "#4C72B0", linewidth = 0.2) +
  geom_segment(data = stag_extreme,
               aes(x = month_day, xend = month_day,
                   y = -2, yend = -2 + (stagnation_index / max(df$stagnation_index, na.rm = TRUE)) * 2),
               color = "darkgrey", alpha = 0.4, linewidth = 0.5,
               inherit.aes = FALSE) +
  coord_cartesian(ylim = c(-2, 40), clip = "off") +
  geom_smooth(data = avg3, aes(x = month_day, y = total_Q),
              color = "#A85820", linewidth = 1.5, method = "loess",
              span = 0.3, se = FALSE, linetype = "solid") +
  geom_smooth(data = avg3, aes(x = month_day, y = O3 / scale_factor),
              color = "darkblue", linewidth = 1.5, method = "loess",
              span = 0.3, se = FALSE, linetype = "11") +
  scale_alpha_continuous(range = c(0.05, 0.3), breaks = 2013:2019,
                         guide = "none") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
 scale_y_continuous(
   name = expression("Q (MJ m"^-2*")"),
    sec.axis = sec_axis(~ . * scale_factor, 
                        name = expression("MDA8" ~ O[3] ~ "(µg m"^-3*")"))) +
  labs(x = NULL) +
  theme_pub_2

### Final plot
p1 <- p1 + ggtitle("")
p2 <- p2 + ggtitle("")
grid.arrange(p1, p2, ncol = 2)
