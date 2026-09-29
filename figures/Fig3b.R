library(openxlsx)
library(tidyverse)
library(grid)

path_1 <- "2_group_lq.xlsx"

df <- read.xlsx(path_1)
df$group_by <- as.factor(df$group_by)
df$LQ       <- as.numeric(df$LQ)

nbtype_levels <- c("compact mid-rise", "compact low", "open mid-high",
                   "open low 2", "open low 1", "sparsely built")

# ── figure 2 part a ─────────────────────────
subgroup_dist <- df %>%
  filter(group_by %in% c("low SES", "mid SES", "high SES"),
         period == "2013-2019") %>%
  group_by(group_by, nbtype) %>%
  summarise(pop = sum(n_raw, na.rm = TRUE), .groups = "drop_last") %>%  # pool the 4 cities
  mutate(pct = 100 * pop / sum(pop)) %>%                                # share within SES
  ungroup() %>%
  mutate(group_by = factor(group_by, levels = c("low SES", "mid SES", "high SES")),
         nbtype   = factor(nbtype,   levels = nbtype_levels)) %>%
  arrange(group_by, nbtype)

subgroup_dist %>%
  select(group_by, nbtype, pct) %>%
  pivot_wider(names_from = nbtype, values_from = pct) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))
# ── figure 2 part c  ─────────────────────────
lcz_colors <- c(
  "compact mid-rise" = "#6e016b",
  "compact low"      = "#88419d",
  "open mid-high"    = "#8c6bb1",
  "open low 2"       = "#8c96c6",
  "open low 1"       = "#9ebcda",
  "sparsely built"   = "#c7e8ef"
)

ses_levels   <- c("low SES", "mid SES", "high SES")
ses_band_btt <- c("high SES", "mid SES", "low SES")                      # bottom -> top
lcz_btt      <- c("sparsely built", "open low 1", "open low 2",
                  "open mid-high", "compact low", "compact mid-rise")    # bottom -> top

# ── Parse the SES & age combinations ──────────────────────────────────
combo <- df %>%
  filter(str_detect(group_by, " & "), period %in% c(2013, 2019)) %>%
  separate(group_by, into = c("ses", "age"), sep = " & ", remove = FALSE) %>%
  mutate(ses = str_trim(ses), age = str_trim(age), period = as.integer(period)) %>%
  filter(ses %in% ses_levels, nbtype %in% lcz_btt)

# age order taken FROM THE DATA (robust to "age_6-35" vs "age_6_35"):
#   sort by the first number, oldest at the bottom of each cluster
age_present <- combo %>% distinct(age) %>% pull(age)
age_rank    <- as.integer(str_extract(age_present, "[0-9]+"))
age_btt     <- age_present[order(-age_rank)]                 # bottom -> top (old -> young)
age_short   <- setNames(sub("^age_", "", age_present), age_present)

# ── Master 72-row order (bottom -> top) ───────────────────────────────
y_levels <- character(0)
for (s in ses_band_btt)
  for (l in lcz_btt)
    for (a in age_btt)
      y_levels <- c(y_levels, paste(s, l, a, sep = "|"))

row_meta <- tibble(row_id = factor(y_levels, levels = y_levels)) %>%
  separate(row_id, into = c("ses", "lcz", "age"), sep = "\\|", remove = FALSE) %>%
  mutate(
    row_id = factor(row_id, levels = y_levels),              
    lcz    = factor(lcz, levels = names(lcz_colors)),
    lab    = age_short[age]
  )
y_labels_vec        <- row_meta$lab
names(y_labels_vec) <- as.character(row_meta$row_id)

# ── One arrow per cell ────────────────────────────────────────────────
arrow_df <- combo %>%
  mutate(row_id = paste(ses, as.character(nbtype), age, sep = "|")) %>%
  group_by(row_id, period) %>%
  summarise(LQ_mean = weighted.mean(LQ, w = n_raw), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = LQ_mean, names_prefix = "LQ_") %>%
  filter(!is.na(LQ_2013), !is.na(LQ_2019)) %>%
  mutate(
    delta     = LQ_2019 - LQ_2013,
    direction = case_when(delta >  0.02 ~ "increasing",
                          delta < -0.02 ~ "decreasing",
                          TRUE          ~ "stable"),
    row_id    = factor(row_id, levels = y_levels)            # SAME levels as backbone
  ) %>%
  filter(!is.na(row_id))

# quick sanity check in the console:
# arrow_df %>% count(direction); nrow(arrow_df)   # how many of the 72 cells are filled

# ── Geometry helpers ──────────────────────────────────────────────────
band_breaks    <- c(24.5, 48.5)
cluster_breaks <- setdiff(seq(4.5, 68.5, by = 4), band_breaks)
band_df        <- tibble(label = c("high SES", "mid SES", "low SES"),
                         y_mid = c(12.5, 36.5, 60.5))

pal_dir <- c("increasing" = "black",
             "decreasing" = "#c95f2a",
             "stable"     = "#888888")

# ── Plot ──────────────────────────────────────────────────────────────
p <- ggplot() +
  
  # SES band shading
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5,  ymax = 24.5, fill = "grey91") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 24.5, ymax = 48.5, fill = "grey96") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 48.5, ymax = 72.5, fill = "white") +
  
  # LCZ colour strip — re-anchored to the new left edge (0.6)
  geom_tile(data = row_meta,
            aes(x = 0.597, y = row_id, fill = lcz),
            width = 0.034, height = 1) +
  scale_fill_manual(values = lcz_colors, breaks = names(lcz_colors), name = "LCZ type") +
  
  # proportionality band + reference line
  annotate("rect", xmin = 0.95, xmax = 1.05, ymin = 0.5, ymax = 72.5,
           fill = "steelblue", alpha = 0.07) +
  geom_vline(xintercept = 1, colour = "grey35", linewidth = 0.35, linetype = "dashed") +
  
  # separators
  geom_hline(yintercept = cluster_breaks, colour = "grey88", linewidth = 0.25) +
  geom_hline(yintercept = band_breaks,    colour = "grey55", linewidth = 0.4) +
  
  # arrows
  geom_segment(
    data = arrow_df %>% filter(direction != "stable"),
    aes(x = LQ_2013, xend = LQ_2019, y = row_id, yend = row_id, colour = direction),
    linewidth = 0.9, lineend = "round",
    arrow = arrow(length = unit(0.11, "cm"), type = "closed", ends = "last")
  ) +
  geom_point(
    data = arrow_df %>% filter(direction != "stable"),
    aes(x = LQ_2013, y = row_id, colour = direction), size = 1.2, shape = 16
  ) +
  geom_segment(
    data = arrow_df %>% filter(direction == "stable"),
    aes(x = LQ_2019, xend = LQ_2019,
        y = as.numeric(row_id) - 0.3, yend = as.numeric(row_id) + 0.3, colour = direction),
    linewidth = 1.0
  ) +
  
  # SES labels — pushed out to the new right edge
  geom_text(data = band_df, aes(x = 1.66, y = y_mid, label = label),
            angle = 90, size = 3.0, colour = "grey25",
            fontface = "bold", hjust = 0.5, inherit.aes = FALSE) +
  
  scale_colour_manual(
    values = pal_dir,
    labels = c("decreasing" = "Decreasing", "increasing" = "Increasing", "stable" = "Stable"),
    name   = "Trend"
  ) +
  scale_x_continuous(
    name   = "Concentration Index  (1.0 = proportional)",
    breaks = seq(0.7, 1.6, 0.1),
    expand = c(0, 0)
  ) +
  scale_y_discrete(name = NULL, labels = y_labels_vec, expand = c(0, 0), drop = FALSE) +
  
  coord_cartesian(xlim = c(0.6, 1.7)) +   # <- view window; nothing gets deleted
  
  theme_minimal(base_family = "Helvetica", base_size = 10) +
  theme(
    panel.grid.major.x = element_line(colour = "grey93", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_rect(colour = "grey45", fill = NA, linewidth = 0.5),
    axis.ticks.y       = element_blank(),
    axis.text.y        = element_text(size = 6.5, colour = "grey15", hjust = 1,
                                      margin = margin(r = 3)),
    axis.text.x        = element_text(size = 8, colour = "grey15"),
    axis.title.x       = element_text(size = 9, margin = margin(t = 7)),
    legend.position    = "right",
    legend.title       = element_text(size = 9, face = "bold", colour = "grey15"),
    legend.text        = element_text(size = 8, colour = "grey15"),
    plot.margin        = margin(12, 5, 10, 0)
  ) +
  guides(
    colour = guide_legend(override.aes = list(linewidth = 1.2), nrow = 3, order = 1),
    fill   = guide_legend(order = 2)
  )

p

##
# Step A — build test_results (this was skipped)
cell_counts <- combo %>%
  group_by(ses, nbtype, age, period) %>%
  summarise(n_raw = sum(n_raw, na.rm = TRUE), .groups = "drop")

totals <- combo %>%
  group_by(ses, age, period) %>%
  summarise(total_n = sum(n_raw, na.rm = TRUE), .groups = "drop")

test_df <- cell_counts %>%
  left_join(totals, by = c("ses", "age", "period")) %>%
  mutate(row_id = paste(ses, nbtype, age, sep = "|")) %>%
  select(row_id, ses, lcz = nbtype, age, period, n_raw, total_n) %>%
  pivot_wider(names_from = period, values_from = c(n_raw, total_n))


# 1. Build test_results (two-proportion test + FDR correction)
test_results <- test_df %>%
  rowwise() %>%
  mutate(
    test = list(
      prop.test(
        x = c(n_raw_2013, n_raw_2019),
        n = c(total_n_2013, total_n_2019)
      )
    ),
    p_value = test$p.value
  ) %>%
  ungroup() %>%
  select(-test) %>%
  mutate(p_adj = p.adjust(p_value, method = "BH"))

# 2. Build the CORRECTED arrow_df using p_adj
arrow_df <- combo %>%
  mutate(row_id = paste(ses, as.character(nbtype), age, sep = "|")) %>%
  group_by(row_id, period) %>%
  summarise(LQ_mean = weighted.mean(LQ, w = n_raw), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = LQ_mean, names_prefix = "LQ_") %>%
  filter(!is.na(LQ_2013), !is.na(LQ_2019)) %>%
  mutate(delta = LQ_2019 - LQ_2013) %>%
  left_join(test_results %>% select(row_id, p_adj), by = "row_id") %>%
  mutate(
    direction = case_when(
      p_adj < 0.05 & delta > 0 ~ "increasing",
      p_adj < 0.05 & delta < 0 ~ "decreasing",
      TRUE ~ "stable"
    ),
    row_id = factor(row_id, levels = y_levels)
  ) %>%
  filter(!is.na(row_id))

# sanity check
sum(is.na(arrow_df$p_adj))
table(arrow_df$direction, useNA = "always")

# 3. ONLY NOW build the plot — arrow_df is correct at this point
p <- ggplot() +
  
  # SES band shading
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5,  ymax = 24.5, fill = "grey91") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 24.5, ymax = 48.5, fill = "grey96") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 48.5, ymax = 72.5, fill = "white") +
  
  # LCZ colour strip — re-anchored to the new left edge (0.6)
  geom_tile(data = row_meta,
            aes(x = 0.597, y = row_id, fill = lcz),
            width = 0.034, height = 1) +
  scale_fill_manual(values = lcz_colors, breaks = names(lcz_colors), name = "LCZ type") +
  
  # proportionality band + reference line
  annotate("rect", xmin = 0.95, xmax = 1.05, ymin = 0.5, ymax = 72.5,
           fill = "steelblue", alpha = 0.07) +
  geom_vline(xintercept = 1, colour = "grey35", linewidth = 0.35, linetype = "dashed") +
  
  # separators
  geom_hline(yintercept = cluster_breaks, colour = "grey88", linewidth = 0.25) +
  geom_hline(yintercept = band_breaks,    colour = "grey55", linewidth = 0.4) +
  
  # arrows
  geom_segment(
    data = arrow_df %>% filter(direction != "stable"),
    aes(x = LQ_2013, xend = LQ_2019, y = row_id, yend = row_id, colour = direction),
    linewidth = 0.9, lineend = "round",
    arrow = arrow(length = unit(0.11, "cm"), type = "closed", ends = "last")
  ) +
  geom_point(
    data = arrow_df %>% filter(direction != "stable"),
    aes(x = LQ_2013, y = row_id, colour = direction), size = 1.2, shape = 16
  ) +
  geom_segment(
    data = arrow_df %>% filter(direction == "stable"),
    aes(x = LQ_2019, xend = LQ_2019,
        y = as.numeric(row_id) - 0.3, yend = as.numeric(row_id) + 0.3, colour = direction),
    linewidth = 1.0
  ) +
  
  # SES labels — pushed out to the new right edge
  geom_text(data = band_df, aes(x = 1.66, y = y_mid, label = label),
            angle = 90, size = 3.0, colour = "grey25",
            fontface = "bold", hjust = 0.5, inherit.aes = FALSE) +
  
  scale_colour_manual(
    values = pal_dir,
    labels = c("decreasing" = "Decreasing", "increasing" = "Increasing", "stable" = "Stable"),
    name   = "Trend"
  ) +
  scale_x_continuous(
    name   = "Concentration Index  (1.0 = proportional)",
    breaks = seq(0.7, 1.6, 0.1),
    expand = c(0, 0)
  ) +
  scale_y_discrete(name = NULL, labels = y_labels_vec, expand = c(0, 0), drop = FALSE) +
  
  coord_cartesian(xlim = c(0.6, 1.7)) +   # <- view window; nothing gets deleted
  
  theme_minimal(base_family = "Helvetica", base_size = 10) +
  theme(
    panel.grid.major.x = element_line(colour = "grey93", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_rect(colour = "grey45", fill = NA, linewidth = 0.5),
    axis.ticks.y       = element_blank(),
    axis.text.y        = element_text(size = 6.5, colour = "grey15", hjust = 1,
                                      margin = margin(r = 3)),
    axis.text.x        = element_text(size = 8, colour = "grey15"),
    axis.title.x       = element_text(size = 9, margin = margin(t = 7)),
    legend.position    = "right",
    legend.title       = element_text(size = 9, face = "bold", colour = "grey15"),
    legend.text        = element_text(size = 8, colour = "grey15"),
    plot.margin        = margin(12, 5, 10, 0)
  ) +
  guides(
    colour = guide_legend(override.aes = list(linewidth = 1.2), nrow = 3, order = 1),
    fill   = guide_legend(order = 2)
  )
p
# ggsave("ses_age_lcz_simple.png", p, width = 9, height = 16, dpi = 300, limitsize = FALSE)

