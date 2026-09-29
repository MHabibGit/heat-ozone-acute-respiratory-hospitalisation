library(tidyverse)
library(patchwork)
library(cowplot)
library(openxlsx)

# ── 0. Load data ──────────────────────────────────────────────────────────────
path_1 <- "/Users/mmoustafahabib/Downloads/260603_0500_DHD_Heat_Ozone_2026_aangepast/DHD_Heat_Ozone_2026/3_forest_plots_all.xlsx"
df <- read.xlsx(path_1)

df <- df %>%
  mutate(nbtype = ifelse(nbtype == "compact mid-rise", "compact mid-high", nbtype)) %>%
  mutate(
    RR         = as.numeric(RR),
    CI_lower   = as.numeric(CI_lower),
    CI_upper   = as.numeric(CI_upper),
    model_type = as.character(model_type)
  )

# ── 1. LCZ order ──────────────────────────────────────────────────────────────
nbtype_levels <- c(
  "compact mid-high", "compact low", "open mid-high",
  "open low 2", "open low 1", "sparsely built"
)

# ── 2. Colours ────────────────────────────────────────────────────────────────
col_O3        <- "#2C4B7C"
col_met_tmax  <- "#8B2E33"
col_met_q     <- "#b56300"
col_Q_cmpd    <- "#bc934b"
col_Tmax_cmpd <- "#bc934b"

offset_O3       <- -0.12
offset_met      <-  0.00
offset_compound <-  0.12

shade_df <- tibble(nb_num = 1:6) %>%
  mutate(
    xmin = nb_num - 0.45,
    xmax = nb_num + 0.45,
    fill = rep(c("grey96", "white"), 3)
  )

# ── 3. Theme ──────────────────────────────────────────────────────────────────
theme_fp <- function(show_x = FALSE) {
  base <- theme_classic(base_size = 9) +
    theme(
      axis.line         = element_blank(),
      axis.ticks        = element_line(color = "grey50", linewidth = 0.3),
      axis.ticks.length = unit(0.10, "cm"),
      axis.text.y       = element_text(size = 7),
      axis.title.y      = element_text(size = 7.5, margin = margin(r = 3)),
      panel.grid        = element_blank(),
      panel.border      = element_rect(color = "grey70", fill = NA,
                                        linewidth = 0.4),
      panel.background  = element_rect(fill = "white", color = NA),
      strip.background  = element_blank(),
      strip.text.x      = element_blank(),
      strip.text.y      = element_blank(),
      legend.position   = "none",
      plot.margin       = margin(1, 2, 1, 2)
    )
  if (show_x) {
    base + theme(axis.text.x = element_text(size = 7.5, color = "grey20"))
  } else {
    base + theme(axis.text.x = element_blank(),
                 axis.ticks.x = element_blank())
  }
}

# ── 4. Row specs — ylim and breaks per row ────────────────────────────────────
row_specs <- list(
  "Low SES"   = list(ylim = c(0.45, 1.75),
                     brk  = c(0.50, 0.75, 1.00, 1.25, 1.50, 1.75)),
  "Mid SES"   = list(ylim = c(0.50, 1.50),
                     brk  = c(0.50, 0.75, 1.00, 1.25, 1.50)),
  "High SES"  = list(ylim = c(0.42, 2.20),
                     brk  = c(0.50, 0.75, 1.00, 1.25, 1.50, 1.75, 2.00)),
  "Age 0-5"   = list(ylim = c(0.18, 2.85),
                     brk  = c(0.20, 0.25, 0.50, 0.75, 1.00, 1.25, 1.50, 2.00, 2.75)),
  "Age 6-35"  = list(ylim = c(0.18, 2.50),
                     brk  = c(0.20, 0.25, 0.50, 0.75, 1.00, 1.25, 1.50, 2.00)),
  "Age 36-65" = list(ylim = c(0.42, 2.20),
                     brk  = c(0.50, 0.75, 1.00, 1.25, 1.50, 1.75, 2.00)),
  "Age 65+"   = list(ylim = c(0.42, 2.00),
                     brk  = c(0.50, 0.75, 1.00, 1.25, 1.50, 1.75))
)

# ── 5. Prepare data for all models ───────────────────────────────────────────
all_models <- c(
  "low_SES_total_Q",  "low_SES_max_T",
  "mid_SES_total_Q",  "mid_SES_max_T",
  "high_SES_total_Q", "high_SES_max_T",
  "Age_0-5_total_Q",  "age_0-5_max_T",
  "Age_6-35_total_Q", "age_6-35_max_T",
  "Age_36-65_total_Q","Age_36-65_max_T",
  "Age_65+_total_Q",  "Age_65+_max_T"
)

df_all <- df %>%
  filter(model_type %in% all_models, percentile == "99th") %>%
  mutate(
    nbtype    = factor(nbtype, levels = nbtype_levels),
    row_label = case_when(
      str_detect(model_type, "low_SES")  ~ "Low income",
      str_detect(model_type, "mid_SES")  ~ "Mid income",
      str_detect(model_type, "high_SES") ~ "High income",
      str_detect(model_type, "0-5")      ~ "Age 0-5",
      str_detect(model_type, "6-35")     ~ "Age 6-35",
      str_detect(model_type, "36-65")    ~ "Age 36-65",
      str_detect(model_type, "65\\+")    ~ "Age 65+"
    ),
    met_model = case_when(
      str_detect(model_type, "total_Q") ~ "Q model",
      str_detect(model_type, "max_T")   ~ "T-max model"
    ),
    met_model = factor(met_model, levels = c("T-max model", "Q model")),
    x_base    = as.numeric(nbtype),
    x_pos     = case_when(
      exposure == "O3"       ~ x_base + offset_O3,
      exposure == "Compound" ~ x_base + offset_compound,
      TRUE                   ~ x_base + offset_met
    ),
    pt_col = case_when(
      exposure == "O3"                                    ~ col_O3,
      exposure == "Compound" & met_model == "Q model"     ~ col_Q_cmpd,
      exposure == "Compound" & met_model == "T-max model" ~ col_Tmax_cmpd,
      met_model == "Q model"                              ~ col_met_q,
      met_model == "T-max model"                          ~ col_met_tmax,
      TRUE                                                ~ "grey55"
    ),
    pt_shp = case_when(
      exposure == "O3"       ~ 1L,
      exposure == "Compound" ~ 2L,
      TRUE                   ~ 0L
    )
  )

# ── 6. Build one row panel ────────────────────────────────────────────────────
make_row <- function(row_name, show_x = FALSE, show_ylab = TRUE) {

  spec <- row_specs[[row_name]]
  d    <- df_all %>% filter(row_label == row_name)

  ggplot(d, aes(x = x_pos, y = RR,
                color = pt_col, shape = as.factor(pt_shp))) +
    geom_rect(data = shade_df,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
                  fill = fill),
              inherit.aes = FALSE, show.legend = FALSE) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_shape_manual(values = c("0" = 0, "1" = 1, "2" = 2)) +
    geom_hline(yintercept = 1, linetype = "dashed",
               linewidth = 0.5, color = "grey50") +
    geom_pointrange(aes(ymin = CI_lower, ymax = CI_upper),
                    size = 0.35, linewidth = 0.35) +
    annotate("segment",
             x = shade_df$xmin, xend = shade_df$xmax,
             y = -Inf, yend = -Inf,
             color = "grey30", linewidth = 0.8) +
    scale_x_continuous(
      breaks = 1:6,
      labels = if (show_x) as.character(1:6) else rep("", 6),
      limits = c(0.5, 6.5)
    ) +
    scale_y_log10(
      breaks = spec$brk,
      labels = sprintf("%.2f", spec$brk)
    ) +
    coord_cartesian(ylim = spec$ylim, clip = "on") +
    facet_grid(. ~ met_model) +
    labs(x = NULL, y = "RR (95% CI)") +
    theme_fp(show_x = show_x) +
    theme(
      axis.title.y = element_text(size = 7.5, angle = 90,
                                   hjust = 0.5, margin = margin(r = 3))
    )
}

# ── 7. Build all rows ─────────────────────────────────────────────────────────
# SES column — 3 rows, last one shows x labels
p_low   <- make_row("Low SES",   show_x = FALSE)
p_mid   <- make_row("Mid SES",   show_x = FALSE)
p_high  <- make_row("High SES",  show_x = TRUE)

# AGE column — 4 rows, last one shows x labels
p_05    <- make_row("Age 0-5",   show_x = FALSE)
p_635   <- make_row("Age 6-35",  show_x = FALSE)
p_3665  <- make_row("Age 36-65", show_x = FALSE)
p_65p   <- make_row("Age 65+",   show_x = TRUE)

# ── 8. Stack each column with equal heights ───────────────────────────────────
col_ses <- p_low / p_mid / p_high +
  plot_layout(heights = c(1, 1, 1))

col_age <- p_05 / p_635 / p_3665 / p_65p +
  plot_layout(heights = c(1, 1, 1, 1))

# ── 9. Add column headers + row labels ───────────────────────────────────────

# Add row labels on right side of each column
add_ses_labels <- function(p) {
  ggdraw(p) +
    draw_label("Tmax \u2013 MDA8 O\u2083",
               x=0.27, y=0.998, hjust=0.5, vjust=1,
               size=8, fontface="bold", color=col_Tmax_cmpd) +
    draw_label("Q \u2013 MDA8 O\u2083",
               x=0.73, y=0.998, hjust=0.5, vjust=1,
               size=8, fontface="bold", color=col_Q_cmpd) +
    # row labels right side — at 1/6, 3/6, 5/6 of height
    draw_label("Low SES",  x=0.995, y=0.835, hjust=1, vjust=0.5,
               angle=270, size=7.5, fontface="bold", color="grey10") +
    draw_label("Mid SES",  x=0.995, y=0.500, hjust=1, vjust=0.5,
               angle=270, size=7.5, fontface="bold", color="grey10") +
    draw_label("High SES", x=0.995, y=0.165, hjust=1, vjust=0.5,
               angle=270, size=7.5, fontface="bold", color="grey10")
}

add_age_labels <- function(p) {
  ggdraw(p) +
    draw_label("Tmax \u2013 MDA8 O\u2083",
               x=0.27, y=0.998, hjust=0.5, vjust=1,
               size=8, fontface="bold", color=col_Tmax_cmpd) +
    draw_label("Q \u2013 MDA8 O\u2083",
               x=0.73, y=0.998, hjust=0.5, vjust=1,
               size=8, fontface="bold", color=col_Q_cmpd) +
    draw_label("Age 0-5",   x=0.995, y=0.875, hjust=1, vjust=0.5,
               angle=270, size=7.5, fontface="bold", color="grey10") +
    draw_label("Age 6-35",  x=0.995, y=0.625, hjust=1, vjust=0.5,
               angle=270, size=7.5, fontface="bold", color="grey10") +
    draw_label("Age 36-65", x=0.995, y=0.375, hjust=1, vjust=0.5,
               angle=270, size=7.5, fontface="bold", color="grey10") +
    draw_label("Age 65+",   x=0.995, y=0.125, hjust=1, vjust=0.5,
               angle=270, size=7.5, fontface="bold", color="grey10")
}

col_ses_hdr <- add_ses_labels(col_ses)
col_age_hdr <- add_age_labels(col_age)

# ── 10. Combine side by side ──────────────────────────────────────────────────
combined <- plot_grid(
  col_ses_hdr,
  col_age_hdr,
  ncol       = 2,
  align      = "h",
  axis       = "tb",
  rel_widths = c(1, 1)
)

print(combined)

ggsave("forest_plot_SES_AGE.pdf", plot = combined,
       width = 280, height = 220, units = "mm", device = cairo_pdf)
ggsave("forest_plot_SES_AGE.png", plot = combined,
       width = 280, height = 220, units = "mm", dpi = 400)

message("Saved: forest_plot_SES_AGE")
