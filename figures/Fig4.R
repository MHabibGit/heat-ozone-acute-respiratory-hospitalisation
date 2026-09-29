library(tidyverse)
library(sf)
library(ggplot2)
library(patchwork)
library(maptiles)
library(tidyterra)
library(grid)

# ── 0. paths ──────────────────────────────────────────────────────────
path_cbs <- "1_weighted_RR.csv" 
path_lcz <- "urban_form.gpkg"

# ── 1. load ───────────────────────────────────────────────────────────
df_cbs <- read.csv(path_cbs, sep = ";", stringsAsFactors = FALSE) %>%
  mutate(crs28992res100m = as.character(crs28992res100m))

LCZ <- st_read(path_lcz)

municipalBoundaries <- st_read("https://service.pdok.nl/cbs/gebiedsindelingen/2024/wfs/v1_0?request=GetFeature&service=WFS&version=1.1.0&outputFormat=application%2Fjson&typeName=gebiedsindelingen:gemeente_gegeneraliseerd") %>%
  filter(statnaam %in% c("Amsterdam", "Rotterdam", "Utrecht", "'s-Gravenhage"))

# one geometry per cell (dedup the nbtype split that caused stacked polygons)
LCZ <- LCZ %>%
  select(crs28992res100m, nbtype_2019, geom) %>%
  distinct(crs28992res100m, .keep_all = TRUE)

lcz <- LCZ %>%
  mutate(lcz = case_when(
    nbtype_2019 %in% c(2, 9) ~ "compact mid-high",
    nbtype_2019 %in% c(3, 4) ~ "open mid-high",
    nbtype_2019 == 5         ~ "compact low",
    nbtype_2019 == 6         ~ "open low 1",
    nbtype_2019 == 8         ~ "open low 2",
    nbtype_2019 %in% c(1, 7) ~ "sparsely built"
  )) %>%
  select(crs28992res100m, lcz, geom)


cities_lcz <- st_intersection(municipalBoundaries, lcz)

base_cells <- cities_lcz %>%
  filter(!duplicated(crs28992res100m)) %>%
  filter(!is.na(lcz)) %>%                       
  mutate(crs28992res100m = as.character(crs28992res100m),
         lcz = factor(lcz, levels = c("compact mid-high", "compact low",
                                      "open mid-high", "open low 2",
                                      "open low 1", "sparsely built"))) %>%
  select(crs28992res100m, statnaam, lcz)

# ── 2. palettes ───────────────────────────────────────────────────────
lcz_colors <- c(
  "compact mid-high" = "#6e016b",
  "compact low"      = "#88419d",
  "open mid-high"    = "#8c6bb1",
  "open low 2"       = "#8c96c6",
  "open low 1"       = "#9ebcda",
  "sparsely built"   = "#edf8fb"
)

dom_levels <- c("Low-SES higher", "High-SES higher", "Low = High")
dom_colors <- c(
  "Low-SES higher"  = "#e66101",   # orange
  "High-SES higher" = "#5e3c99",   # purple (PuOr, colourblind-safe pair)
  "Low = High"      = "#969696"    # grey - tie
)

# ── 3. dominance tables (one per metric) ──────────────────────────────
rr_rank <- c("<= 0.95" = 1, "0.95-1.05" = 2, "1.05-1.15" = 3,
             "1.15-1.25" = 4, "> 1.25" = 5)

normalize_rr <- function(x) {
  key <- gsub("[[:space:],]", "", x)
  dplyr::case_when(
    key == "<=0.95"      ~ "<= 0.95",
    key == ">0.95<=1.05" ~ "0.95-1.05",
    key == ">1.05<=1.15" ~ "1.05-1.15",
    key == ">1.15<=1.25" ~ "1.15-1.25",
    key == ">1.25"       ~ "> 1.25",
    TRUE                 ~ NA_character_
  )
}

build_dom <- function(rr_var) {
  df_cbs %>%
    mutate(rank = rr_rank[normalize_rr(.data[[rr_var]])]) %>%
    filter(!is.na(rank)) %>%
    group_by(statnaam, crs28992res100m, income) %>%      # collapse cell x nbtype
    summarise(rank = max(rank), .groups = "drop") %>%    # worst-case per (cell, income)
    pivot_wider(names_from = income, values_from = rank) %>%
    rename(low = `low SES`, mid = `mid SES`, high = `high SES`) %>%
    mutate(
      top     = pmax(low, mid, high, na.rm = TRUE),
      is_low  = !is.na(low)  & low  == top,
      is_mid  = !is.na(mid)  & mid  == top,
      is_high = !is.na(high) & high == top,
      k_top   = is_low + is_mid + is_high,
      dominant = case_when(
        k_top > 1 ~ "Tied",
        is_low    ~ "Low",
        is_mid    ~ "Mid",
        is_high   ~ "High"
      )
    )
}

# elevated cells only get a colour; everything else -> NA (undrawn)
make_dom_cat <- function(dom) {
  out <- dom %>%
    transmute(statnaam, crs28992res100m,
              cat = case_when(
                top >= 3 & dominant == "Low"  ~ "Low-SES higher",
                top >= 3 & dominant == "High" ~ "High-SES higher",
                top >= 3 & dominant == "Tied" ~ "Low = High",
                top >= 3 & dominant == "Mid"  ~ "Mid-SES higher",  # expected empty
                TRUE ~ NA_character_
              ))
  if (any(out$cat == "Mid-SES higher", na.rm = TRUE))
    warning("Mid-dominant elevated cells exist - add Mid to the legend.")
  out
}

dom_cat_q    <- make_dom_cat(build_dom("weighted_RR_Q_Ozone_range"))
dom_cat_tmax <- make_dom_cat(build_dom("weighted_RR_Tmax_Ozone_range"))

# ── 4. city windows + basemaps ────────────────────────────────────────
cities <- data.frame(
  city = c("Rotterdam", "Amsterdam", "Utrecht", "'s-Gravenhage"),
  lon  = c(4.4666,      4.92,        5.08,      4.3107),
  lat  = c(51.920,      52.358,      52.0907,   52.0705)
)
cities_rd <- cities %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(28992)

make_window <- function(point, width = 20000, height = 16500) {
  xy <- st_coordinates(point)
  st_as_sfc(st_bbox(c(xmin = xy[1] - width/2, xmax = xy[1] + width/2,
                      ymin = xy[2] - height/2, ymax = xy[2] + height/2),
                    crs = st_crs(point)))
}
windows_sf <- do.call(rbind, lapply(seq_len(nrow(cities_rd)), function(i)
  st_sf(city = cities_rd$city[i], geometry = make_window(cities_rd[i, ]))))

city_order <- c("Amsterdam", "Rotterdam", "Utrecht", "'s-Gravenhage")
city_labs  <- c("Amsterdam" = "AMS", "Rotterdam" = "ROT",
                "Utrecht" = "UTR", "'s-Gravenhage" = "HAG")

city_tiles <- setNames(lapply(city_order, function(ct) {
  win <- st_sf(geometry = windows_sf %>% filter(city == !!ct) %>% st_geometry())
  get_tiles(win, provider = "CartoDB.Positron", zoom = 12, crop = TRUE)
}), city_order)

# ── 5. panel builders ─────────────────────────────────────────────────
map_shell <- function(bb) list(
  coord_sf(xlim = c(bb[["xmin"]], bb[["xmax"]]),
           ylim = c(bb[["ymin"]], bb[["ymax"]]), expand = FALSE),
  theme_void(base_family = "Helvetica"),
  theme(plot.margin  = margin(1, 1, 1, 1),
        panel.border = element_rect(color = "grey30", fill = NA, linewidth = 0.4),
        legend.title = element_text(size = 9, face = "bold"),
        legend.text  = element_text(size = 8))
)

make_lcz_panel <- function(city_name) {
  cells <- base_cells %>% filter(statnaam == city_name)
  bound <- municipalBoundaries %>% filter(statnaam == city_name) %>%
    st_transform(st_crs(cells))
  tiles <- city_tiles[[city_name]]
  bb    <- st_bbox(windows_sf %>% filter(city == city_name) %>% st_geometry())

  ggplot() +
    geom_spatraster_rgb(data = tiles) +
    geom_sf(data = cells, aes(fill = lcz), color = NA, alpha = 0.9) +
    scale_fill_manual(values = lcz_colors, name = "LCZ type",
                      limits = names(lcz_colors), drop = FALSE, na.value = "grey85") +
    geom_sf(data = bound, fill = NA, color = "black", linewidth = 0.5) +
    map_shell(bb)
}

make_dom_panel <- function(city_name, dom_cat) {
  foot <- base_cells %>% filter(statnaam == city_name)          # faint footprint
  elev <- foot %>%
    inner_join(dom_cat %>% filter(!is.na(cat)),
               by = c("statnaam", "crs28992res100m")) %>%
    mutate(cat = factor(cat, levels = dom_levels))
  bound <- municipalBoundaries %>% filter(statnaam == city_name) %>%
    st_transform(st_crs(foot))
  tiles <- city_tiles[[city_name]]
  bb    <- st_bbox(windows_sf %>% filter(city == city_name) %>% st_geometry())

  ggplot() +
    geom_sf(data = foot, fill = "grey92", color = NA) +   # context (no basemap)
    geom_sf(data = elev, aes(fill = cat), color = NA) +   # the story
    scale_fill_manual(values = dom_colors, name = "Highest-risk SES group (elevated cells)",
                      limits = dom_levels, drop = FALSE) +
    geom_sf(data = bound, fill = NA, color = "grey20", linewidth = 0.5) +
    map_shell(bb)
}
# ── 6. assemble ───────────────────────────────────────────────────────
lcz_panels  <- setNames(lapply(city_order, make_lcz_panel), city_order)
domQ_panels <- setNames(lapply(city_order, make_dom_panel, dom_cat = dom_cat_q),    city_order)
domT_panels <- setNames(lapply(city_order, make_dom_panel, dom_cat = dom_cat_tmax), city_order)

blank <- wrap_elements(grid::textGrob(""))
hdr   <- function(t) wrap_elements(grid::textGrob(t, gp = grid::gpar(fontsize = 11, fontface = "bold")))
lbl   <- function(t) wrap_elements(grid::textGrob(t, rot = 90, gp = grid::gpar(fontsize = 11, fontface = "bold")))

header_row <- blank | hdr("LCZ") | hdr("Dominance (Q-MDA8 O3)") | hdr("Dominance (Tmax-MDA8 O3)")
city_rows  <- lapply(city_order, function(ct)
  lbl(city_labs[[ct]]) | lcz_panels[[ct]] | domQ_panels[[ct]] | domT_panels[[ct]])

final <- Reduce(`/`, c(list(header_row), city_rows)) +
  plot_layout(heights = c(0.08, rep(1, 4)),
              widths  = c(0.05, rep(1, 3)),
              guides  = "collect") &
  theme(legend.position = "bottom")

ggsave("fig_lcz_dominance_Q_Tmax.png", final,
       width = 210, height = 230, units = "mm", dpi = 300, bg = "white")
ggsave("fig_lcz_dominance_Q_Tmax.pdf", final,
       width = 210, height = 230, units = "mm", device = cairo_pdf, bg = "white")

final

