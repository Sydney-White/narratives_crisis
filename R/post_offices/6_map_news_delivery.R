### employed in news delivery

rm(list = ls())
library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(RColorBrewer)

custom_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(10)
custom_palette_diff <- colorRampPalette(RColorBrewer::brewer.pal(11, "RdBu"))(10)

save_plot <- function(plot_obj, local_path, overleaf_path) {
  ggsave(
    plot = plot_obj,
    filename = local_path,
    device = pdf,
    bg = "white",
    width = 6,
    height = 4.29,
    units = "in"
  )

  try(
    ggsave(
      plot = plot_obj,
      filename = overleaf_path,
      device = pdf,
      bg = "white",
      width = 6,
      height = 4.29,
      units = "in"
    ),
    silent = TRUE
  )
}

news_delivery_data <- read_csv(
  "Data/census_micro_data/final_form/all_census_normal.csv",
  show_col_types = FALSE
) %>%
  mutate(news_delivery = as.numeric(news_delivery))

sf_1900 <- read_sf(
  "Data/Shapefiles/nhgis0001_shapefile_tl2008_us_county_1900/US_county_1900_conflated.shp"
) %>%
  mutate(
    ICPSRST = as.numeric(ICPSRST),
    ICPSRCTY = as.numeric(ICPSRCTY)
  ) %>%
  filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory"))

sf_1900 <- st_simplify(sf_1900, dTolerance = 1000, preserveTopology = TRUE)

news_delivery_1900_1910 <- news_delivery_data %>%
  filter(year %in% c(1900, 1910)) %>%
  select(stateicp, countyicp, year, news_delivery) %>%
  pivot_wider(
    names_from = year,
    values_from = news_delivery,
    names_prefix = "news_delivery_"
  ) %>%
  mutate(news_delivery_diff_1900_1910 = news_delivery_1910 - news_delivery_1900)

news_delivery_map_1900 <- news_delivery_data %>%
  filter(year == 1900) %>%
  select(stateicp, countyicp, news_delivery) %>%
  full_join(
    sf_1900,
    by = c("stateicp" = "ICPSRST", "countyicp" = "ICPSRCTY")
  ) %>%
  st_as_sf() %>%
  mutate(log_news_delivery = log1p(news_delivery))

quantiles <- quantile(
  news_delivery_map_1900$log_news_delivery,
  c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
  na.rm = TRUE
)

news_delivery_map_1900 <- news_delivery_map_1900 %>%
  mutate(var_cut = cut(log_news_delivery, breaks = unique(quantiles), include.lowest = TRUE)) %>%
  st_as_sf()

ggplot() +
  geom_sf(data = news_delivery_map_1900, aes(fill = var_cut), color = NA, size = 0.1) +
  scale_fill_manual(values = custom_palette, name = "") +
  guides(fill = guide_legend(title = "Log news delivery count", nrow = 3)) +
  coord_sf(crs = 2163) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.key.size = grid::unit(0.8, "lines"),
    panel.border = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )

save_plot(
  plot_obj = p,
  local_path = "Data/data_outputs/map_news_delivery_log_count_1900.pdf",
  overleaf_path = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures/map_news_delivery_log_count_1900.pdf"
)

news_delivery_map_1910 <- news_delivery_data %>%
  filter(year == 1910) %>%
  select(stateicp, countyicp, news_delivery) %>%
  full_join(
    sf_1900,
    by = c("stateicp" = "ICPSRST", "countyicp" = "ICPSRCTY")
  ) %>%
  st_as_sf() %>%
  mutate(log_news_delivery = log1p(news_delivery))

quantiles <- quantile(
  news_delivery_map_1910$log_news_delivery,
  c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
  na.rm = TRUE
)

news_delivery_map_1910 <- news_delivery_map_1910 %>%
  mutate(var_cut = cut(log_news_delivery, breaks = unique(quantiles), include.lowest = TRUE)) %>%
  st_as_sf()

p <- ggplot() +
  geom_sf(data = news_delivery_map_1910, aes(fill = var_cut), color = NA, size = 0.1) +
  scale_fill_manual(values = custom_palette, name = "") +
  guides(fill = guide_legend(title = "Log news delivery count", nrow = 3)) +
  coord_sf(crs = 2163) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.key.size = grid::unit(0.8, "lines"),
    panel.border = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )

save_plot(
  plot_obj = p,
  local_path = "Data/data_outputs/map_news_delivery_log_count_1910.pdf",
  overleaf_path = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures/map_news_delivery_log_count_1910.pdf"
)

news_delivery_diff_map <- news_delivery_1900_1910 %>%
  select(stateicp, countyicp, news_delivery_diff_1900_1910) %>%
  full_join(
    sf_1900,
    by = c("stateicp" = "ICPSRST", "countyicp" = "ICPSRCTY")
  ) %>%
  st_as_sf()

quantiles <- quantile(
  news_delivery_diff_map$news_delivery_diff_1900_1910,
  c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
  na.rm = TRUE
)

news_delivery_diff_map <- news_delivery_diff_map %>%
  mutate(
    var_cut = cut(
      news_delivery_diff_1900_1910,
      breaks = unique(quantiles),
      include.lowest = TRUE
    )
  ) %>%
  st_as_sf()

p <- ggplot() +
  geom_sf(data = news_delivery_diff_map, aes(fill = var_cut), color = NA, size = 0.1) +
  scale_fill_manual(values = custom_palette_diff, name = "") +
  guides(fill = guide_legend(title = "Change in news delivery count (1910 - 1900)", nrow = 3)) +
  coord_sf(crs = 2163) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.key.size = grid::unit(0.8, "lines"),
    panel.border = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.position = "bottom"
  )

save_plot(
  plot_obj = p,
  local_path = "Data/data_outputs/map_news_delivery_count_diff_1900_1910.pdf",
  overleaf_path = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures/map_news_delivery_count_diff_1900_1910.pdf"
)
