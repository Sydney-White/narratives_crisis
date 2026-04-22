### employed at banks

rm(list = ls())
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(RColorBrewer)

custom_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(10)
custom_palette_diff <- colorRampPalette(RColorBrewer::brewer.pal(11, "RdBu"))(10)

# read data ---------------------------------------------------------------

us_bank_employed <- read_csv("Data/census_micro_data/final_form/all_census_normal.csv")

us_bank_employed <- us_bank_employed %>%
  mutate(
    bank_teller = as.numeric(bank_teller),
    all_employment = as.numeric(all_employment),
    bank_teller_share = if_else(all_employment > 0, bank_teller / all_employment, NA_real_)
  )

# check which counties have highest shares --------------------------------

sf_1900 <- read_sf(
  "Data/Shapefiles/nhgis0001_shapefile_tl2008_us_county_1900/US_county_1900_conflated.shp"
) %>%
  mutate(
    ICPSRST = as.numeric(ICPSRST),
    ICPSRCTY = as.numeric(ICPSRCTY)
  ) %>%
  filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory"))

sf_1900 <- st_simplify(sf_1900, dTolerance = 1000, preserveTopology = TRUE)

county_lookup <- sf_1900 %>%
  st_drop_geometry() %>%
  select(
    ICPSRST,
    ICPSRCTY,
    NHGISNAM,
    STATENAM
  ) %>%
  distinct()

top_bank_teller_share_1900 <- us_bank_employed %>%
  filter(year == 1900) %>%
  left_join(
    county_lookup,
    by = c("stateicp" = "ICPSRST", "countyicp" = "ICPSRCTY")
  ) %>%
  arrange(desc(bank_teller_share)) %>%
  mutate(rank = row_number()) %>%
  select(
    rank,
    STATENAM,
    NHGISNAM,
    stateicp,
    countyicp,
    bank_teller,
    all_employment,
    bank_teller_share
  ) %>%
  slice(1:25)

top_bank_teller_1900 <- us_bank_employed %>%
  filter(year == 1900) %>%
  left_join(
    county_lookup,
    by = c("stateicp" = "ICPSRST", "countyicp" = "ICPSRCTY")
  ) %>%
  arrange(desc(bank_teller)) %>%
  mutate(rank = row_number()) %>%
  select(
    rank,
    STATENAM,
    NHGISNAM,
    stateicp,
    countyicp,
    bank_teller,
    all_employment,
    bank_teller_share
  ) %>%
  slice(1:25)

print(top_bank_teller_share_1900)

# change 1900 -> 1910 -----------------------------------------------------

bank_teller_1900_1910 <- us_bank_employed %>%
  filter(year %in% c(1900, 1910)) %>%
  select(stateicp, countyicp, year, bank_teller, all_employment, bank_teller_share) %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = c(bank_teller, all_employment, bank_teller_share),
    names_sep = "_"
  ) %>%
  mutate(
    bank_teller_diff_1900_1910 = bank_teller_1910 - bank_teller_1900,
    bank_teller_share_diff_1900_1910 = bank_teller_share_1910 - bank_teller_share_1900
  ) %>%
  left_join(
    county_lookup,
    by = c("stateicp" = "ICPSRST", "countyicp" = "ICPSRCTY")
  )

top_bank_teller_share_change_1900_1910 <- bank_teller_1900_1910 %>%
  arrange(desc(bank_teller_share_diff_1900_1910)) %>%
  mutate(rank = row_number()) %>%
  select(
    rank,
    STATENAM,
    NHGISNAM,
    stateicp,
    countyicp,
    bank_teller_share_1900,
    bank_teller_share_1910,
    bank_teller_share_diff_1900_1910,
    bank_teller_1900,
    bank_teller_1910,
    bank_teller_diff_1900_1910
  ) %>%
  slice(1:25)

write.csv(
  top_bank_teller_share_change_1900_1910,
  "Data/data_outputs/top_bank_teller_share_change_1900_1910.csv",
  row.names = FALSE
)

# map bank_teller share ---------------------------------------------------

bank_teller_map_1900 <- us_bank_employed %>%
  filter(year == 1900) %>%
  select(
    stateicp,
    countyicp,
    bank_teller,
    all_employment,
    bank_teller_share
  ) %>%
  full_join(
    sf_1900,
    by = c("stateicp" = "ICPSRST", "countyicp" = "ICPSRCTY")
  ) %>%
  st_as_sf()

quantiles <- quantile(
  bank_teller_map_1900$bank_teller_share,
  c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
  na.rm = TRUE
)

bank_teller_map_1900 <- bank_teller_map_1900 %>%
  mutate(var_cut = cut(bank_teller_share, breaks = unique(quantiles), include.lowest = TRUE)) %>%
  st_as_sf()

p <- ggplot() +
  geom_sf(data = bank_teller_map_1900, aes(fill = var_cut), color = NA, size = 0.1) +
  scale_fill_manual(values = custom_palette, name = "") +
  guides(fill = guide_legend(title = "Bank teller share", nrow = 3)) +
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

ggsave(
  plot = p,
  filename = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures/map_bank_teller_share_1900.pdf",
  device = pdf, bg = "white", width = 6, height = 4.29, units = "in"
)

bank_teller_map_1910 <- us_bank_employed %>%
  filter(year == 1910) %>%
  select(
    stateicp,
    countyicp,
    bank_teller,
    all_employment,
    bank_teller_share
  ) %>%
  full_join(
    sf_1900,
    by = c("stateicp" = "ICPSRST", "countyicp" = "ICPSRCTY")
  ) %>%
  st_as_sf()

quantiles <- quantile(
  bank_teller_map_1910$bank_teller_share,
  c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
  na.rm = TRUE
)

bank_teller_map_1910 <- bank_teller_map_1910 %>%
  mutate(var_cut = cut(bank_teller_share, breaks = unique(quantiles), include.lowest = TRUE)) %>%
  st_as_sf()

p <- ggplot() +
  geom_sf(data = bank_teller_map_1910, aes(fill = var_cut), color = NA, size = 0.1) +
  scale_fill_manual(values = custom_palette, name = "") +
  guides(fill = guide_legend(title = "Bank teller share", nrow = 3)) +
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

ggsave(
  plot = p,
  filename = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures/map_bank_teller_share_1910.pdf",
  device = pdf, bg = "white", width = 6, height = 4.29, units = "in"
)

bank_teller_diff_map <- bank_teller_1900_1910 %>%
  select(
    stateicp,
    countyicp,
    bank_teller_share_diff_1900_1910,
    bank_teller_diff_1900_1910
  ) %>%
  full_join(
    sf_1900,
    by = c("stateicp" = "ICPSRST", "countyicp" = "ICPSRCTY")
  ) %>%
  st_as_sf()

quantiles <- quantile(
  bank_teller_diff_map$bank_teller_share_diff_1900_1910,
  c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
  na.rm = TRUE
)

bank_teller_diff_map <- bank_teller_diff_map %>%
  mutate(var_cut = cut(bank_teller_share_diff_1900_1910, breaks = unique(quantiles), include.lowest = TRUE)) %>%
  st_as_sf()

p <- ggplot() +
  geom_sf(data = bank_teller_diff_map, aes(fill = var_cut), color = NA, size = 0.1) +
  scale_fill_manual(values = custom_palette_diff, name = "") +
  guides(fill = guide_legend(title = "Change in bank teller share (1910 - 1900)", nrow = 3)) +
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

ggsave(
  plot = p,
  filename = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures/map_bank_teller_share_diff_1900_1910.pdf",
  device = pdf, bg = "white", width = 6, height = 4.29, units = "in"
)
