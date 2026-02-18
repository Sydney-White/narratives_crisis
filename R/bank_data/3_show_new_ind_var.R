### make new ind. var 

library(readr)
library(dplyr)
library(stringr)
library(sf)
library(stargazer)
library(tidyverse)
library(tidygeocoder)
rm(list = ls())
custom_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(10)

# -------------------------------------------------------------------------

### re load 
df_banks_non_sf <- read.csv("Data/data_outputs/digitized_bank_failures.csv") %>% 
  mutate(Circulation_outstanding_at_failure = as.numeric(Circulation_outstanding_at_failure)) %>% 
  filter(failure_year %in% c(1907:1908))

df_geo <- df_banks_non_sf %>%
  mutate(
    query = paste(city, State, sep = ", ")
  ) %>%
  geocode(
    address = query,
    method = "arcgis",
    lat = latitude,
    long = longitude
  ) %>% 
  # drop any rows where geocoding failed
  filter(!is.na(latitude), !is.na(longitude))

df_banks_sf <- st_as_sf(
  df_geo,
  coords = c("longitude", "latitude"),
  crs = 4326,      # WGS84
  remove = FALSE   # keep lat/long columns
) %>% 
  st_transform(5070)  # project into meters for distance

sf_1900 <- read_sf(paste0("Data/data_inputs/Shapefiles/", "nhgis0001_shapefile_tl2008_us_county_1900/",
                          "US_county_1900_conflated.shp")) %>%
  mutate(NHGISST = as.numeric(NHGISST)) %>%
  mutate(NHGISCTY = as.numeric(NHGISCTY)) %>%
  filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory"))
sf_1900 <- st_simplify(sf_1900, dTolerance = 1000, preserveTopology = TRUE) # smooth shape files so code runs faster

# compute measure ---------------------------------------------------------

sf_1900_proj <- st_transform(sf_1900, 5070)
counties_centroids <- sf_1900_proj %>%
  mutate(geometry = st_centroid(geometry)) %>%
  st_as_sf()

### created weighted measure... 
df_banks_sf <- df_banks_sf %>%
  mutate(weight_circ = Circulation_outstanding_at_failure / max(Circulation_outstanding_at_failure,
                                                           na.rm = TRUE))

dist_mat <- st_distance(counties_centroids, df_banks_sf)
dist_mat_num <- units::drop_units(dist_mat)
inv_dist <- 1 / (1 + dist_mat_num) ## inverse distance 
inv_dist[is.infinite(inv_dist)] <- 0
weighted_dist <- sweep(inv_dist, 2, df_banks_sf$weight_circ, `*`)
bank_circ_exposure <- (rowSums(weighted_dist, na.rm = TRUE)*1000)
hist(bank_circ_exposure)

# map ---------------------------------------------------------------------

sf_1900$bank_circ_exposure <- bank_circ_exposure  # attach metric to polygon data
quantile <- quantile(sf_1900$bank_circ_exposure, c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                                   0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE)

sf_1900 <- sf_1900 %>%
  mutate(var_cut = cut(bank_circ_exposure, breaks = unique(quantile), include.lowest = TRUE)) %>%
  mutate(var_cut = as.factor(var_cut))

p <- ggplot() +
  geom_sf(
    data = sf_1900, aes(fill = var_cut), color = NA,
    size = 0.1
  ) +
  scale_fill_manual(values = custom_palette, name = "") +
  guides(fill = guide_legend(title = "Circ Exposure", nrow = 3))+
  coord_sf(crs = 2163) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.key.size = unit(0.4, "lines"),
    panel.border = element_blank(),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.position = "bottom"
  )

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/HPE Final Project/figures/circ_measure_weighted.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

# -------------------------------------------------------------------------

### created weighted measure... 
df_banks_sf <- df_banks_sf %>%
  mutate(weight_capital = Capital_at_failure / max(Capital_at_failure,
                                                           na.rm = TRUE))

dist_mat <- st_distance(counties_centroids, df_banks_sf)
dist_mat_num <- units::drop_units(dist_mat)
inv_dist <- 1 / (1 + dist_mat_num) ## inverse distance 
inv_dist[is.infinite(inv_dist)] <- 0
weighted_dist <- sweep(inv_dist, 2, df_banks_sf$weight_capital, `*`)
bank_capital_exposure <- rowSums(weighted_dist, na.rm = TRUE)
sf_1900$bank_capital_exposure <- 1000*bank_capital_exposure  # attach metric to polygon data

quantile <- quantile(sf_1900$bank_capital_exposure, c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                              0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE)

sf_1900 <- sf_1900 %>%
  mutate(var_cut = cut(bank_capital_exposure, breaks = unique(quantile), include.lowest = TRUE)) %>%
  mutate(var_cut = as.factor(var_cut))

p <- ggplot() +
  geom_sf(
    data = sf_1900, aes(fill = var_cut), color = NA,
    size = 0.1
  ) +
  scale_fill_manual(values = custom_palette, name = "") +
  guides(fill = guide_legend(title = "Asset Exposure", nrow = 3)) + 
  coord_sf(crs = 2163) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.key.size = unit(0.4, "lines"),
    panel.border = element_blank(),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.position = "bottom"
  )

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/HPE Final Project/figures/assets_measure_weighted.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

output_1900 <- sf_1900 %>% 
  st_drop_geometry() %>% 
  select(!var_cut)
  
write.csv(output_1900, "Data/data_outputs/bank_exposure_1907_1908.csv")
