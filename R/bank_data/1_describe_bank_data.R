### describe bank data

library(readr)
library(dplyr)
library(stringr)
library(sf)
library(stargazer)
library(tidyverse)
rm(list = ls())

# plot distribution of crisis ---------------------------------------------

### re load 
df_joined_non_sf <- read.csv("Data/data_inputs/digitized_bank_failures.csv") %>% 
  mutate(Circulation_outstanding_at_failure = as.numeric(Circulation_outstanding_at_failure))

sf_1900 <- read_sf(paste0("Data/data_inputs/Shapefiles/", "nhgis0001_shapefile_tl2008_us_county_1900/",
                          "US_county_1900_conflated.shp")) %>%
  mutate(NHGISST = as.numeric(NHGISST)) %>%
  mutate(NHGISCTY = as.numeric(NHGISCTY)) %>%
  filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory"))
sf_1900 <- st_simplify(sf_1900, dTolerance = 1000, preserveTopology = TRUE) # smooth shape files so code runs faster

df_joined <- df_joined_non_sf %>% 
  full_join(sf_1900, by = c("NHGISST", "NHGISCTY"))

# load data ---------------------------------------------------------------

# failures in 1907-08
df_joined_1907_1908 <- df_joined %>% 
  filter(failure_year %in% c(1907, 1908)) %>% 
  st_as_sf()

county_agg <- df_joined_1907_1908 %>%
  st_drop_geometry() %>%
  group_by(NHGISST, NHGISCTY) %>%  
  summarize(
    cap_fail = sum(Capital_at_failure, na.rm = TRUE),
    circ_fail = sum(Circulation_outstanding_at_failure, na.rm = TRUE),
    .groups = "drop"
  )

df_counties <- sf_1900 %>%
  left_join(county_agg, by = c("NHGISST", "NHGISCTY"))

county_points <- st_centroid(sf_1900) %>%
  left_join(county_agg, by = c("NHGISST", "NHGISCTY"))

p <- ggplot() +
  geom_sf(data = sf_1900, fill = "white", color = "grey85", size = 0.1) +
  geom_sf(
    data = county_points %>% filter(!is.na(cap_fail)),
    aes(size = cap_fail),
    color = "#1E90FF",
    alpha = 0.7
  ) +
  scale_size_continuous(name = "Capital at failure", range = c(2, 10)) +
  coord_sf(crs = 2163) +
  theme_bw() + 
  theme(
    axis.ticks = element_blank(),
    panel.border = element_rect(color = NA, fill = NA),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.position = "bottom")

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/HPE Final Project/figures/capital_fail.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

# show capital outstanding ------------------------------------------------

p <- ggplot() +
  geom_sf(data = sf_1900, fill = "white", color = "grey85", size = 0.1) +
  geom_sf(
    data = county_points %>% filter(!is.na(circ_fail)),
    aes(size = circ_fail),
    color = "#1E90FF",
    alpha = 0.7
  ) +
  scale_size_continuous(name = "Circulation at failure", range = c(2, 10)) +
  coord_sf(crs = 2163) +
  theme_bw() + 
  theme(
    axis.ticks = element_blank(),
    panel.border = element_rect(color = NA, fill = NA),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.position = "bottom")

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/HPE Final Project/figures/circulation_locate.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

# show time series of failed banks -------------------------------------------------------------------------

df_counts <- df_joined_non_sf %>%
  group_by(failure_year) %>% 
  summarise(n_failures = n(), .groups = "drop")

panic_start <- 1907
panic_end   <- 1908

p <- ggplot(df_counts, aes(x = failure_year, y = n_failures)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(color = "black", size = 0.5) +
  annotate(
    "rect",
    xmin = panic_start,
    xmax = panic_end,
    ymin = -Inf, ymax = Inf,
    alpha = 0.2,
    fill = "grey70"
  ) + 
  theme_bw(base_family = "serif") +
  labs(
    x = "",
    y = "No. banks failed",
    title = ""
  )
ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/HPE Final Project/figures/failed_banks_year.pdf",
       device = png, bg = "white", width = 4.88, height = 3.34, units = "in")
