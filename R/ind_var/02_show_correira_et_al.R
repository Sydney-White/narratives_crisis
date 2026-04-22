### show coerreira et al 

rm(list = ls())
library(haven)
library(dplyr)
library(ggplot2)
library(stargazer)
library(tidyr)
library(sf)

# summary stats table -----------------------------------------------------

receiverships_panel_full <- read.csv("Data/data_outputs/receiverships_panel_full.csv")
receiverships_panel_1907_1908 <- read.csv("Data/data_outputs/receiverships_panel_1907_1908.csv")
colnames(receiverships_panel_full)

# failure reasons table (1907-1908) --------------------------------------

cause_counts_1907_1908 <- receiverships_panel_1907_1908 %>%
  count(simplified_cause_of_failure, name = "n_banks", sort = TRUE)

stargazer(
  as.data.frame(cause_counts_1907_1908),
  summary = FALSE,
  rownames = FALSE,
  header = FALSE,
  label = "tab:receiverships_cause_counts_1907_1908",
  title = "OCC Receiverships by Simplified Cause of Failure, 1907--1908",
  out = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/appendix_tables/receiverships_cause_counts_1907_1908.tex"
)

# map: circles for failures ------------------------------------------------

receiverships_county_1907_1908 <- read.csv("Data/data_outputs/county_level_1907_failures.csv")

sf_1900 <- read_sf(
  "Data/Shapefiles/nhgis0001_shapefile_tl2008_us_county_1900/US_county_1900_conflated.shp"
) %>%
  mutate(NHGISST = as.numeric(NHGISST)) %>%
  mutate(NHGISCTY = as.numeric(NHGISCTY)) %>%
  filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory"))

sf_1900 <- st_simplify(sf_1900, dTolerance = 1000, preserveTopology = TRUE)

county_agg <- receiverships_county_1907_1908 %>%
  mutate(NHGISST = as.numeric(NHGISST), NHGISCTY = as.numeric(NHGISCTY)) %>%
  group_by(NHGISST, NHGISCTY) %>%
  summarize(
    cap_fail = sum(failure_capital, na.rm = TRUE),
    circ_fail = sum(circulation_at_failure, na.rm = TRUE),
    deposits_fail = sum(deposits_at_suspension, na.rm = TRUE),
    .groups = "drop"
  )

county_points <- st_centroid(sf_1900) %>%
  left_join(county_agg, by = c("NHGISST", "NHGISCTY"))

p <- ggplot() +
  geom_sf(data = sf_1900, fill = "white", color = "grey85", size = 0.1) +
  geom_sf(
    data = county_points %>% filter(!is.na(cap_fail), cap_fail > 0),
    aes(size = cap_fail),
    color = "#2A7F7F",
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
    legend.position = "bottom"
  )

ggsave(
  plot = p,
  filename = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures/correira_capital_fail.pdf",
  device = pdf,
  bg = "white", width = 4.88, height = 3.34,
  units = "in"
)

p <- ggplot() +
  geom_sf(data = sf_1900, fill = "white", color = "grey85", size = 0.1) +
  geom_sf(
    data = county_points %>% filter(!is.na(circ_fail), circ_fail > 0),
    aes(size = circ_fail),
    color = "#2A7F7F",
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
    legend.position = "bottom"
  )

ggsave(
  plot = p,
  filename = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures/correira_circulation_locate.pdf",
  device = pdf,
  bg = "white",
  width = 4.88,
  height = 3.34,
  units = "in"
)

p <- ggplot() +
  geom_sf(data = sf_1900, fill = "white", color = "grey85", size = 0.1) +
  geom_sf(
    data = county_points %>% filter(!is.na(deposits_fail), deposits_fail > 0),
    aes(size = deposits_fail),
    color = "#2A7F7F",
    alpha = 0.7
  ) +
  scale_size_continuous(name = "Deposits at failure", range = c(2, 10)) +
  coord_sf(crs = 2163) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    panel.border = element_rect(color = NA, fill = NA),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  )

ggsave(
  plot = p,
  filename = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures/correira_deposits_locate.pdf",
  device = pdf,
  bg = "white",
  width = 4.88,
  height = 3.34,
  units = "in"
)

# deposits-weighted failures time series ----------------------------------

failures_weighted_deposits_by_year <- receiverships_panel_full %>%
  distinct(failure_id, bank_name, receiver_year, receiver_month, deposits_at_suspension) %>%
  filter(!is.na(receiver_year)) %>%
  filter(receiver_year >= 1865, receiver_year <= 1914) %>%
  group_by(receiver_year) %>%
  summarise(
    total_failed_deposits = sum(deposits_at_suspension, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(total_failed_deposits_mil = total_failed_deposits / 1e6)

p <- ggplot(failures_weighted_deposits_by_year, aes(x = receiver_year, y = total_failed_deposits_mil)) +
  annotate(
    "rect",
    xmin = 1907, xmax = 1908,
    ymin = -Inf, ymax = Inf,
    alpha = 0.2, fill = "grey70"
  ) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(color = "black", size = 0.8) +
  theme_bw() +
  labs(x = "", y = "Failed-bank deposits (millions of dollars)", title = "")

ggsave(
  plot = p,
  filename = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures/us_bank_failures_by_year_weighted_deposits.pdf",
  device = pdf, bg = "white", width = 6, height = 4.29, units = "in"
)

# unweighted full panel ---------------------------------------------------

failures_by_year <- receiverships_panel_full %>%
  distinct(failure_id, receiver_year) %>%
  filter(!is.na(receiver_year),
         receiver_year >= 1865,
         receiver_year <= 1914) %>%
  count(receiver_year, name = "total_failures") %>%
  complete(receiver_year = 1865:1914,
           fill = list(total_failures = 0)) %>%
  arrange(receiver_year)

p <- ggplot(failures_by_year, 
       aes(x = receiver_year, y = total_failures)) +
  
  annotate(
    "rect",
    xmin = 1907 - 0.5,
    xmax = 1908 + 0.5,
    ymin = -Inf, ymax = Inf,
    fill = "grey70",
    alpha = 0.25
  ) +
  
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(color = "black", size = 0.8) +
  
  theme_bw() +
  labs(
    x = "",
    y = "Number of failed banks",
    title = ""
  )

ggsave(
  plot = p,
  filename = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures/us_bank_failures_by_year_unweighted.pdf",
  device = pdf, bg = "white", width = 6, height = 4.29, units = "in"
)
