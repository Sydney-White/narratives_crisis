## 04 jaremski fishback 

rm(list = ls())
library(tidyverse)
library(haven)
library(stargazer)
library(sf)
library(RColorBrewer)
library(classInt)
custom_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(10)

# Read back the saved panel and run descriptives from this dataset --------

banks_panel <- read.csv("Data/data_outputs/jaremski_fishback.csv")

# 3) Distribution plot and summary tables (1900) -------------------------

banks_1900 <- banks_panel %>%
  filter(year == 1900)

colnames(banks_1900)
p <- ggplot(banks_1900, aes(x = bank_per_10k_population)) +
  geom_histogram(bins = 50) +
  theme_bw()

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/New Independent Var/jaremski_data/banks_per_10k.pdf", 
       device = pdf, bg = "white", width = 6, height = 4.29, units = "in")

banks_summary_1900 <- banks_1900 %>%
  select(
    num_county_banks,
    num_national_banks,
    num_private_banks,
    num_total_banks,
    total_population,
    bank_per_10k_population,
    log_num_total_banks
  )

stargazer(
  as.data.frame(banks_summary_1900),
  header = FALSE,
  label = "banks_summary_1900",
  title = "County-level summary stats for 1900.",
  out = "~/Dropbox/Apps/Overleaf/New Independent Var/jaremski_data/summary_stats.tex"
)

top_counties_1900 <- banks_1900 %>%
  arrange(desc(num_total_banks)) %>%
  slice(1:10) %>%
  select(
    state_abbrev,
    county_name,
    num_county_banks,
    num_national_banks,
    num_private_banks,
    num_total_banks
  )

stargazer(
  as.data.frame(top_counties_1900),
  summary = FALSE, rownames = FALSE, header = FALSE,
  font.size = "small",
  label = "top_counties_1900",
  title = "Top ten counties with highest number of banks in 1900.",
  out = "~/Dropbox/Apps/Overleaf/New Independent Var/jaremski_data/top_ten.tex"
)

# 4) 1900 map of log total banks -----------------------------------------

sf_1900 <- read_sf("Data/Shapefiles/nhgis0001_shapefile_tl2008_us_county_1900/US_county_1900_conflated.shp") %>%
  mutate(
    ICPSRST = as.numeric(ICPSRST),
    ICPSRCTY = as.numeric(ICPSRCTY),
    NHGISST = as.numeric(NHGISST),
    NHGISCTY = as.numeric(NHGISCTY)
  ) %>%
  st_simplify(dTolerance = 1000, preserveTopology = TRUE)

banks_sf_1900 <- banks_1900 %>%
  full_join(sf_1900,
            by = c("state_fips" = "ICPSRST", "county_fips" = "ICPSRCTY"),
            relationship = "many-to-many"
  ) %>%
  st_as_sf() %>%
  filter(STATENAM != "Alaska Territory", STATENAM != "Hawaii Territory") %>%
  st_simplify(dTolerance = 1000, preserveTopology = TRUE)

# try interacted measure  -------------------------------------------------

## load main measure 
bank_market_exposure <- read.csv("Data/data_outputs/bank_market_exposure.csv")

banks_sf_1900 <- banks_sf_1900 %>% # 0 don't join 
  full_join(bank_market_exposure, by = c("NHGISST" = "from_NHGISST", "NHGISCTY" = "from_NHGISCTY")) %>% 
  mutate(
    effective_dep_exposure = bank_dep_exposure * log1p(bank_per_10k_population)
  )

## show log number of banks 
quantiles <- quantile(
  banks_sf_1900$effective_dep_exposure,
  c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
  na.rm = TRUE
)

banks_sf_1900 <- banks_sf_1900 %>%
  mutate(var_cut = cut(effective_dep_exposure, breaks = unique(quantiles))) %>%
  st_as_sf()

p <- ggplot() +
  geom_sf(data = banks_sf_1900, aes(fill = var_cut), color = NA, size = 0.1) +
  scale_fill_manual(values = custom_palette, name = "") +
  guides(fill = guide_legend(title = "Deposit Loss x Pre-existing", nrow = 3)) +
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

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/New Independent Var/jaremski_data/map_log_num_banks_1900.pdf", 
       device = pdf, bg = "white", width = 6, height = 4.29, units = "in")

# -------------------------------------------------------------------------


## show log number of banks 
quantiles <- quantile(
  banks_sf_1900$log_num_total_banks,
  c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
  na.rm = TRUE
)

banks_sf_1900 <- banks_sf_1900 %>%
  mutate(var_cut = cut(log_num_total_banks, breaks = unique(quantiles))) %>%
  st_as_sf()

p <- ggplot() +
  geom_sf(data = banks_sf_1900, aes(fill = var_cut), color = NA, size = 0.1) +
  scale_fill_manual(values = custom_palette, name = "") +
  guides(fill = guide_legend(title = "Log Num Banks", nrow = 3)) +
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

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/New Independent Var/jaremski_data/map_log_num_banks_1900.pdf", 
       device = pdf, bg = "white", width = 6, height = 4.29, units = "in")

# map banks per 10k people ------------------------------------------------

## show per 10k people 
quantiles <- quantile(
  banks_sf_1900$bank_per_10k_population,
  c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
  na.rm = TRUE
)

banks_sf_1900 <- banks_sf_1900 %>%
  mutate(var_cut = cut(bank_per_10k_population, breaks = unique(quantiles))) %>%
  st_as_sf()

p <- ggplot() +
  geom_sf(data = banks_sf_1900, aes(fill = var_cut), color = NA, size = 0.1) +
  scale_fill_manual(values = custom_palette, name = "") +
  guides(fill = guide_legend(title = "Banks per 10k", nrow = 3)) +
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

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures/map_banks_per_10k.pdf", 
       device = pdf, bg = "white", width = 6, height = 4.29, units = "in")
ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/New Independent Var/jaremski_data/map_banks_per_10k.pdf", 
       device = pdf, bg = "white", width = 6, height = 4.29, units = "in")
