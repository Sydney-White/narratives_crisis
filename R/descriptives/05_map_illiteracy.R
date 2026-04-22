#### Show illiteracy levels in 1900 

rm(list = ls())
library(sf)
library(tidyverse)
library(RColorBrewer)
custom_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(10)

# -------------------------------------------------------------------------

sf_1900 <- read_sf(paste0("Data/Shapefiles/", "nhgis0001_shapefile_tl2008_us_county_1900/",
                          "US_county_1900_conflated.shp")) %>%
  mutate(NHGISST = as.numeric(NHGISST)) %>%
  mutate(NHGISCTY = as.numeric(NHGISCTY)) %>%
  filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory"))
sf_1900 <- st_simplify(sf_1900, dTolerance = 1000, preserveTopology = TRUE) # smooth shape files so code runs faster

us_census_1900 <- read_csv("Data/data_inputs/all_census_normal.csv") %>% 
  filter(year == 1900) %>% 
  select(year, state_1890, county_1890, illiterate, population) %>% 
  mutate(pct_illiterate = illiterate/population)

### map percent illiterate 
quantile <- quantile(us_census_1900$pct_illiterate, c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                              0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE)

us_census_1900 <- us_census_1900 %>%
  mutate(var_cut = cut(pct_illiterate, breaks = quantile)) %>%
  mutate(var_cut = as.factor(var_cut))

us_census_1900 <- us_census_1900 %>% 
  full_join(sf_1900, by = c("state_1890" = "NHGISST", "county_1890" = "NHGISCTY")) %>% 
  st_as_sf()

p <- ggplot() +
  geom_sf(
    data = us_census_1900, aes(fill = var_cut), color = NA,
    size = 0.1
  ) +
  scale_fill_manual(values = custom_palette, name = "") +
  guides(fill = guide_legend(title = "% Illiterate", nrow = 3))+
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

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures/illiteracy_national.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

# national level ----------------------------------------------------------

us_illiteracy_1900 <- read_csv("Data/data_inputs/all_census_normal.csv") %>% 
  filter(year == 1900) %>% 
  select(illiterate, population) %>% 
  summarise(
    total_illiterate = sum(illiterate, na.rm = TRUE),
    total_population = sum(population, na.rm = TRUE),
    national_pct_illiterate = total_illiterate / total_population
  )
