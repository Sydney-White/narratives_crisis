## show rail data from Donaldson and Hornbeck 

rm(list = ls())
library(sf)
library(tidyverse)
library(purrr)

# download data  ----------------------------------------------------------

# rail  is component six from Donaldson, Hornbeck

years <- c(1890, 1900, 1911)

df_all <- purrr::map(years, ~ read_sf(paste0("Data/NetworkDatabase_HR_2021/", ., "/Component_6_", ., ".shp")))
df_all <- map2(df_all, years, ~ mutate(.x, year = .y))

df_all <- bind_rows(df_all) 

origin_file <- paste0("Data/Shapefiles/")
sf_1900 <- read_sf(paste0(origin_file, "nhgis0001_shapefile_tl2008_us_county_1900/",
                          "US_county_1900_conflated.shp")) %>%
  mutate(NHGISST = as.numeric(NHGISST)) %>%
  mutate(NHGISCTY = as.numeric(NHGISCTY)) %>%
  filter(STATENAM != "Alaska Territory") %>% 
  filter(STATENAM != "Hawaii Territory")
sf_1900 <- st_simplify(sf_1900, dTolerance = 1000, preserveTopology = TRUE) # smooth shape files so code runs faster

# map rail ----------------------------------------------------------------

df_all <- df_all %>% mutate(year = as.character(year))

st_crs(df_all)
st_crs(sf_1900)

map_rail <- function(year_string, string_title) {
  df_year <- df_all %>% filter(year == year_string)
  
  ggplot() +
    geom_sf(data = sf_1900, fill = "lightgrey", color = "darkgrey") + 
    geom_sf(data = df_year) +
    theme_bw() +
    ggtitle(string_title)
}

p <- map_rail("1900", "1900 Railways")
ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/New Independent Var/other_images/rail_1900.pdf",
       device = pdf, bg = "white", width = 6, height = 4.29, units = "in")
