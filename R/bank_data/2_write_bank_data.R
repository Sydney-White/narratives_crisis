### create dataset on banks 

library(readr)
library(dplyr)
library(stringr)
library(sf)
library(tidyverse)
library(tidygeocoder)
library(lubridate)
rm(list = ls())

# read data -------------------------------------------------------------------------

bank_failures <- read.csv("Data/archival_data/banks_with_state.csv")

clean_year <- function(x) {
  # Extract ANY 4-digit year between 1863 and 1920
  yr <- str_extract(x, "\\b(18[6-9][0-9]|19[0-1][0-9]|1920)\\b")
  as.integer(yr)
}

all_banks_clean <- bank_failures %>%
  separate(
    Location_and_Bank,
    into = c("city", "bank"),
    sep = ",",
    extra = "merge"
  ) %>%
  mutate(
    year_app   = clean_year(Receiver_appointed),
    year_close = clean_year(Receivership_closed),
    city = str_trim(city),
    bank = str_trim(bank),
    
    failure_year = pmin(year_app, year_close, na.rm = TRUE),
    
    failure_year = if_else(is.infinite(failure_year), NA_integer_, failure_year),
    
    failure_date = if_else(
      !is.na(failure_year),
      ymd(paste0(failure_year, "-01-01")),
      NA_Date_
    )
  )

# geocode to 1900 county boundaries ---------------------------------------------------------------

df_geo <- all_banks_clean %>%
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

df_geo_sf <- df_geo %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

sf_1900 <- read_sf(paste0("Data/data_inputs/Shapefiles/", "nhgis0001_shapefile_tl2008_us_county_1900/",
                          "US_county_1900_conflated.shp")) %>%
  mutate(ICPSRST = as.numeric(ICPSRST)) %>%
  mutate(ICPSRCTY = as.numeric(ICPSRCTY)) %>%
  filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory"))
sf_1900 <- st_simplify(sf_1900, dTolerance = 1000, preserveTopology = TRUE) # smooth shape files so code runs faster

sf_1900 <- st_transform(sf_1900, 2163)
df_geo_sf <- st_transform(df_geo_sf, 2163)

df_joined <- st_join(df_geo_sf, sf_1900)

df_joined_no_geom <- df_joined %>% 
  st_drop_geometry()

write.csv(df_joined_no_geom, "Data/data_outputs/digitized_bank_failures.csv")
