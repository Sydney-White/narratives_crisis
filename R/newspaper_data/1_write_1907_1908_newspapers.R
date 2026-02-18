### chronicling america 

library(readr)
library(tidyverse)
library(arrow)
library(stringr)
library(RColorBrewer)
library(data.table)
library(stargazer)
rm(list = ls())
library(glue)

# load data -------------------------------------------------------------------------

sf_1900 <- read_sf(paste0("Data/data_inputs/Shapefiles/", "nhgis0001_shapefile_tl2008_us_county_1900/",
                          "US_county_1900_conflated.shp")) %>%
  mutate(NHGISST = as.numeric(NHGISST)) %>%
  mutate(NHGISCTY = as.numeric(NHGISCTY)) %>%
  filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory"))
sf_1900 <- st_simplify(sf_1900, dTolerance = 1000, preserveTopology = TRUE) # smooth shape files so code runs faster

chronicling_america <- read_csv("Data/data_inputs/chronicling-america.csv")

year <- 1907
path <- glue::glue("Data/python_data_outputs/filtered_1907_numbered_files")
files <- list.files(path, pattern = "\\.parquet$", full.names = TRUE)
df_1907 <- map_dfr(files, read_parquet)

df_1907 <- df_1907 %>%
  mutate(year = 1907,
         lccn_id = str_extract(file, "sn[0-9]{8}"))

### repeat for 1908
path <- glue::glue("Data/python_data_outputs/filtered_1908_numbered_files")
files <- list.files(path, pattern = "\\.parquet$", full.names = TRUE)

df_1908 <- map_dfr(files, read_parquet)
df_1908 <- df_1908 %>%
  mutate(year = 1908,
         lccn_id = str_extract(file, "sn[0-9]{8}"))

df_full <- rbind(df_1907, df_1908)

all_articles <- df_full %>%
  full_join(chronicling_america, by = c("lccn_id" = "LCCN")) %>%
  mutate(
    date_clean = str_replace(date, " .*$", ""),
    date_clean = as.Date(date_clean)
  )

###### GEOCODE TO THE COUNTY LEVEL 

all_articles_clean <- all_articles %>%
  separate(`Geo Location`, into = c("latitude", "longitude"),
           sep = ",", remove = FALSE, fill = "right") %>%
  mutate(
    latitude  = as.numeric(str_trim(latitude)),
    longitude = as.numeric(str_trim(longitude))
  ) %>%
  filter(!is.na(latitude), !is.na(longitude))

articles_sf <- st_as_sf(
  all_articles_clean,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

sf_1900 <- st_transform(sf_1900, 4326)

articles_with_county <- st_join( ## spatial join 
  articles_sf,
  sf_1900,
  join = st_within,
  left = TRUE
)
articles_df <- st_drop_geometry(articles_with_county)

fwrite(articles_df, "Data/data_outputs/financial_articles_all.csv")
