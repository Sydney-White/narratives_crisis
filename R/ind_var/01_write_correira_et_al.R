## data from correia et al: descriptives from receiverships panel

rm(list = ls())
library(haven)
library(dplyr)
library(ggplot2)
library(stargazer)
library(stringr)
library(purrr)
library(tidygeocoder)
library(sf)

receiverships_panel <- read_dta("Data/data_inputs/qje-repkit-to-upload/sources/occ-receiverships/receiverships_panel.dta")

# parse dates -------------------------------------------------------------

extract_year_regex <- function(x) {
  x <- as.character(x)
  y <- sub(".*((18|19|20)[0-9OISl]{2}).*", "\\1", x)
  y <- ifelse(grepl("^(18|19|20)[0-9OISl]{2}$", y), y, NA_character_)
  y <- gsub("O", "0", y)
  y <- gsub("I", "1", y)
  y <- gsub("l", "1", y)
  y <- gsub("S", "5", y)
  y <- suppressWarnings(as.integer(y))
  ifelse(!is.na(y) & y >= 1800 & y <= 2050, y, NA_integer_)
}

extract_month_regex <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z]", "", x)
  m <- ifelse(grepl("^jan", x), 1,
       ifelse(grepl("^feb", x), 2,
       ifelse(grepl("^mar", x), 3,
       ifelse(grepl("^apr", x), 4,
       ifelse(grepl("^may", x), 5,
       ifelse(grepl("^jun", x), 6,
       ifelse(grepl("^jul", x), 7,
       ifelse(grepl("^aug", x), 8,
       ifelse(grepl("^sep", x), 9,
       ifelse(grepl("^oct", x), 10,
       ifelse(grepl("^nov", x), 11,
       ifelse(grepl("^dec", x), 12, NA_integer_))))))))))))
  as.integer(m)
}

# clean panel -------------------------------------------------------------

receiverships_panel <- receiverships_panel %>%
  mutate(
    receiver_appt_year = extract_year_regex(date_receiver_appt),
    closed_year = extract_year_regex(date_closed),
    receiver_appt_month = extract_month_regex(date_receiver_appt),
    closed_month = extract_month_regex(date_closed),
    receiver_year = ifelse(!is.na(receiver_appt_year), receiver_appt_year, closed_year),
    receiver_month = ifelse(!is.na(receiver_appt_month), receiver_appt_month, closed_month)
  )

clean_cause <- function(x) {
  ifelse(is.na(x) | trimws(x) == "", "Unknown", x)
}

receiverships_panel_clean <- receiverships_panel %>%
  transmute(
    failure_id,
    bank_name,
    receiver_year,
    receiver_month,
    organization_capital,
    failure_capital,
    circulation_at_failure,
    deposits_at_suspension,
    total_assets,
    total_liab_established,
    total_collections_all_sources,
    dividends_paid,
    receivers_salary_other,
    legal_expenses,
    simplified_cause_of_failure
  )

head(receiverships_panel_clean)

write.csv(
  receiverships_panel_clean,
  "Data/data_outputs/receiverships_panel_full.csv",
  row.names = FALSE
)

parsed_banks <- receiverships_panel_clean %>%
  mutate(
    # split on comma
    parts = str_split(bank_name, ","),
    
    # city = second-to-last element
    city_raw = map_chr(parts, ~ {
      if (length(.x) >= 2) str_squish(.x[length(.x) - 1]) else NA_character_
    }),
    
    # state = last element
    state_raw = map_chr(parts, ~ {
      if (length(.x) >= 1) str_squish(.x[length(.x)]) else NA_character_
    }),
    
    # clean city text
    city = city_raw %>%
      str_replace_all("[^A-Za-z .'-]", "") %>%  # remove junk while keeping common punctuation
      str_squish(),
    
    # clean state strings
    state_clean = state_raw %>%
      str_replace_all("[^A-Za-z.]", "") %>%    # keep A–Z and periods
      str_squish() %>%
      str_to_title()
  )

receiverships_panel_clean_1907_1908 <- parsed_banks %>% 
  filter(receiver_year %in% c(1907:1908))

colnames(receiverships_panel_clean_1907_1908)

write.csv(
  receiverships_panel_clean_1907_1908 %>% select(!c(parts, city_raw, state_raw)),
  "Data/data_outputs/receiverships_panel_1907_1908.csv",
  row.names = FALSE
)

df_geo <- receiverships_panel_clean_1907_1908 %>%
  mutate(
    query = paste(city, state_clean, sep = ", ")
  ) %>%
  geocode(
    address = query,
    method = "arcgis",
    lat = latitude,
    long = longitude
  ) %>% 
  filter(!is.na(latitude), !is.na(longitude))

df_geo_sf <- df_geo %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

sf_1900 <- read_sf(paste0("Data/Shapefiles/", "nhgis0001_shapefile_tl2008_us_county_1900/",
                          "US_county_1900_conflated.shp")) %>%
  mutate(ICPSRST = as.numeric(ICPSRST)) %>%
  mutate(ICPSRCTY = as.numeric(ICPSRCTY)) %>%
  filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory"))
sf_1900 <- st_simplify(sf_1900, dTolerance = 1000, preserveTopology = TRUE) # smooth shape files so code runs faster

sf_1900 <- st_transform(sf_1900, 2163)
df_geo_sf <- st_transform(df_geo_sf, 2163)

df_joined <- st_join(df_geo_sf, sf_1900)

df_joined_no_geom <- df_joined %>% 
  st_drop_geometry() %>% 
  select(!c(parts, city_raw, state_raw))
class(df_joined_no_geom)
which(sapply(df_joined_no_geom, is.list))

write.csv(
  df_joined_no_geom,
  "Data/data_outputs/county_level_1907_failures.csv",
  row.names = FALSE
)
