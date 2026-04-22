### Build 1900-1910 Jaremski-Fishback bank panel

rm(list = ls())
library(tidyverse)
library(haven)
library(stargazer)
library(sf)

# paths -------------------------------------------------------------------

input_path <- "Data/Jaremski_Fishback - Replication File/Data/banks+ag.dta"

# 1) Build and write cleaned 1900-1910 bank panel ------------------------

banks_raw <- read_dta(input_path)

banks_panel_1900_1910 <- banks_raw %>%
  filter(yr >= 1900, yr <= 1910) %>%
  transmute(
    state_fips = as.numeric(stateicp),
    county_fips = as.numeric(countycode),
    year = as.integer(yr),
    state_abbrev = state,
    state_name = statename,
    county_name = county,
    num_county_banks = as.numeric(cn),
    num_national_banks = as.numeric(nn),
    num_private_banks = as.numeric(pn),
    num_total_banks = as.numeric(tn),
    total_population = as.numeric(totpop),
    urban_share = as.numeric(urb),
    urban25_share = as.numeric(urb25),
    county_area_sq_miles = as.numeric(sqmi),
    bank_per_10k_population = if_else(
      total_population > 0,
      1e4 * num_total_banks / total_population,
      NA_real_
    ),
    share_county_banks = if_else(
      num_total_banks > 0,
      num_county_banks / num_total_banks,
      NA_real_
    ),
    share_national_banks = if_else(
      num_total_banks > 0,
      num_national_banks / num_total_banks,
      NA_real_
    ),
    share_private_banks = if_else(
      num_total_banks > 0,
      num_private_banks / num_total_banks,
      NA_real_
    ),
    log_num_county_banks = log1p(num_county_banks),
    log_num_national_banks = log1p(num_national_banks),
    log_num_private_banks = log1p(num_private_banks),
    log_num_total_banks = log1p(num_total_banks),
    log_total_population = log1p(total_population),
    log_bank_per_10k_population = log1p(bank_per_10k_population)
  ) %>%
  arrange(year, state_fips, county_fips)

write.csv(banks_panel_1900_1910, "Data/data_outputs/jaremski_fishback.csv")


