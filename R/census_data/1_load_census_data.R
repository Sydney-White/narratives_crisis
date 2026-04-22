# script loads all micro census data

# note that if you want to upload from the micro census data, you will need to download
# farm, sex, race, birthplace, literacy, citizenship status, industry codes, occupation codes,
# age under 16, school, and urban.
# then open computer terminal, change directory to file folder,
## use "gunzip" command to unzip the file in terminal and then run

# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(data.table)
rm(list = ls())
library(data.table)
library(tidyverse)

# 1900 --------------------------------------------------------------------

process_1900 <- function(index) {
  message(index)
  df_census_sub <- fread("Data/census_micro_data/usa_00015.csv",
                         nrows = 10000000, skip = (index - 1) * 10000000)
  colnames(df_census_sub) <- tolower(colnames(
    fread("Data/census_micro_data/usa_00015.csv", nrows = 0)))
  df_census_sub[, age16 := 1 * (age <= 16)]
  df_census_sub <- df_census_sub[, .(n = .N
                                     #farm = mean(farm - 1, na.rm = T)
  ), by = .(stateicp, countyicp, farm, sex, race, bpl,
            lit, ind1950, occ1950, citizen, nativity,
            age16, school, urban)]
  return(df_census_sub)
}

indices <- 1:8

df_census <- lapply(indices, process_1900) %>%
  rbindlist()
df_census <- df_census[, .(n = sum(n)),
                       by = .(stateicp, countyicp, sex, race, bpl, lit, occ1950, ind1950, farm,
                              citizen, nativity, age16, school, urban)]
fwrite(df_census, "Data/census_micro_data/us_census_compact/census_1900_compact.csv")
gc()

# 1910 --------------------------------------------------------------------

process_1910 <- function(index) {
  message(index)
  df_census_sub <- fread("Data/census_micro_data/usa_00014.csv",
                         nrows = 10000000, skip = (index - 1) * 10000000)
  colnames(df_census_sub) <- tolower(colnames(
    fread("Data/census_micro_data/usa_00014.csv", nrows = 0)))
  df_census_sub[, age16 := 1 * (age <= 16)]
  df_census_sub <- df_census_sub[, .(n = .N
                                     #farm = mean(farm - 1, na.rm = T)
  ), by = .(stateicp, countyicp, farm, sex, race, bpl,
            lit, ind1950, occ1950, citizen, nativity,
            age16, school, urban)]
  return(df_census_sub)
}

indices <- 1:10

df_census <- lapply(indices, process_1910) %>%
  rbindlist()
df_census <- df_census[, .(n = sum(n)),
                       by = .(stateicp, countyicp, sex, race, bpl, lit, occ1950, ind1950, farm,
                              citizen, nativity, age16, school, urban)]
fwrite(df_census, "Data/census_micro_data/us_census_compact/census_1910_compact.csv")
gc()
