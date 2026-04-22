# Reads census data between 1900 and 1910
# forms main economic variables
# output to all_census_normal.csv

# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(tidyverse)

rm(list = ls())
library(tidyverse)

# load and bind census ----------------------------------------------------

census_1900_compact <- read_csv("Data/census_micro_data/us_census_compact/census_1900_compact.csv",
                                show_col_types = FALSE) %>%
  mutate(year = 1900)
census_1910_compact <- read_csv("Data/census_micro_data/us_census_compact/census_1910_compact.csv",
                                show_col_types = FALSE) %>%
  mutate(year = 1910)

## rbind all into one dataset
all_census <- rbind(census_1900_compact, census_1910_compact)

unique(all_census$year)
gc()

#########################
# process all -- see codebook
########################

aggregate_census <- function(census_file) {
  census_file_aggregate <- census_file %>%
    group_by(year, stateicp, countyicp) %>%
    summarise(ipums_population = sum(n, na.rm = T),
              under_16 = sum(n * (age16==1), na.rm = T),
              nonwhite = sum(n * (race != 1), na.rm = T),
              white = sum(n * (race==1), na.rm = T),
              teacher = sum(n * (occ1950==093),na.rm=T),
              black = sum(n * (race==2), na.rm = T),
              bank_teller = sum(n * (occ1950 == 305), na.rm = T), 
              postmaster = sum(n * (occ1950 == 270), na.rm = T),
              news_delivery = sum(n * (occ1950 == 460), na.rm = T), 
              nontrade_employment = sum(n * (ind1950 %in% c(600:946, 247)), na.rm = T),
              agriculture_employment = sum(n * (ind1950 %in% c(105)), na.rm = T),
              local_govt_employment = sum(n * (ind1950 == 936), na.rm = T), # only look at local government
              all_employment = sum(n * (occ1950 %in% c(000:976)), na.rm = T),
              manufacturing_employment = sum(n * ind1950 %in% c(306:499), na.rm = T),
              under_16_school = sum(n * (school == 2 & age16==1), na.rm=T),
              urban_pop = sum(n * (urban == 2), na.rm=T),
              farm_pop = sum(n * (farm == 2), na.rm = T),
    )
  
  return(census_file_aggregate)
}


census_data_wide <- aggregate_census(all_census)
# census_data_long <- census_data_wide %>%
#   pivot_longer(!c(year, stateicp, countyicp), names_to = "var", values_to="value")
# unique(census_data_long$year)

###################
# normalize to 1890
###################

# county_crosswalk <- fread("Data/Shapefiles/county_county_crosswalks/crosswalks_to_1890_counties.csv") %>%
#   filter(year %in% c(1880:1920))
# 
# census_data_long <- census_data_long %>%
#   full_join(county_crosswalk, by = c("stateicp" = "ICPSRST", "countyicp" = "ICPSRCTY", "year"))
# 
# census_data_long %>%
#   filter(var == "foreign_born") %>%
#   group_by(year) %>%
#   summarise(total_fb = sum(value, na.rm = TRUE))
# 
# # Check ipums_population before normalizing
# census_data_long %>%
#   filter(var == "ipums_population") %>%
#   group_by(year) %>%
#   summarise(total_pop = sum(value, na.rm = TRUE))
# 
# normal_census <- census_data_long %>%
#   group_by(year, state_1890, county_1890, var) %>%
#   summarise(value = sum(value * weight, na.rm = T))
# 
# normal_census %>%
#   filter(var == "foreign_born") %>%
#   group_by(year) %>%
#   summarise(total_fb_norm = sum(value, na.rm = TRUE))
# 
# normal_census %>%
#   filter(var == "ipums_population") %>%
#   group_by(year) %>%
#   summarise(total_pop_norm = sum(value, na.rm = TRUE))
# 
# census_data_wide <- normal_census %>%
#   pivot_wider(names_from = var, values_from = value)

###################
# create share variables
# in some cases for percent employment categories, need to check if denominator (all_employment) is zero. if so,
# then there is zero percent employment
###################

census_data_wide <- census_data_wide %>%
  mutate(
    ### create employment variable
    pct_manuf = ifelse(all_employment ==0, 0, (manufacturing_employment / all_employment) * 100),
    pct_nontrade = ifelse(all_employment == 0, 0, (nontrade_employment / all_employment) * 100),
    pct_agric = ifelse(all_employment == 0, 0, (agriculture_employment / all_employment) * 100),
    
    ### create population variables
    
    pct_urban = ifelse(ipums_population == 0, 0, urban_pop / ipums_population) * 100,
    pct_farm = ifelse(ipums_population == 0, 0, farm_pop / ipums_population) * 100,
    pct_black = ifelse(ipums_population == 0, 0, black / ipums_population) * 100,
    pct_white = ifelse(ipums_population == 0, 0, white / ipums_population) * 100,
    pct_nonwhite = ifelse(ipums_population == 0, 0, nonwhite / ipums_population) * 100 
    
  )

##### add nhgis population data 
# 
# aggregate_pop <- read.csv("Data/original_data/nhgis0029_csv/nhgis0029_ts_nominal_county.csv")
# aggregate_pop <- aggregate_pop %>%
#   pivot_longer(cols = c("A00AA1880":"A00AA1920"),
#                names_to = "var", values_to = "aggregate_pop") %>%
#   mutate(year = as.numeric(substr(var, 6, 9)), .before = STATE) %>%
#   select(year, STATE, COUNTY,  NHGISCTY = COUNTYNH,
#          NHGISST = STATENH, aggregate_pop)
# 
# ## following code normalizes this data to 1890
# county_crosswalk <- fread("Data/Shapefiles/county_county_crosswalks/crosswalks_to_1890_counties.csv") %>%
#   filter(year %in% c(1880:1920))
# 
# aggregate_pop <- aggregate_pop %>%
#   full_join(county_crosswalk, by = c("NHGISST", "NHGISCTY", "year"))
# 
# aggregate_normal <- aggregate_pop %>%
#   group_by(year, state_1890, county_1890) %>%
#   summarise(aggregate_pop = sum(aggregate_pop * weight, na.rm = T)) %>%
#   select(year, county_1890, state_1890, aggregate_pop)
# 
# ## some issues with duplicates?
# 
# aggregate_normal <- aggregate_normal %>%
#   group_by(year, county_1890, state_1890) %>%
#   summarise(population = sum(aggregate_pop, na.rm = TRUE))
# 
# all_census <- census_data_wide %>% # full join
#   full_join(aggregate_normal, by = c('state_1890', "county_1890", "year"))
# all_census %>%
#   count(state_1890, county_1890, year) %>%
#   filter(n > 1)

write_csv(census_data_wide, "Data/census_micro_data/final_form/all_census_normal.csv")

census_data_1910 <- census_data_wide %>% filter(year == 1900)
sum(census_data_1910$news_delivery, na.rm = T)
# # check with aggregate census ---------------------------------------------
# 
# ggplot(all_census, aes(x=population, y=ipums_population)) +
#   geom_point() +
#   xlab("Aggregate ipums_population from NHGIS") +
#   ylab("Aggregate Micro Census Data from IPUMS") +
#   ggtitle("Normalized NHGIS versus Normalized IPUMS Micro Census Data") + theme_classic()
