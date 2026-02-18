#### fix up electoral data 

rm(list = ls())
library(data.table)
library(janitor)
library(labelled)
library(haven)
library(tidyverse)

# -------------------------------------------------------------------------

elections <- read_dta("Data/data_inputs/County-Election Data/DS0001/08611-0001-Data.dta")

elections <- labelled::foreign_to_labelled(elections) # makes the stata labels into R
elections <- sjlabelled::label_to_colnames(elections)
colnames(elections) <- tolower(colnames(elections))
colnames(elections) <- janitor::make_clean_names(colnames(elections))

elections_clean <- elections %>% ## extract the problematic missing data 
  filter(x1904_pres_rep_percent <= 100) %>% 
  filter(x1908_pres_rep_percent <= 100)

elections_1900 <- elections_clean %>%
  mutate(
    rep_swing_1904_1908 = x1908_pres_rep_percent - x1904_pres_rep_percent, 
    rep_swing_1904_1912 = x1912_pres_rep_percent - x1904_pres_rep_percent, 
    
    socialist_swing_1904_1908 = x1908_pres_socialist_percent - x1904_pres_socialist_percent, 
    socialist_swing_1904_1912 = x1912_pres_s0cialist_percent - x1904_pres_socialist_percent,
    
    anti_rep_1904 = 100 - x1904_pres_rep_percent,
    anti_rep_1908 = 100 - x1908_pres_rep_percent,
    anti_rep_swing_1904_1908 = anti_rep_1908 - anti_rep_1904,

  ) %>%  ## keep only id + these vars 
  select(icpsr_state_code, county_identification_no, 
         x1904_pres_rep_percent, x1904_pres_dem_percent, 
         x1908_pres_rep_percent, x1908_pres_dem_percent, 
         x1912_pres_rep_percent, x1912_pres_dem_percent, x1912_pres_s0cialist_percent,
         rep_swing_1904_1908:anti_rep_swing_1904_1908)

write.csv(elections_1900, "Data/data_outputs/clean_election_data.csv")
