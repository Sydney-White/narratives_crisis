### funke tables 

rm(list = ls())
library(fixest)
library(tidyverse)
library(haven)

# load data ---------------------------------------------------------------

funke_data <- read_dta("~/Dropbox/Columbia/Spring 2025/Game Theory/R/EER-D-16-00187_replication/EER-D-16-00187_data.dta") 

funke_data <- funke_data %>%
  mutate(warx = ifelse((year >= 1914 & year <= 1918) | (year >= 1939 & year <= 1949), 1, 0))

funke_data <- funke_data %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(
    crisis_ols = crisisJST,
    postcrisis = lag(crisis_ols, 1) + lag(crisis_ols, 2) + lag(crisis_ols, 3) + lag(crisis_ols, 4) + lag(crisis_ols, 5),
    postcrisis = ifelse(is.na(postcrisis), 0, ifelse(postcrisis >= 1, 1, 0))
  ) %>%
  ungroup()

# create macro controls (log differences) 
funke_data <- funke_data %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(
    lrgdp = log(rgdp),
    dlrgdp = 100 * (lrgdp - lag(lrgdp)),
    lcpi = log(cpi),
    dlcpi = 100 * (lcpi - lag(lcpi))
  ) %>%
  ungroup()

# estimate regressions without controls -----------------------------------

# Right-wing vote share regressions - FEs for country 
reg_right_full <- feols(right ~ postcrisis | ccode, data = funke_data 
                            %>% filter(warx != 1))
reg_right_full
reg_right_prewar <- feols(right ~ postcrisis | ccode, data = funke_data
                            %>% filter(warx != 1, year >= 1919, year <= 1938))
reg_right_postwar <- feols(right ~ postcrisis | ccode, data = funke_data %>% 
                             filter(warx != 1, year >= 1950, year <= 2014))
reg_right_postwar

# left-wing vote share regressions -- not signficiant 
reg_left_full <- feols(left ~ postcrisis | ccode, data = funke_data %>% 
                         filter(warx != 1))
reg_left_prewar <- feols(left ~ postcrisis | ccode, data = funke_data %>% 
                           filter(warx != 1, year >= 1919, year <= 1938))
reg_left_postwar <- feols(left ~ postcrisis | ccode, data = funke_data %>% 
                            filter(warx != 1, year >= 1950, year <= 2014))
reg_left_postwar


# estimate regressions with controls --------------------------------------


