##### Appendix Regressions 

rm(list = ls())
library(data.table)
library(janitor)
library(labelled)
library(haven)
library(tidyverse)
library(texreg)
library(fixest)
library(sf)

# -------------------------------------------------------------------------

coef_names <- list(
  "exposure_circ_z:after_october" = "Circ. Exposure × After Oct. 1907", 
  "exposure_cap_z:after_october" = "Assets Exposure × After Oct. 1907"
)

# -------------------------------------------------------------------------

news_narratives <- read.csv("Data/data_outputs/blame_share_monthly.csv")
bank_exposure <- read.csv("Data/data_outputs/bank_exposure_1907_1908.csv")
us_census_1900 <- read_csv("Data/data_inputs/all_census_normal.csv") %>% 
  filter(year == 1900)
elections <- read.csv("Data/data_outputs/clean_election_data.csv")

sf_1900 <- read_sf(paste0("Data/data_inputs/Shapefiles/", "nhgis0001_shapefile_tl2008_us_county_1900/",
                          "US_county_1900_conflated.shp")) %>%
  mutate(ICPSRST = as.numeric(ICPSRST)) %>%
  mutate(ICPSRCTY = as.numeric(ICPSRCTY)) %>%
  mutate(NHGISCTY = as.numeric(NHGISCTY)) %>%
  mutate(NHGISST = as.numeric(NHGISST)) %>%
  filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory")) %>% 
  select(ICPSRCTY, ICPSRST, NHGISST, NHGISCTY) 
sf_1900 <- st_drop_geometry(sf_1900) 

# -------------------------------------------------------------------------

news_narratives_bank <- news_narratives %>%
  mutate(month = as.Date(month)) %>% 
  mutate(after_october = ifelse(month >= as.Date("1907-10-01"), 1, 0)) %>% 
  full_join(bank_exposure, by = c("STATENAM", "NHGISNAM"))

news_narratives_bank <- news_narratives_bank %>%
  mutate(exposure_circ_z = scale(bank_circ_exposure)[,1], 
         exposure_cap_z = scale(bank_capital_exposure)[,1])

# try subsetted to the Midwest/Northeast ----------------------------------

northeast_midwest_states <- c(
  # Northeast
  "Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island",
  "Connecticut", "New York", "New Jersey", "Pennsylvania",
  
  # Midwest
  "Ohio", "Indiana", "Illinois", "Michigan", "Wisconsin",
  "Minnesota", "Iowa", "Missouri", "North Dakota", "South Dakota",
  "Nebraska", "Kansas"
)

news_subset <- news_narratives_bank %>%
  filter(STATENAM %in% northeast_midwest_states)

base_1 <- feols(
  llm_blame_share ~ exposure_circ_z:after_october | NHGISNAM + month,
  cluster = ~ STATENAM,
  data = news_subset
)
base_1
base_2 <- feols(
  llm_blame_share ~ exposure_cap_z:after_october | NHGISNAM + month,
  cluster = ~ STATENAM,
  data = news_subset
)

base_2

texreg(
  list(base_1, base_2),
  custom.model.names = c("(1)", "(2)"),
  file = "~/Dropbox/Apps/Overleaf/HPE Final Project/appendix_tables/midwest_exposure_blame.tex",
  label = "table:midwest_exposure_blame",
  custom.header = list("\\textit{Blame Share, 1907}" = 1:2),
  custom.coef.map = coef_names,
  custom.gof.rows = list(
    "Controls"   = c("--", "--"),  
    "County FEs" = c("\\checkmark", "\\checkmark"),
    "Month FEs"  = c("\\checkmark", "\\checkmark")
  ),
  custom.gof.names = c("N", "$R^2$"),
  stars = c(0.1, 0.05, 0.01),
  digits = 3,
  booktabs = TRUE,
  threeparttable = TRUE,
  use.packages = FALSE,
  include.nobs = TRUE,
  include.groups = FALSE,
  include.rsquared = TRUE,
  include.adjrs = FALSE,
  include.proj.stats = FALSE,
  include.deviance = FALSE,
  include.loglik = FALSE,
  include.pseudors = FALSE,
  caption = "\\textit{Exposure to Crisis and Financial Blame in Newspapers, subset to Midwest/Northeast.}",
  custom.note = paste0(
    
    "\\item Notes: This table reports estimates of the relationship between ",
    "county-level exposure to the 1907 banking panic and the share of ",
    "newspaper articles assigning domestic financial blame. ",
    "Exposure is standardized and interacted with an indicator for months ",
    "after October 1907. All specifications include county and month ",
    "fixed effects. Standard errors are clustered at the state level."
  )
) 

# run table 2 -------------------------------------------------------------

news_narratives_bank <- news_narratives %>%
  mutate(month = as.Date(month)) %>% 
  mutate(after_october = ifelse(month >= as.Date("1907-10-01"), 1, 0)) %>% 
  full_join(bank_exposure, by = c( "STATENAM",  "NHGISNAM"))

# Appendix Table ---------------------------------------------

colnames(elections)

bank_elections <- elections %>% 
  full_join(bank_exposure, by = c("icpsr_state_code" = "ICPSRST", 
                                  "county_identification_no" = "ICPSRCTY")) %>% 
  full_join(us_census_1900, by = c("NHGISST" = "state_1890", "NHGISCTY" = "county_1890"))

colnames(bank_elections)
## prior is that negative relationship between incumbent government and bank exposure 
bank_elections$z_circ_exposure <- scale(bank_elections$bank_circ_exposure)
bank_elections$z_cap_exposure <- scale(bank_elections$bank_capital_exposure)

base_1 <- feols(anti_rep_swing_1904_1908 ~ z_circ_exposure  | STATENAM, 
                data = bank_elections) 

controls_1 <- feols(anti_rep_swing_1904_1908 ~ z_circ_exposure + pct_manuf +
                      pct_urban  + pct_agric + pct_fb | STATENAM, 
                    data = bank_elections) 
controls_1
base_2 <- feols(anti_rep_swing_1904_1908 ~ z_cap_exposure | STATENAM, 
                data = bank_elections) 
controls_2 <- feols(anti_rep_swing_1904_1908 ~ z_cap_exposure+ 
                      pct_manuf  +  pct_agric + pct_fb | STATENAM, 
                    data = bank_elections) 

coef_names <- list(
  "z_circ_exposure" = "Panic Exposure (Circ.)",
  "z_cap_exposure"  = "Panic Exposure (Cap.)"
)

texreg(list(base_1, controls_1, base_2, controls_2),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       file = "~/Dropbox/Apps/Overleaf/HPE Final Project/appendix_tables/exposure_voting.tex",
       label = "table:exposure_voting",
       custom.header = list("$\\Delta$ Rep. Vote (1904$-$1908)" = 1:4),
       custom.coef.map = coef_names,
       custom.gof.rows = list(
         "Controls" = c("--", "\\checkmark", "--", "\\checkmark"),
         "State FEs" = c("\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark")
       ),
       custom.gof.names = c("N", "$R^2$"),
       stars = c(0.1, 0.05, 0.01),
       digits = 3,
       booktabs = TRUE,
       threeparttable = TRUE,
       use.packages = FALSE,
       include.nobs = TRUE,
       include.groups = FALSE,
       include.rsquared = TRUE,
       include.adjrs = FALSE,
       include.proj.stats = FALSE,
       include.deviance = FALSE,
       include.loglik = FALSE,
       include.pseudors = FALSE,
       caption = "\\textit{Exposure to Crisis and Republican Support.}",
       custom.note = paste0(
         "\\item Notes: This table reports regressions of the change in 
         Republican vote share between 1904 and 1908 on 
         county-level exposure to the 1907 banking panic.
         Columns (1) and (2) use circulation-weighted exposure;
         columns (3) and (4) use capital-weighted exposure. 
         Columns (2) and (4) include controls for manufacturing, 
         urbanization, agriculture, and foreign-born population shares.
         All specifications include state fixed effects, and standard errors
         are clustered at the county level."
       )
)

# consider electoral subset -------------------------------------------------------------------------

news_narratives_final_quarter <- news_narratives %>%
  filter(month > as.Date("1907-10-01"))

colnames(news_narratives_final_quarter)

news_narratives_final_quarter <- news_narratives_final_quarter %>%
  mutate(
    quarter = paste0(year(month), "Q", quarter(month))
  )

news_narratives_final_quarter <- news_narratives_final_quarter %>%
  group_by(STATENAM, NHGISNAM, NHGISST, NHGISCTY, quarter) %>%
  summarise(
    total_financial_q = sum(total_financial, na.rm = TRUE),
    blame_articles_q = sum(blame_articles_llm, na.rm = TRUE),
    blame_share_q4 = if_else(total_financial_q == 0,
                             NA_real_,
                             blame_articles_q / total_financial_q),
    .groups = "drop"
  ) 

news_narratives_final_quarter <- news_narratives_final_quarter %>% 
  left_join(sf_1900, by = c("NHGISST", "NHGISCTY")) 

## merge with electoral and census baselines 

news_narratives_midwest <- news_narratives_final_quarter %>% 
  left_join(us_census_1900, by = c("NHGISST" = "state_1890",
                                   "NHGISCTY" = "county_1890")) %>% 
  left_join(elections, c("ICPSRST" = "icpsr_state_code",
                         "ICPSRCTY" = "county_identification_no")) %>% 
  filter(STATENAM %in% northeast_midwest_states)

base_1 <- feols(
  anti_rep_swing_1904_1908 ~ blame_share_q4,
  cluster = ~ STATENAM,
  data = news_narratives_midwest
)
base_1
controls_1 <- feols(
  anti_rep_swing_1904_1908 ~ blame_share_q4 + pct_manuf + pct_urban,
  cluster = ~ STATENAM,
  data = news_narratives_midwest
)
controls_1

base_2 <- feols(
  anti_rep_swing_1904_1908 ~ blame_share_q4,
  cluster = ~ STATENAM,
  data = news_narratives_midwest
)
base_2
controls_2 <- feols(
  anti_rep_swing_1904_1908 ~ blame_share_q4 + pct_manuf + pct_urban,
  cluster = ~ STATENAM,
  data = news_narratives_midwest
)

coef_names <- list(
  "blame_share_q4" = "Blame Share (Q4 1907)",
  "pct_manuf"      = "% Manufacturing (1900)",
  "pct_urban"      = "% Urbanization (1900)"
)

texreg(
  list(base_1, controls_1, base_2, controls_2),
  custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
  custom.coef.map = coef_names,
  custom.header = list("$\\Delta$ Rep. Vote, 1904-1908" = 1:2, 
                       "$\\Delta$ Socialist Vote, 1904-1908" = 3:4),
  file = "~/Dropbox/Apps/Overleaf/HPE Final Project/appendix_tables/blame_republican_swing_midwest.tex",
  label = "table:blame_republican_swing_midwest",
  stars = c(0.1, 0.05, 0.01),
  digits = 3,
  booktabs = TRUE,
  threeparttable = TRUE,
  use.packages = FALSE,
  include.nobs = TRUE,
  include.groups = FALSE,
  include.rsquared = TRUE,
  include.adjrs = FALSE,
  include.proj.stats = FALSE,
  include.deviance = FALSE,
  include.loglik = FALSE,
  include.pseudors = FALSE,
  caption = "\\textit{Financial Narratives and Electoral Swing, 1904--1908, subset to Midwest/Northeast.}",
  custom.note = paste0(
    "\\item Notes: This table reports estimates of the relationship between ",
    "county-level financial-blame narratives in newspapers and changes in electoral outcomes ",
    "between 1904 and 1908. Models (1) and (3) present baseline specifications. ",
    "Models (2) and (4) add controls for the share of county employment in manufacturing (1900) ",
    "and the share of the population living in urban areas (1900). ",
    "Standard errors are clustered at the state level."
  )
)
