#### run regressions 

rm(list = ls())
library(data.table)
library(janitor)
library(labelled)
library(sf)
library(haven)
library(tidyverse)
library(texreg)
library(fixest)

# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(
#   digest,
#   readr,
#   fuzzyjoin,
#   data.table,
#   stringdist,
#   sf,
#   tidyverse
# )

# -------------------------------------------------------------------------

news_narratives <- read.csv("Data/data_outputs/blame_share_monthly.csv")
bank_exposure <- read.csv("Data/data_outputs/bank_exposure_1907_1908.csv")

bank_mrkt_exposure <- read.csv("Data/data_outputs/bank_market_exposure.csv")
colnames(bank_mrkt_exposure)

us_census_1900 <- read_csv("Data/data_inputs/all_census_normal.csv") %>% 
  filter(year == 1900)
elections <- read.csv("Data/data_outputs/clean_election_data.csv")

sf_1900 <- read_sf(paste0("Data/Shapefiles/", "nhgis0001_shapefile_tl2008_us_county_1900/",
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
  full_join(bank_mrkt_exposure, by = c("NHGISST" = "from_NHGISST",
                                       "NHGISCTY" = "from_NHGISCTY"))

news_narratives_bank <- news_narratives_bank %>%
  mutate(
    bank_circ_exposure_std = scale(bank_circ_exposure),
    bank_cap_exposure_std  = scale(bank_cap_exposure)
  )

base_1 <- feols(
  llm_blame_share ~ bank_circ_exposure_std:after_october | NHGISNAM + month,
  cluster = ~ STATENAM,
  data = news_narratives_bank
)
base_1

base_2 <- feols(
  llm_blame_share ~ bank_cap_exposure_std:after_october | NHGISNAM + month,
  cluster = ~ STATENAM,
  data = news_narratives_bank
)
base_2

bert_base_1 <- feols(
  bert_blame_share ~ bank_circ_exposure_std:after_october | NHGISNAM + month,
  cluster = ~ STATENAM,
  data = news_narratives_bank
)
bert_base_1
bert_base_2 <- feols(
  bert_blame_share ~ bank_cap_exposure_std:after_october | NHGISNAM + month,
  cluster = ~ STATENAM,
  data = news_narratives_bank
)
bert_base_2

coef_names <- list(
  "bank_circ_exposure_std:after_october" = "Circ. Exposure × After Oct. 1907", 
  "bank_cap_exposure_std:after_october" = "Assets Exposure × After Oct. 1907"
)

texreg(
  list(base_1, base_2, bert_base_1, bert_base_2),
  custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
  file = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/tables/exposure_blame.tex",
  label = "table:exposure_blame",
  custom.header = list(
    "\\textit{LLM-Based Blame Share}"  = 1:2,
    "\\textit{BERT-Based Blame Share}" = 3:4
  ),
  custom.coef.map = coef_names,
  custom.gof.rows = list(
    "Controls"   = c("--", "--", "--", "--"),  
    "County FEs" = c("\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
    "Month FEs"  = c("\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark")
  ),
  custom.gof.names = c("N", "$R^2$"),
  stars = c(0.1, 0.05, 0.01),
  digits = 3,
  fontsize = "tiny",
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
  caption = "\\textit{Exposure to Crisis and Financial Blame in Newspapers.}",
  custom.note = paste0(
    "\\item Notes: This table reports estimates of the relationship between ",
    "county-level exposure to the 1907 banking panic and the share of ",
    "newspaper articles assigning domestic financial blame. ",
    "Columns (1)--(2) use blame shares constructed from LLM classifications. ",
    "Columns (3)--(4) use blame shares constructed from predicted probabilities ",
    "of a fine-tuned DistilBERT encoder. ",
    "Exposure is standardized and interacted with an indicator for months ",
    "after October 1907. All specifications include county and month ",
    "fixed effects. Standard errors are clustered at the state level."
  )
)

# -------------------------------------------------------------------------
# first table subset: Census Midwest + Northeast only

northeast_states <- c(
  "Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont",
  "New Jersey", "New York", "Pennsylvania"
)

midwest_states <- c(
  "Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin",
  "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska",
  "North Dakota", "South Dakota"
)

news_narratives_bank_midwest_northeast <- news_narratives_bank %>%
  filter(STATENAM %in% c(northeast_states, midwest_states)) %>%
  mutate(
    bank_circ_exposure_std = scale(bank_circ_exposure),
    bank_cap_exposure_std  = scale(bank_cap_exposure)
  )

base_1_midwest_northeast <- feols(
  llm_blame_share ~ bank_circ_exposure_std:after_october | NHGISNAM + month,
  cluster = ~ STATENAM,
  data = news_narratives_bank_midwest_northeast
)

base_2_midwest_northeast <- feols(
  llm_blame_share ~ bank_cap_exposure_std:after_october | NHGISNAM + month,
  cluster = ~ STATENAM,
  data = news_narratives_bank_midwest_northeast
)

bert_base_1_midwest_northeast <- feols(
  bert_blame_share ~ bank_circ_exposure_std:after_october | NHGISNAM + month,
  cluster = ~ STATENAM,
  data = news_narratives_bank_midwest_northeast
)

bert_base_2_midwest_northeast <- feols(
  bert_blame_share ~ bank_cap_exposure_std:after_october | NHGISNAM + month,
  cluster = ~ STATENAM,
  data = news_narratives_bank_midwest_northeast
)

texreg(
  list(
    base_1_midwest_northeast,
    base_2_midwest_northeast,
    bert_base_1_midwest_northeast,
    bert_base_2_midwest_northeast
  ),
  custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
  file = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/tables/exposure_blame_midwest_northeast.tex",
  label = "table:exposure_blame_midwest_northeast",
  custom.header = list(
    "\\textit{LLM-Based Blame Share}"  = 1:2,
    "\\textit{BERT-Based Blame Share}" = 3:4
  ),
  custom.coef.map = coef_names,
  custom.gof.rows = list(
    "Controls"   = c("--", "--", "--", "--"),
    "County FEs" = c("\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
    "Month FEs"  = c("\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark")
  ),
  custom.gof.names = c("N", "$R^2$"),
  stars = c(0.1, 0.05, 0.01),
  digits = 3,
  fontsize = "tiny",
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
  caption = "\\textit{Exposure to Crisis and Financial Blame in Newspapers (Midwest + Northeast).}",
  custom.note = paste0(
    "\\item Notes: Same specification as the main table, but estimated only on counties in Census Midwest and Northeast states. ",
    "Exposure is standardized within this restricted sample and interacted with an indicator for months after October 1907. ",
    "All specifications include county and month fixed effects. Standard errors are clustered at the state level."
  )
)

# -------------------------------------------------------------------------

news_narratives_final_quarter <- news_narratives %>%
  filter(month > as.Date("1907-10-01"))

colnames(news_narratives_final_quarter)

news_narratives_final_quarter <- news_narratives_final_quarter %>%
  mutate(
    quarter = paste0(year(month), "Q", quarter(month))
  )

colnames(news_narratives_final_quarter)

news_narratives_final_quarter <- news_narratives_final_quarter %>%
  group_by(STATENAM, NHGISNAM, NHGISST, NHGISCTY, quarter) %>%
  summarise(
    total_financial_q = sum(total_financial, na.rm = TRUE),
    
    blame_articles_llm_q = sum(blame_articles_llm, na.rm = TRUE),
    llm_blame_share_q = if_else(
      total_financial_q == 0,
      NA_real_,
      blame_articles_llm_q / total_financial_q
    ),
    
    bert_blame_sum_q = sum(bert_blame_sum, na.rm = TRUE),
    bert_blame_share_q = if_else(
      total_financial_q == 0,
      NA_real_,
      bert_blame_sum_q / total_financial_q
    ),
    
    .groups = "drop"
  )

news_narratives_final_quarter <- news_narratives_final_quarter %>% 
  left_join(sf_1900, by = c("NHGISST", "NHGISCTY"))

## merge with electoral and census baselines 

news_narratives_final <- news_narratives_final_quarter %>% 
  left_join(us_census_1900, by = c("NHGISST" = "state_1890",
                                   "NHGISCTY" = "county_1890")) %>% 
  left_join(elections, c("ICPSRST" = "icpsr_state_code",
                              "ICPSRCTY" = "county_identification_no"))

colnames(news_narratives_final)

rep_llm_base <- feols(
  anti_rep_swing_1904_1908 ~ llm_blame_share_q,
  cluster = ~ STATENAM,
  data = news_narratives_final
)
rep_llm_base

rep_llm_controls <- feols(
  anti_rep_swing_1904_1908 ~ llm_blame_share_q + pct_manuf + pct_urban,
  cluster = ~ STATENAM,
  data = news_narratives_final
)

rep_bert_base <- feols(
  anti_rep_swing_1904_1908 ~ bert_blame_share_q,
  cluster = ~ STATENAM,
  data = news_narratives_final
)

rep_bert_controls <- feols(
  anti_rep_swing_1904_1908 ~ bert_blame_share_q + pct_manuf + pct_urban,
  cluster = ~ STATENAM,
  data = news_narratives_final
)


coef_names <- list(
  "llm_blame_share_q" = "LLM Blame Share (Q4 1907)", 
  "bert_blame_share_q" = "BERT Blame Share (Q4 1907)",
  "pct_manuf"      = "% Manufacturing (1900)",
  "pct_urban"      = "% Urbanization (1900)"
)

texreg(
  l = list(rep_llm_base, rep_llm_controls, rep_bert_base, rep_bert_controls),
  custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
  custom.coef.map = coef_names,
  custom.header = list(
    "\\textit{$\\Delta$ Rep. Loss, 1904--1908}" = 1:4
  ),
  file = "~/Dropbox/Apps/Overleaf/Financial Crisis Narratives - LLM/blame_republican_swing.tex",
  label = "table:blame_republican_swing",
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
  caption = "\\textit{Financial Blame Narratives and Republican Losses, 1904--1908.}",
  custom.note = paste0(
    "\\item Notes: This table reports estimates of the relationship between ",
    "county-level blame-oriented financial narratives in newspapers and changes ",
    "in Republican vote share between the 1904 and 1908 presidential elections. ",
    "Columns (1)--(2) use blame shares constructed from LLM classifications. ",
    "Columns (3)--(4) use blame shares constructed from predicted probabilities ",
    "from a DistilBERT model. ",
    "Even-numbered columns include controls for the share of county employment ",
    "in manufacturing (1900) and the share of the population living in urban areas (1900). ",
    "Standard errors are clustered at the state level."
  )
)
