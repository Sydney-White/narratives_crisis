rm(list = ls())

library(readr)
library(dplyr)
library(ggplot2)
library(fixest)
library(sf)
library(texreg)

# Explore whether the change in Socialist vote share from 1904 to 1912
# is associated with county exposure to the 1907-1908 crisis.

elections <- read_csv("Data/data_outputs/clean_election_data.csv", show_col_types = FALSE) %>%
  select(
    icpsr_state_code,
    county_identification_no,
    x1904_pres_rep_percent,
    socialist_swing_1904_1912
  )

county_crosswalk_1900 <- read_sf(
  "Data/Shapefiles/nhgis0001_shapefile_tl2008_us_county_1900/US_county_1900_conflated.shp"
) %>%
  st_drop_geometry() %>%
  transmute(
    nhgis_state_code = as.numeric(NHGISST),
    nhgis_county_code = as.numeric(NHGISCTY),
    icpsr_state_code = as.numeric(ICPSRST),
    county_identification_no = as.numeric(ICPSRCTY)
  ) %>%
  distinct()

bank_exposure <- read_csv("Data/data_outputs/bank_market_exposure.csv", show_col_types = FALSE) %>%
  transmute(
    nhgis_state_code = from_NHGISST,
    nhgis_county_code = from_NHGISCTY,
    bank_circ_exposure,
    bank_cap_exposure,
    adjusted_bank_dep_exposure
  ) %>%
  left_join(
    county_crosswalk_1900,
    by = c("nhgis_state_code", "nhgis_county_code")
  )

county_failures <- read_csv("Data/data_outputs/county_level_1907_failures.csv", show_col_types = FALSE) %>%
  transmute(
    icpsr_state_code = ICPSRST,
    county_identification_no = ICPSRCTY,
    failure_capital,
    circulation_at_failure,
    deposits_at_suspension
  ) %>%
  group_by(icpsr_state_code, county_identification_no) %>%
  summarise(
    n_failures_1907_1908 = n(),
    total_failure_capital = sum(failure_capital, na.rm = TRUE),
    total_failure_circulation = sum(circulation_at_failure, na.rm = TRUE),
    total_failure_deposits = sum(deposits_at_suspension, na.rm = TRUE),
    .groups = "drop"
  )

census_1900 <- read_csv("Data/census_micro_data/final_form/all_census_normal.csv", show_col_types = FALSE) %>%
  filter(year == 1900) %>%
  transmute(
    icpsr_state_code = stateicp,
    county_identification_no = countyicp,
    ipums_population,
    pct_urban,
    pct_manuf,
    pct_agric,
    pct_nonwhite,
    postmaster,
    news_delivery
  ) %>%
  mutate(
    log_population_1900 = log(ipums_population),
    log_postmaster_1900 = log1p(postmaster),
    log_news_delivery_1900 = log1p(news_delivery)
  )

electoral_analysis_df <- elections %>%
  left_join(bank_exposure, by = c("icpsr_state_code", "county_identification_no")) %>%
  left_join(county_failures, by = c("icpsr_state_code", "county_identification_no")) %>%
  left_join(census_1900, by = c("icpsr_state_code", "county_identification_no")) %>%
  mutate(
    n_failures_1907_1908 = coalesce(n_failures_1907_1908, 0),
    total_failure_capital = coalesce(total_failure_capital, 0),
    total_failure_circulation = coalesce(total_failure_circulation, 0),
    total_failure_deposits = coalesce(total_failure_deposits, 0),
    any_failure_1907_1908 = as.integer(n_failures_1907_1908 > 0)
  )

# Core exploratory controls are pre-crisis county characteristics.
analysis_sample <- electoral_analysis_df %>%
  filter(
    !is.na(socialist_swing_1904_1912),
    !is.na(bank_circ_exposure),
    !is.na(bank_cap_exposure),
    !is.na(adjusted_bank_dep_exposure),
    !is.na(log_population_1900),
    !is.na(pct_urban),
    !is.na(pct_manuf),
    !is.na(pct_agric),
    !is.na(pct_nonwhite)
  ) %>%
  mutate(
    log_adjusted_bank_dep_exposure = log1p(adjusted_bank_dep_exposure),
    std_adjusted_bank_dep_exposure = as.numeric(scale(adjusted_bank_dep_exposure)),
    std_log_adjusted_bank_dep_exposure = as.numeric(scale(log_adjusted_bank_dep_exposure))
  )

model_1 <- feols(
  socialist_swing_1904_1912 ~ std_adjusted_bank_dep_exposure,
  data = analysis_sample
)

model_2 <- feols(
  socialist_swing_1904_1912 ~ std_adjusted_bank_dep_exposure + pct_urban + log_population_1900 +
    pct_manuf + pct_agric + pct_nonwhite,
  data = analysis_sample
)

model_3 <- feols(
  socialist_swing_1904_1912 ~ std_adjusted_bank_dep_exposure + pct_urban + log_population_1900 +
    pct_manuf + pct_agric + pct_nonwhite + x1904_pres_rep_percent |
    icpsr_state_code,
  data = analysis_sample
)

model_4 <- feols(
  socialist_swing_1904_1912 ~ std_adjusted_bank_dep_exposure + pct_urban + log_population_1900 +
    pct_manuf + pct_agric + pct_nonwhite + x1904_pres_rep_percent +
    log_postmaster_1900 + log_news_delivery_1900 |
    icpsr_state_code,
  data = analysis_sample
)

model_5 <- feols(
  socialist_swing_1904_1912 ~ log_adjusted_bank_dep_exposure + pct_urban + log_population_1900 +
    pct_manuf + pct_agric + pct_nonwhite + x1904_pres_rep_percent |
    icpsr_state_code,
  data = analysis_sample
)

model_6 <- feols(
  socialist_swing_1904_1912 ~ std_log_adjusted_bank_dep_exposure + pct_urban + log_population_1900 +
    pct_manuf + pct_agric + pct_nonwhite + x1904_pres_rep_percent |
    icpsr_state_code,
  data = analysis_sample
)

socialist_scatter_dep <- ggplot(
  analysis_sample,
  aes(x = log_adjusted_bank_dep_exposure, y = socialist_swing_1904_1912)
) +
  geom_point(alpha = 0.45, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
  labs(
    x = "log(1 + adjusted bank deposit exposure)",
    y = "Socialist vote swing, 1904-1912",
    title = "Socialist Vote Swing and Logged Adjusted Deposit Exposure"
  ) +
  theme_minimal(base_size = 12)

socialist_scatter_failures <- ggplot(
  analysis_sample,
  aes(x = n_failures_1907_1908, y = socialist_swing_1904_1912)
) +
  geom_point(alpha = 0.45, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
  labs(
    x = "Number of bank failures, 1907-1908",
    y = "Socialist vote swing, 1904-1912",
    title = "Socialist Vote Swing and County Failures"
  ) +
  theme_minimal(base_size = 12)

cat("Analysis sample size:", nrow(analysis_sample), "\n")
print(
  analysis_sample %>%
    summarise(
      mean_socialist_swing = mean(socialist_swing_1904_1912, na.rm = TRUE),
      median_socialist_swing = median(socialist_swing_1904_1912, na.rm = TRUE),
      mean_adjusted_bank_dep_exposure = mean(adjusted_bank_dep_exposure, na.rm = TRUE),
      median_adjusted_bank_dep_exposure = median(adjusted_bank_dep_exposure, na.rm = TRUE),
      mean_log_adjusted_bank_dep_exposure = mean(log_adjusted_bank_dep_exposure, na.rm = TRUE),
      sd_log_adjusted_bank_dep_exposure = sd(log_adjusted_bank_dep_exposure, na.rm = TRUE),
      share_with_any_failure = mean(any_failure_1907_1908, na.rm = TRUE),
      mean_pct_urban = mean(pct_urban, na.rm = TRUE)
    )
)

# Preferred outcome:
# change in Socialist vote share from 1904 to 1912.
# Preferred treatment scale:
# standardized log(1 + adjusted bank deposit exposure).
texreg_models <- list(model_1, model_2, model_3, model_4, model_5, model_6)

screenreg(
  texreg_models,
  custom.model.names = c(
    "Std. raw exposure",
    "Std. raw exposure + controls",
    "Std. raw exposure + controls + state FE",
    "Std. raw exposure + addl. controls + state FE",
    "log(1 + exposure) + controls + state FE",
    "Std. log(1 + exposure) + controls + state FE"
  ),
  file = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/tables/socialist_economic_shock.tex",
  custom.coef.map = list(
    "std_adjusted_bank_dep_exposure" = "Std. adjusted dep. exposure",
    "log_adjusted_bank_dep_exposure" = "log(1 + adjusted dep. exposure)",
    "std_log_adjusted_bank_dep_exposure" = "Std. log(1 + adjusted dep. exposure)",
    "pct_urban" = "Percent urban",
    "log_population_1900" = "log population, 1900",
    "pct_manuf" = "Percent manufacturing",
    "pct_agric" = "Percent agriculture",
    "pct_nonwhite" = "Percent nonwhite",
    "x1904_pres_rep_percent" = "Republican vote share, 1904",
    "log_postmaster_1900" = "log postmasters, 1900",
    "log_news_delivery_1900" = "log news delivery, 1900"
  ),
  digits = 3,
  stars = c(0.01, 0.05, 0.1),
  include.adjrs = FALSE,
  include.bic = FALSE,
  include.aic = FALSE,
  include.rmse = FALSE,
  custom.note = "Outcome in all columns: Socialist vote share swing from 1904 to 1912. State fixed-effects models report standard errors clustered by state."
)

