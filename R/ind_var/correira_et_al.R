## data from correia et al: descriptives from receiverships panel

rm(list = ls())
library(haven)
library(dplyr)
library(ggplot2)
library(stargazer)

receiverships_panel <- read_dta("Data/data_inputs/qje-repkit-to-upload/sources/occ-receiverships/receiverships_panel.dta")

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

receiverships_panel <- receiverships_panel %>%
  mutate(
    receiver_appt_year = extract_year_regex(date_receiver_appt),
    closed_year = extract_year_regex(date_closed),
    receiver_appt_month = extract_month_regex(date_receiver_appt),
    closed_month = extract_month_regex(date_closed),
    receiver_year = ifelse(!is.na(receiver_appt_year), receiver_appt_year, closed_year),
    receiver_month = ifelse(!is.na(receiver_appt_month), receiver_appt_month, closed_month)
  )

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

receiverships_summary <- receiverships_panel_clean %>%
  transmute(
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
    legal_expenses
  )


stargazer(
  as.data.frame(receiverships_summary),
  header = FALSE,
  label = "tab:receiverships_panel_summary",
  title = "Summary Statistics: OCC Receiverships Panel",
  out = "~/Dropbox/Apps/Overleaf/New Independent Var/correia_output/receiverships_panel_summary.tex"
)

cause_counts <- receiverships_panel_clean %>%
  mutate(simplified_cause_of_failure = ifelse(is.na(simplified_cause_of_failure) | trimws(simplified_cause_of_failure) == "", "Unknown", simplified_cause_of_failure)) %>%
  count(simplified_cause_of_failure, name = "n_banks", sort = TRUE)

stargazer(
  as.data.frame(cause_counts),
  summary = FALSE,
  rownames = FALSE,
  header = FALSE,
  label = "tab:receiverships_cause_counts",
  title = "OCC Receiverships by Simplified Cause of Failure",
  out = "~/Dropbox/Apps/Overleaf/New Independent Var/correia_output/receiverships_cause_counts.tex"
)

failures_by_year <- receiverships_panel_clean %>%
  distinct(failure_id, bank_name, receiver_year) %>%
  filter(!is.na(receiver_year)) %>%
  filter(receiver_year >= 1865, receiver_year <= 1914) %>%
  count(receiver_year, name = "n_failures", sort = FALSE)

p <- ggplot(failures_by_year, aes(x = receiver_year, y = n_failures)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(color = "black", size = 0.8) +
  theme_bw() +
  labs(x = "Year", y = "Number of bank failures", title = "")

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/New Independent Var/correia_output/us_bank_failures_by_year.pdf",
       device = pdf, bg = "white", width = 6, height = 4.29, units = "in")

stargazer(
  as.data.frame(failures_by_year),
  summary = FALSE,
  rownames = FALSE,
  header = FALSE,
  label = "tab:receiverships_failures_by_year",
  title = "U.S. Bank Failures by Year (OCC Receiverships)",
  out = "~/Dropbox/Apps/Overleaf/New Independent Var/correia_output/us_bank_failures_by_year.tex"
)
