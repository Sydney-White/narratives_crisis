## 07 population change vs deposit exposure

rm(list = ls())
library(readr)
library(dplyr)
library(ggplot2)
library(fixest)

section_teal <- "#2A7F7F"
section_gray <- "#595959"

figure_dir <- path.expand("~/Dropbox/Apps/Overleaf/1907 Bankers Panic/appendix_figures")
table_dir <- path.expand("~/Dropbox/Apps/Overleaf/1907 Bankers Panic/appendix_tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

plot_path <- file.path(figure_dir, "population_growth_vs_deposit_exposure.pdf")
table_path <- file.path(table_dir, "population_growth_vs_deposit_exposure.tex")
data_path <- "Data/data_outputs/population_growth_vs_deposit_exposure.csv"

bank_exposure <- read_csv("Data/data_outputs/bank_market_exposure.csv", show_col_types = FALSE)

census_wide <- read_csv("Data/data_inputs/all_census_normal.csv", show_col_types = FALSE) %>%
  filter(year %in% c(1900, 1910)) %>%
  transmute(
    year,
    state_1890,
    county_1890,
    population = coalesce(population, ipums_population),
    pct_urban,
    pct_manuf,
    pct_agric
  ) %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = c(population, pct_urban, pct_manuf, pct_agric),
    names_sep = "_"
  )

analysis_df <- bank_exposure %>%
  left_join(
    census_wide,
    by = c("from_NHGISST" = "state_1890", "from_NHGISCTY" = "county_1890")
  ) %>%
  mutate(
    pop_change = population_1910 - population_1900,
    pop_growth_pct = 100 * (population_1910 - population_1900) / population_1900,
    ln_pop_growth = log(population_1910 / population_1900),
    ln_pop_1900 = log(population_1900),
    ln_dep_std = as.numeric(scale(ln_bank_dep_exposure))
  ) %>%
  filter(
    is.finite(ln_dep_std),
    is.finite(ln_pop_growth),
    is.finite(ln_pop_1900)
  )

write_csv(analysis_df, data_path)

m1 <- feols(
  ln_pop_growth ~ ln_dep_std,
  cluster = ~ from_NHGISST,
  data = analysis_df
)

m2 <- feols(
  ln_pop_growth ~ ln_dep_std + ln_pop_1900 + pct_urban_1900 + pct_manuf_1900 + pct_agric_1900,
  cluster = ~ from_NHGISST,
  data = analysis_df
)

etable_tex <- capture.output(
  etable(
    m1, m2,
    tex = TRUE,
    digits = 3,
    fitstat = ~ n + r2,
    dict = c(
      ln_dep_std = "Deposit exposure (std., log)",
      ln_pop_1900 = "Log population, 1900",
      pct_urban_1900 = "Percent urban, 1900",
      pct_manuf_1900 = "Percent manufacturing, 1900",
      pct_agric_1900 = "Percent agriculture, 1900"
    ),
    title = "Deposit Exposure and County Population Growth, 1900--1910",
    label = "tab:population_growth_vs_deposit_exposure"
  )
)

writeLines(etable_tex, table_path)

ggplot(analysis_df, aes(x = ln_dep_std, y = ln_pop_growth)) +
  geom_hline(yintercept = 0, color = section_gray, linewidth = 0.4, linetype = "dashed") +
  geom_point(color = section_teal, alpha = 0.3, size = 1.1) +
  geom_smooth(method = "lm", se = TRUE, color = section_gray, fill = "#BFD9D9", linewidth = 0.8) +
  labs(
    x = "Deposit exposure (standardized log measure)",
    y = "Log population growth, 1900 to 1910",
    title = "County Population Growth and Exposure to Failed-Bank Deposits"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

ggsave(
  filename = plot_path,
  plot = p,
  device = pdf,
  bg = "white",
  width = 6.5,
  height = 4.5,
  units = "in"
)

print(summary(m1))
print(summary(m2))

cor(analysis_df$ln_dep_std, analysis_df$ln_pop_growth)
