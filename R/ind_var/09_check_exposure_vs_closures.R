## 09 check whether high exposure counties saw more bank closures

rm(list = ls())
library(data.table)
library(dplyr)
library(ggplot2)
library(stargazer)

# inputs ------------------------------------------------------------------

jf <- fread("Data/data_outputs/jaremski_fishback.csv")
exposure <- fread("Data/data_outputs/bank_market_exposure.csv")

# closures measure from Jaremski-Fishback ---------------------------------
# available panel years are 1900 and 1910 in this file

jf_0010 <- jf[year %in% c(1900, 1910), .(
  state_fips,
  county_fips,
  year,
  num_total_banks
)]

jf_wide <- dcast(
  jf_0010,
  state_fips + county_fips ~ year,
  value.var = "num_total_banks"
)

setnames(jf_wide, old = c("1900", "1910"), new = c("banks_1900", "banks_1910"))


jf_wide[, banks_1900 := as.numeric(banks_1900)]
jf_wide[, banks_1910 := as.numeric(banks_1910)]

jf_wide[, banks_closed_0010 := pmax(banks_1900 - banks_1910, 0)]
jf_wide[, closure_rate_0010 := fifelse(banks_1900 > 0, banks_closed_0010 / banks_1900, NA_real_)]

# merge with exposure ------------------------------------------------------

exposure[, from_NHGISST := as.numeric(from_NHGISST)]
exposure[, from_NHGISCTY := as.numeric(from_NHGISCTY)]

df <- merge(
  jf_wide,
  exposure,
  by.x = c("state_fips", "county_fips"),
  by.y = c("from_NHGISST", "from_NHGISCTY"),
  all = FALSE
)

df[, ln_bank_circ_exposure := log(bank_circ_exposure + 1)]
df[, ln_bank_cap_exposure := log(bank_cap_exposure + 1)]
df[, ln_bank_dep_exposure := log(bank_dep_exposure + 1)]
df[, ln_bank_assets_exposure := log(bank_assets_exposure + 1)]

write.csv(df, "Data/data_outputs/exposure_vs_closures_1900_1910.csv", row.names = FALSE)

# correlation check --------------------------------------------------------

rho_closed <- cor(df$ln_bank_circ_exposure, df$banks_closed_0010, use = "complete.obs", method = "spearman")
rho_rate <- cor(df$ln_bank_circ_exposure, df$closure_rate_0010, use = "complete.obs", method = "spearman")

cor_table <- data.frame(
  metric = c("Spearman corr: ln circ exposure vs banks closed", "Spearman corr: ln circ exposure vs closure rate"),
  value = c(rho_closed, rho_rate)
)

stargazer(
  cor_table,
  summary = FALSE,
  rownames = FALSE,
  header = FALSE,
  title = "Exposure-Closure Correlations (1900 to 1910)",
  out = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/appendix_tables/exposure_closure_correlations_1900_1910.tex"
)

# scatter: closures count --------------------------------------------------

p <- ggplot(df, aes(x = ln_bank_circ_exposure, y = banks_closed_0010)) +
  geom_point(alpha = 0.35, size = 0.9, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red3", linewidth = 0.8) +
  theme_bw() +
  labs(
    x = "Log circulation exposure",
    y = "Banks closed (1900 to 1910)",
    title = ""
  )

ggsave(
  plot = p,
  filename = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures/exposure_vs_banks_closed_1900_1910.pdf",
  device = pdf, bg = "white", width = 6, height = 4.29, units = "in"
)

# scatter: closure rate ----------------------------------------------------

p <- ggplot(df, aes(x = ln_bank_circ_exposure, y = closure_rate_0010)) +
  geom_point(alpha = 0.35, size = 0.9, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red3", linewidth = 0.8) +
  theme_bw() +
  labs(
    x = "Log circulation exposure",
    y = "Closure rate (closed / banks in 1900)",
    title = ""
  )

ggsave(
  plot = p,
  filename = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures/exposure_vs_closure_rate_1900_1910.pdf",
  device = pdf, bg = "white", width = 6, height = 4.29, units = "in"
)

# binned means by exposure decile -----------------------------------------

df_deciles <- df %>%
  mutate(
    exposure_decile = ntile(ln_bank_circ_exposure, 10)
  ) %>%
  group_by(exposure_decile) %>%
  summarise(
    mean_ln_bank_circ_exposure = mean(ln_bank_circ_exposure, na.rm = TRUE),
    mean_banks_closed_0010 = mean(banks_closed_0010, na.rm = TRUE),
    mean_closure_rate_0010 = mean(closure_rate_0010, na.rm = TRUE),
    n_counties = n(),
    .groups = "drop"
  )

write.csv(df_deciles, "Data/data_outputs/exposure_closure_deciles_1900_1910.csv", row.names = FALSE)

stargazer(
  as.data.frame(df_deciles),
  summary = FALSE,
  rownames = FALSE,
  header = FALSE,
  title = "Banks Closed by Exposure Decile (1900 to 1910)",
  out = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/appendix_tables/exposure_closure_deciles_1900_1910.tex"
)
