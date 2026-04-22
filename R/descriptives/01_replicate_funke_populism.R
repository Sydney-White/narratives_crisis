#### Funke et al paper -- replicate literature 

rm(list = ls())
library(haven)
library(tidyverse)
library(stargazer)
library(fixest)
library(texreg)

# -------------------------------------------------------------------------

funke_data <- read_dta("Data/data_inputs/EER-D-16-00187_replication/EER-D-16-00187_data.dta")
colnames(funke_data)
unique(funke_data$iso)

df <- funke_data %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(
    logright    = log1p(right),
    loggovvote  = log1p(govvote),
    logoppvote  = log1p(oppvote),
    logfrac     = log1p(frac),
    lpartycount = log1p(partycount),
    dlrgdp      = rgdp - lag(rgdp),
    dlcpi       = cpi - lag(cpi)
  ) %>%
  ungroup()

df_exclude_war <- df %>%
  filter(year >= 1919 & year <= 2014,
         !(year >= 1939 & year <= 1949)) %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(
    logright = log1p(right),
    dlrgdp   = rgdp - lag(rgdp),
    dlcpi    = cpi - lag(cpi)
  ) %>%
  ungroup()

horizons <- 1:5
for (h in horizons) {
  df_exclude_war[[paste0("dlogright", h)]] <-
    100 * (lead(df_exclude_war$logright, h) - df_exclude_war$logright)
}

lp_models <- lapply(horizons, function(h) {
  feols(
    as.formula(paste0(
      "dlogright", h,
      " ~ pk_fin + pk_norm + pk_dis + dlrgdp + dlcpi +
        lag(dlrgdp,1) + lag(dlcpi,1) +
        logright + lag(logright,1) | ccode"
    )),
    data = df_exclude_war,
    cluster = "ccode"
  )
})

texreg(
  lp_models,
  custom.model.names = paste("Year", 1:5),
  custom.coef.map = list(
    "pk_fin"  = "Financial recession",
    "pk_norm" = "Normal recession",
    "pk_dis"  = "Non-fin. macro shock"
  ),
  stars = c(0.01, 0.05, 0.1),
  digits = 2,
  booktabs = TRUE,
  use.packages = FALSE,
  caption = "Local Projections: Cumulative Change in Far-Right Vote Share (Excluding WWII)",
  label = "tab:lp_far_right",
  include.rsquared = TRUE,
  include.nobs = TRUE
)

# what about heterogeneity?  ----------------------------------------------

p <- ggplot(df_exclude_war, aes(x = dlogright5)) +
  geom_histogram(binwidth = 10, color = "white", fill = "steelblue") +
  geom_vline(aes(xintercept = mean(dlogright5, na.rm = TRUE)),
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "",
    x = "Δ log(right vote share, 5-year horizon)",
    y = "Number of crisis observations"
  ) +
  theme_minimal(base_size = 13)

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/Competing Narratives of Financial Crises/figures/distribution_logrshare.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

## add negative table 

df_exclude_war <- df_exclude_war %>%
  mutate(
    negative_far_right = ifelse(pk_fin == 1 & dlogright5 < 0, 1, 0)
  )

negative_table <- df_exclude_war %>%
  filter(negative_far_right == 1) %>%
  select(iso, year, dlogright5) %>%
  arrange(year) %>% 
  mutate(dlogright5 = round(dlogright5, 2))

stargazer(
  negative_table,
  summary = FALSE,
  title = "Cases with Negative Far-Right Response After Financial Crisis Excludes WWII. 
  Otherwise between 1919-2014, as in their first table.",
  label = "tab:negative_far_right",
  rownames = FALSE,
  digits = 2)