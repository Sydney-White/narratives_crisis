##### replicate results from FUnke 

rm(list = ls())
library(haven)
library(slider)
library(tidyverse)
library(stargazer)
library(fixest)
library(texreg)

# -------------------------------------------------------------------------

funke_data <- read_dta("EER-D-16-00187_replication/EER-D-16-00187_data.dta")
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

output_path <- "~/Dropbox/Apps/Overleaf/Competing Narratives of Financial Crises/tables/table_lp_far_right.tex"

texreg(
  lp_models,
  file = output_path,
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
  digits = 2, 
  out = "~/Dropbox/Apps/Overleaf/Competing Narratives of Financial Crises/tables/negative_responses.tex"
)
 
# # Generate war dummy
# funke_data <- funke_data %>%
#   mutate(warx = if_else((year >= 1914 & year <= 1918) | (year >= 1939 & year <= 1949), 1, 0))
# 
# funke_data <- funke_data %>%
#   arrange(ccode, year) %>%
#   group_by(ccode) %>%
#   mutate(
#     near_war = slide_index_dbl(warx, year, ~ max(.x, na.rm = TRUE), 
#                                .before = 5, .after = 5))
# 
# funke_data <- funke_data %>% 
#   mutate( crisis_f1 = crisisJST )
# 
# funke_data <- funke_data %>%
#   arrange(ccode, year) %>%
#   group_by(ccode) %>%
#   mutate(
#     prepost = case_when(
#       slide_index_lgl(crisis_f1, year, ~ any(.x == 1), .before = 5, .after = 1) ~ "Post-crisis",
#       slide_index_lgl(crisis_f1, year, ~ any(.x == 1), .before = 1, .after = 5) ~ "Pre-crisis",
#       TRUE ~ NA_character_
#     )
#   ) %>%
#   ungroup()
# 
# plot_data <- funke_data %>%
#   filter(!is.na(prepost)) ## lose a lot of data 
# 
# plot_data <- plot_data %>% 
#   group_by(prepost) %>%
#   summarise(
#     `Far-left (Avg.)` = mean(left, na.rm = TRUE),
#     `Far-right (Avg.)` = mean(right, na.rm = TRUE),
#     `Total (Avg.)` = mean(extr, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   pivot_longer(cols = -prepost, names_to = "group", values_to = "value") %>% 
#   mutate(prepost = factor(prepost, levels = c("Pre-crisis", "Post-crisis")))
# 
# p <- ggplot(plot_data, aes(x = prepost, y = value, fill = group)) +
#   geom_col(position = "dodge", color = "black") +
#   scale_fill_manual(values = c("white", "black", "grey70")) +
#   labs(
#     x = NULL,
#     y = "Percentage of total votes",
#     fill = "group"
#   ) +
#   theme_minimal()
# 
# ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/Competing Narratives/figures/motivating_empirics.pdf",
#        device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")
# 
# # repeat but without war years -------------------------------------------------------------------------
# 
# funke_data <- read_dta("Dropbox/Columbia/Spring 2025/Game Theory/R/EER-D-16-00187_replication/EER-D-16-00187_data.dta")
# funke_data$iso
# # Generate war dummy
# funke_data <- funke_data %>%
#   mutate(warx = if_else((year >= 1914 & year <= 1918) | (year >= 1939 & year <= 1949), 1, 0))
# 
# funke_data <- funke_data %>%
#   arrange(ccode, year) %>%
#   group_by(ccode) %>%
#   mutate(
#     near_war = slide_index_dbl(warx, year, ~ max(.x, na.rm = TRUE), 
#                                .before = 5, .after = 5))
# 
# funke_data <- funke_data %>% 
#   mutate( crisis_f1 = crisisJST )
# 
# funke_data <- funke_data %>%
#   arrange(ccode, year) %>%
#   group_by(ccode) %>%
#   mutate(
#     prepost = case_when(
#       slide_index_lgl(crisis_f1, year, ~ any(.x == 1), .before = 5, .after = 1) ~ "Post-crisis",
#       slide_index_lgl(crisis_f1, year, ~ any(.x == 1), .before = 1, .after = 5) ~ "Pre-crisis",
#       TRUE ~ NA_character_
#     )
#   ) %>%
#   ungroup()
# 
# plot_data <- funke_data %>%
#   filter(!is.na(prepost)) ## lose a lot of data 
# 
# plot_data_wo_war <- plot_data %>% 
#   filter(warx != 1) %>% 
#   group_by(prepost) %>%
#   summarise(
#     `Far-left (Avg.)` = mean(left, na.rm = TRUE),
#     `Far-right (Avg.)` = mean(right, na.rm = TRUE),
#     `Total (Avg.)` = mean(extr, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   pivot_longer(cols = -prepost, names_to = "group", values_to = "value") %>% 
#   mutate(prepost = factor(prepost, levels = c("Pre-crisis", "Post-crisis")))
# 
# p <- ggplot(plot_data_wo_war, aes(x = prepost, y = value, fill = group)) +
#   geom_col(position = "dodge", color = "black") +
#   scale_fill_manual(values = c("white", "black", "grey70")) +
#   labs(
#     x = NULL,
#     y = "Percentage of total votes",
#     fill = "group"
#   ) +
#   theme_minimal()
# 
# ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/Competing Narratives/figures/funke_without_war.pdf",
#        device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")
