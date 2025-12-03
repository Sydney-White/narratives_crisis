#### run preliminary regressions 

rm(list = ls())
library(haven)
library(tidyverse)
library(stargazer)

# look at data ------------------------------------------------------------

funke_data <- read_dta("~/Dropbox/Columbia/Spring 2025/Game Theory/R/EER-D-16-00187_replication/EER-D-16-00187_data.dta")
colnames(funke_data)
funke_data$rgdp

funke_data$crisisJST

funke_data <- funke_data %>%
  group_by(iso) %>%
  arrange(year) %>%
  mutate(rgdp_growth = 100 * (rgdp / lag(rgdp) - 1))

# funke_data <- funke_data %>%
#   group_by(iso) %>%
#   mutate(log_rgdp = log(rgdp),
#          log_rgdp_dev = log_rgdp - mean(log_rgdp, na.rm = TRUE))
# # 
# funke_data <- funke_data %>%
#   group_by(country) %>%
#   arrange(year) %>%
#   mutate(rgdp_growth = 100 * (rgdp / lag(rgdp) - 1))

avg_growth <- funke_data %>%
  group_by(crisisJST) %>%
  summarise(mean_growth = mean(rgdp_growth, na.rm = TRUE))

ggplot(avg_growth, aes(x = factor(crisisJST), y = mean_growth, 
                       fill = factor(crisisJST))) +
  geom_bar(stat = "identity") +
  labs(
    x = "Banking Crisis",
    y = "Average Real GDP Growth (%)",
    title = "Average Real GDP Growth by Crisis Status"
  ) +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "#333333"), guide = "none") +
  theme_minimal()

summary(lm(right ~ rgdp_growth, data = funke_data %>% filter(crisisJST == 1)))

ggplot(funke_data %>% filter(crisisJST == 1), 
       aes(x = rgdp_growth, y = right)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    x = "Real GDP Growth (Crisis Years Only)",
    y = "Right Vote Share (%)",
    title = "Right-Wing Support and GDP Growth During Banking Crises"
  ) +
  theme_minimal()

lm(right ~ rgdp_growth * crisisJST, data = funke_data)


