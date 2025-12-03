#### funke 2024 populist leaders 

rm(list = ls())
library(haven)
library(slider)
library(tidyverse)
library(stargazer)

# -------------------------------------------------------------------------

PLE_panel2 <- read_dta("~/Dropbox/Columbia/Spring 2025/Game Theory/R/EER-D-16-00187_replication/PLE_panel2.dta")
colnames(PLE_panel2)

populist_by_year <- PLE_panel2 %>%
  group_by(year) %>%
  summarise(
    Total = sum(pop, na.rm = TRUE),
    Left = sum(lpop, na.rm = TRUE),
    Right = sum(rpop, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Total, Left, Right), names_to = "Type", values_to = "Count")

p <- ggplot(populist_by_year, aes(x = year, y = Count, color = Type)) +
  geom_line(linewidth = 0.5) +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "gray30", linewidth = 0.6) +
  labs(
    title = "",
    x = "",
    y = "Number of Countries with Populist Leaders",
    color = "Populist Type", 
    family = "serif"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c(
      "Total" = "#333333",  
      "Left" = "#1b9e77", 
      "Right" = "#d95f02"    
    )
  )

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/Competing Narratives of Financial Crises/figures/populist_time_series.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

unique(PLE_panel2$country)

# make stargazer table ----------------------------------------------------

# Get sorted country list and split into two columns
country_list <- sort(unique(PLE_panel2$country))
n <- length(country_list)
half_n <- ceiling(n / 2)

country_table <- data.frame(
  "list_1" = country_list[1:half_n],
  "list_2" = c(country_list[(half_n + 1):n], rep(NA, half_n - (n - half_n)))
)

stargazer(country_table,
          type = "latex",
          label = "tab:country_list", 
          summary = FALSE,
          title = "Countries in PLE Dataset \\citep{funke2023populist}",
          out = "~/Dropbox/Apps/Overleaf/Competing Narratives of Financial Crises/country_list.tex")
