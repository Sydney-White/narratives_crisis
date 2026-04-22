rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

input_path <- "Data/data_inputs/JSTdatasetR6.xlsx"

jst_data <- read_excel(input_path)

us_1900_1920 <- jst_data %>%
  filter(iso == "USA", year >= 1900, year <= 1914) %>%
  arrange(year) %>%
  mutate(
    wage_growth = 100 * ((wage / lag(wage)) - 1)
  )

plot_df <- us_1900_1920 %>%
  select(year, exports, imports, gdp, wage_growth) %>%
  pivot_longer(
    cols = -year,
    names_to = "variable",
    values_to = "value"
  ) %>%
  arrange(year)

panel_labels <- c(
  exports = "Exports",
  imports = "Imports",
  gdp = "GDP",
  wage_growth = "Wage Growth (%)"
)

plot_df$variable <- factor(
  plot_df$variable,
  levels = c("exports", "imports", "gdp", "wage_growth"),
  labels = panel_labels[c("exports", "imports", "gdp", "wage_growth")]
)

four_panel_plot <- ggplot(plot_df, aes(x = year, y = value)) +
  geom_line(color = "black", linewidth = 0.8, na.rm = TRUE) +
  geom_point(color = "black", size = 1.6, na.rm = TRUE) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(1900, 1920, by = 2)) +
  labs(
    x = "",
    y = NULL,
    title = ""
  ) +
  theme_minimal(base_size = 12)

print(four_panel_plot)

ggsave(plot = four_panel_plot, filename = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/appendix_figures/economic_indicators.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

