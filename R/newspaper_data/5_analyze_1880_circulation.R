library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(tidyverse)

rm(list = ls())

input_path <- "Data/data_inputs/nhgis0056_csv/0056_csv .csv"
output_csv <- "Data/data_outputs/newspaper_circulation_1880_state.csv"
output_plot <- "Data/data_outputs/newspaper_circulation_1880_top_states.pdf"

circulation_1880 <- read_csv(input_path, show_col_types = FALSE) %>%
  transmute(
    year = YEAR,
    state = STATE,
    state_code = STATEA,
    area_name = AREANAME,
    n_newspapers = AQZ001,
    daily_circulation = AQ1001,
    weekly_other_circulation = AQ1002,
    total_circulation = AQ1001 + AQ1002,
    circulation_per_newspaper = if_else(
      AQZ001 > 0,
      (AQ1001 + AQ1002) / AQZ001,
      NA_real_
    )
  ) %>%
  arrange(desc(total_circulation))

plot_df <- circulation_1880 %>%
  slice_head(n = 30) %>%
  mutate(state = factor(state, levels = rev(state)))

ggplot(plot_df, aes(x = state, y = total_circulation)) +
  geom_col(fill = "#2b8cbe", width = 0.75) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    x = NULL,
    y = "Aggregate circulation per issue",
    title = "Top states by newspaper circulation, 1880",
    subtitle = "Total = daily circulation plus weekly and other circulation"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

cat("Top 30 states by total circulation in 1880:\n")
print(
  circulation_1880 %>%
    select(state, n_newspapers, daily_circulation, weekly_other_circulation, total_circulation) %>%
    slice_head(n = 30)
)
