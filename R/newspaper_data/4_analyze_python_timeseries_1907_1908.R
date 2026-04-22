library(arrow)
library(dplyr)
library(ggplot2)
library(glue)
library(lubridate)
library(purrr)
library(readr)
library(tidyr)

rm(list = ls())

input_files <- c(
  "Python/data/data_outputs/american_stories_filtered_1907/american_stories_1907_filtered.parquet",
  "Python/data/data_outputs/american_stories_filtered_1908/american_stories_1908_filtered.parquet", 
  "Python/data/data_outputs/american_stories_filtered_1909/american_stories_1909_filtered.parquet",
  "Python/data/data_outputs/american_stories_filtered_1910/american_stories_1910_filtered.parquet"
  
)

monthly_hits <- map_dfr(input_files, read_parquet) %>%
  mutate(
    date = as.Date(date),
    month = floor_date(date, unit = "month")
  ) %>%
  filter(date >= as.Date("1907-01-01"),
         date <= as.Date("1909-12-31")) %>%
  count(month, wt = n_matches, name = "monthly_hits") %>%
  complete(
    month = seq(as.Date("1907-01-01"), as.Date("1909-12-01"), by = "month"),
    fill = list(monthly_hits = 0)
  ) %>%
  arrange(month)

panic_start <- as.Date("1907-10-01")
panic_end <- as.Date("1908-01-31")

p <- ggplot(monthly_hits, aes(x = month, y = monthly_hits)) +
  annotate(
    "rect",
    xmin = panic_start,
    xmax = panic_end,
    ymin = -Inf,
    ymax = Inf,
    fill = "grey80",
    alpha = 0.4
  ) +
  geom_line(linewidth = 0.8, color = "black") +
  geom_point(size = 1.8, color = "black") +
  scale_x_date(
    breaks = seq(as.Date("1907-01-01"), as.Date("1910-12-01"), by = "3 months"),
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  labs(
    x = NULL,
    y = "Monthly count of hits",
    title = ""
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

ggsave(
  filename = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures/panic_articles_1907_1908.pdf",
  plot = p, device = "pdf", bg = "white", width = 6.5, height = 4,
  units = "in"
)
