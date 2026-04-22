## 07 trace one-bank adjusted deposit exposure

rm(list = ls())
library(sf)
library(dplyr)
library(data.table)
library(ggplot2)

selected_failure_id <- 484
theta <- 3
epsilon <- 1e-6

section_teal <- "#2A7F7F"
section_gray <- "#595959"
overleaf_dir <- path.expand("~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures")
dir.create(overleaf_dir, recursive = TRUE, showWarnings = FALSE)

# read data ---------------------------------------------------------------

banks_dt <- fread("Data/data_outputs/county_level_1907_failures.csv")
costs_df <- fread("Data/data_outputs/all_transportation_costs.csv")
bank_counts_1900 <- fread("Data/data_outputs/jaremski_fishback.csv")[
  year == 1900,
  .(
    state_fips = as.numeric(state_fips),
    county_fips = as.numeric(county_fips),
    num_total_banks_1900 = as.numeric(num_total_banks)
  )
]

sf_1900 <- read_sf(
  "Data/Shapefiles/nhgis0001_shapefile_tl2008_us_county_1900/US_county_1900_conflated.shp"
) %>%
  mutate(
    NHGISST = as.numeric(NHGISST),
    NHGISCTY = as.numeric(NHGISCTY),
    ICPSRST = as.numeric(ICPSRST),
    ICPSRCTY = as.numeric(ICPSRCTY)
  ) %>%
  filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory"))

county_crosswalk <- sf_1900 %>%
  st_drop_geometry() %>%
  transmute(
    ICPSRST,
    ICPSRCTY,
    to_NHGISST = NHGISST,
    to_NHGISCTY = NHGISCTY
  ) %>%
  filter(!is.na(ICPSRST), !is.na(ICPSRCTY)) %>%
  distinct()

bank_counts_1900 <- bank_counts_1900 %>%
  left_join(
    county_crosswalk,
    by = c("state_fips" = "ICPSRST", "county_fips" = "ICPSRCTY")
  ) %>%
  filter(!is.na(to_NHGISST), !is.na(to_NHGISCTY)) %>%
  select(to_NHGISST, to_NHGISCTY, num_total_banks_1900)

banks_dt[, deposits_at_suspension := as.numeric(deposits_at_suspension)]
max_dep <- max(banks_dt$deposits_at_suspension, na.rm = TRUE)
if (!is.finite(max_dep) || max_dep == 0) max_dep <- 1
banks_dt[, w_dep := deposits_at_suspension / max_dep]

selected_bank <- banks_dt[failure_id == selected_failure_id][1]
if (nrow(selected_bank) == 0) stop("Selected bank not found.")

# construct selected-bank contribution surface ---------------------------

selected_costs <- costs_df[
  to_NHGISST == as.numeric(selected_bank$NHGISST) &
    to_NHGISCTY == as.numeric(selected_bank$NHGISCTY),
  .(from_NHGISST, from_NHGISCTY, cost)
]

selected_trace <- selected_costs %>%
  left_join(
    bank_counts_1900,
    by = c("from_NHGISST" = "to_NHGISST", "from_NHGISCTY" = "to_NHGISCTY")
  ) %>%
  mutate(
    num_total_banks_1900 = coalesce(num_total_banks_1900, 0),
    selected_bank_dep_contrib_raw = selected_bank$w_dep * ((cost + epsilon)^(-theta)),
    selected_bank_dep_contrib_adj = if_else(
      num_total_banks_1900 > 0,
      selected_bank_dep_contrib_raw / num_total_banks_1900,
      NA_real_
    )
  )

county_labels <- sf_1900 %>%
  st_drop_geometry() %>%
  select(
    NHGISST,
    NHGISCTY,
    NHGISNAM,
    STATENAM
  ) %>%
  distinct()

selected_trace <- selected_trace %>%
  left_join(
    county_labels,
    by = c("from_NHGISST" = "NHGISST", "from_NHGISCTY" = "NHGISCTY")
  ) %>%
  arrange(desc(selected_bank_dep_contrib_adj)) %>%
  mutate(rank_adj = row_number())

# fwrite(
#   selected_trace,
#   "Data/data_outputs/trace_one_bank_adjusted_deposit_exposure.csv"
# )

top_exposed_counties <- selected_trace %>%
  filter(is.finite(selected_bank_dep_contrib_adj)) %>%
  select(
    rank_adj,
    STATENAM,
    NHGISNAM,
    from_NHGISST,
    from_NHGISCTY,
    cost,
    num_total_banks_1900,
    selected_bank_dep_contrib_raw,
    selected_bank_dep_contrib_adj
  ) %>%
  slice(1:20)

# write.csv(
#   top_exposed_counties,
#   "Data/data_outputs/trace_one_bank_adjusted_deposit_exposure_top20.csv",
#   row.names = FALSE
# )

# map adjusted contribution -----------------------------------------------

trace_sf <- selected_trace %>%
  left_join(
    sf_1900,
    by = c("from_NHGISST" = "NHGISST", "from_NHGISCTY" = "NHGISCTY")
  ) %>%
  st_as_sf()

selected_bank_county <- sf_1900 %>%
  filter(
    NHGISST == as.numeric(selected_bank$NHGISST),
    NHGISCTY == as.numeric(selected_bank$NHGISCTY)
  )

quantiles <- quantile(
  trace_sf$selected_bank_dep_contrib_adj,
  probs = seq(0, 1, 0.1),
  na.rm = TRUE
)

trace_map <- trace_sf %>%
  mutate(
    dep_bin = cut(
      selected_bank_dep_contrib_adj,
      breaks = unique(quantiles),
      include.lowest = TRUE
    )
  ) %>%
  st_as_sf()

ggplot() +
  geom_sf(data = trace_map, aes(fill = dep_bin), color = NA, linewidth = 0.05) +
  geom_sf(data = selected_bank_county, fill = NA, color = section_gray, linewidth = 0.35) +
  scale_fill_manual(
    values = colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(length(levels(trace_map$dep_bin))),
    na.value = "grey95",
    name = ""
  ) +
  guides(fill = guide_legend(title = "Adj. one-bank deposit exposure", nrow = 3)) +
  coord_sf(crs = 2163) +
  labs(
    title = paste0("Adjusted Deposit Exposure from One Failed Bank: ", selected_bank$bank_name)
  ) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(face = "bold", size = 11),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.position = "bottom"
  )

ggsave(
  plot = p,
  filename = file.path(overleaf_dir, "trace_one_bank_adjusted_deposit_exposure_map.pdf"),
  device = pdf,
  bg = "white",
  width = 6.5,
  height = 4.6,
  units = "in"
)

print(selected_bank[, .(
  failure_id,
  bank_name,
  city,
  STATENAM,
  NHGISST,
  NHGISCTY,
  deposits_at_suspension,
  w_dep
)])

print(top_exposed_counties)
