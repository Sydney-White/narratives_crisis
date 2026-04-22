rm(list = ls())

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(sf)

sf_use_s2(FALSE)

input_path <- "Data/data_outputs/newspapers/chronicling_merge/chronicling_to_gentzkow_final_matches.csv"
shape_path <- "Data/Shapefiles/nhgis0001_shapefile_tl2008_us_county_1900/US_county_1900_conflated.shp"
output_dir <- "Data/data_outputs/newspapers/descriptives"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

teal <- "#2A7F7F"
gray <- "#595959"
light_teal <- "#A9D3D0"

clean_geo_component <- function(x) {
  suppressWarnings(as.numeric(str_squish(as.character(x))))
}

newspapers_raw <- read_csv(input_path, show_col_types = FALSE)

if (!"adprice" %in% names(newspapers_raw)) {
  newspapers_raw$adprice <- NA_real_
}

if (!"circ" %in% names(newspapers_raw)) {
  newspapers_raw$circ <- NA_real_
}

newspapers <- newspapers_raw %>%
  mutate(
    Number_of_Issues_num = suppressWarnings(as.numeric(`Number of Issues`)),
    subprice = suppressWarnings(as.numeric(subprice)),
    adprice = suppressWarnings(as.numeric(adprice)),
    circ = suppressWarnings(as.numeric(circ))
  ) %>%
  arrange(desc(final_match_type == "exact"), match_score) %>%
  group_by(LCCN) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(
    match_group = factor(
      final_match_type,
      levels = c("exact", "fuzzy_high", "fuzzy_medium")
    )
  )

summary_counts <- tibble(
  matched_lccn = n_distinct(newspapers$LCCN),
  exact_matches = sum(newspapers$final_match_type == "exact", na.rm = TRUE),
  fuzzy_high_matches = sum(newspapers$final_match_type == "fuzzy_high", na.rm = TRUE),
  fuzzy_medium_matches = sum(newspapers$final_match_type == "fuzzy_medium", na.rm = TRUE),
  states_covered = n_distinct(newspapers$State),
  counties_reported = n_distinct(paste(newspapers$State, newspapers$County)),
  with_subprice = sum(!is.na(newspapers$subprice)),
  with_adprice = sum(!is.na(newspapers$adprice)),
  with_circulation = sum(!is.na(newspapers$circ)),
  with_partisanship = sum(!is.na(newspapers$political_label)),
  with_party_endorsement = sum(!is.na(newspapers$party_endorsement_label))
)

numeric_summary <- tibble(
  variable = c("subprice", "adprice", "circulation", "number_of_issues"),
  non_missing = c(
    sum(!is.na(newspapers$subprice)),
    sum(!is.na(newspapers$adprice)),
    sum(!is.na(newspapers$circ)),
    sum(!is.na(newspapers$Number_of_Issues_num))
  ),
  mean = c(
    mean(newspapers$subprice, na.rm = TRUE),
    mean(newspapers$adprice, na.rm = TRUE),
    mean(newspapers$circ, na.rm = TRUE),
    mean(newspapers$Number_of_Issues_num, na.rm = TRUE)
  ),
  median = c(
    median(newspapers$subprice, na.rm = TRUE),
    median(newspapers$adprice, na.rm = TRUE),
    median(newspapers$circ, na.rm = TRUE),
    median(newspapers$Number_of_Issues_num, na.rm = TRUE)
  ),
  p90 = c(
    quantile(newspapers$subprice, 0.9, na.rm = TRUE),
    quantile(newspapers$adprice, 0.9, na.rm = TRUE),
    quantile(newspapers$circ, 0.9, na.rm = TRUE),
    quantile(newspapers$Number_of_Issues_num, 0.9, na.rm = TRUE)
  )
) %>%
  mutate(across(c(mean, median, p90), ~ ifelse(is.nan(.x), NA_real_, .x)))

partisan_summary <- newspapers %>%
  mutate(political_label = replace_na(political_label, "Missing")) %>%
  count(political_label, sort = TRUE, name = "n") %>%
  mutate(share = n / sum(n))

state_summary <- newspapers %>%
  count(State, sort = TRUE, name = "matched_newspapers")

sf_1900 <- read_sf(shape_path) %>%
  filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory")) %>%
  st_transform(4326)

newspapers_geo <- newspapers %>%
  separate(`Geo Location`,
           into = c("latitude", "longitude"),
           sep = ",",
           remove = FALSE,
           fill = "right") %>%
  mutate(
    latitude = clean_geo_component(latitude),
    longitude = clean_geo_component(longitude)
  ) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

newspapers_with_county <- st_join(
  newspapers_geo,
  sf_1900 %>%
    select(NHGISST, NHGISCTY, STATENAM, NHGISNAM),
  join = st_within,
  left = TRUE
)

county_coverage <- newspapers_with_county %>%
  st_drop_geometry() %>%
  filter(!is.na(NHGISST), !is.na(NHGISCTY)) %>%
  count(NHGISST, NHGISCTY, name = "matched_newspapers") %>%
  mutate(
    coverage_bin = cut(
      matched_newspapers,
      breaks = c(0, 1, 2, 5, 10, Inf),
      labels = c("1", "2", "3-5", "6-10", "11+"),
      include.lowest = TRUE
    )
  )

county_map_df <- sf_1900 %>%
  left_join(county_coverage, by = c("NHGISST", "NHGISCTY"))

coverage_map <- ggplot() +
  geom_sf(data = county_map_df, fill = "grey95", color = "white", linewidth = 0.05) +
  geom_sf(
    data = county_map_df %>% filter(!is.na(coverage_bin)),
    aes(fill = coverage_bin),
    color = NA
  ) +
  coord_sf(crs = 2163) +
  scale_fill_manual(
    values = c("1" = light_teal, "2" = "#7FB8B3", "3-5" = teal, "6-10" = "#1E5E5E", "11+" = "#103636"),
    name = "Matched newspapers"
  ) +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "Matched newspaper coverage by 1900 county")

subprice_plot <- newspapers %>%
  filter(!is.na(subprice)) %>%
  ggplot(aes(x = subprice)) +
  geom_histogram(fill = teal, color = "white", bins = 30) +
  theme_bw() +
  labs(x = "Annual subscription price", y = "Count", title = "Distribution of subscription prices")

circulation_plot <- newspapers %>%
  filter(!is.na(circ), circ > 0) %>%
  ggplot(aes(x = circ)) +
  geom_histogram(fill = gray, color = "white", bins = 30) +
  scale_x_log10() +
  theme_bw() +
  labs(x = "Circulation (log scale)", y = "Count", title = "Distribution of circulation")

partisan_plot <- partisan_summary %>%
  mutate(political_label = factor(political_label, levels = rev(political_label))) %>%
  ggplot(aes(x = political_label, y = n)) +
  geom_col(fill = teal) +
  coord_flip() +
  theme_bw() +
  labs(x = "", y = "Matched newspapers", title = "Partisan affiliation in matched sample")

price_circulation_plot <- newspapers %>%
  filter(!is.na(subprice), !is.na(circ), circ > 0) %>%
  ggplot(aes(x = circ, y = subprice, color = match_group)) +
  geom_point(alpha = 0.65, size = 1.7) +
  scale_x_log10() +
  scale_color_manual(values = c("exact" = teal, "fuzzy_high" = gray, "fuzzy_medium" = light_teal), na.translate = FALSE) +
  theme_bw() +
  labs(
    x = "Circulation (log scale)",
    y = "Annual subscription price",
    color = "Match type",
    title = "Subscription prices and circulation"
  )

ggsave(file.path(output_dir, "matched_newspaper_county_coverage_map.pdf"),
       plot = coverage_map, device = pdf, bg = "white", width = 8, height = 5.5, units = "in")
ggsave(file.path(output_dir, "subscription_price_distribution.pdf"),
       plot = subprice_plot, device = pdf, bg = "white", width = 6, height = 4, units = "in")
ggsave(file.path(output_dir, "circulation_distribution.pdf"),
       plot = circulation_plot, device = pdf, bg = "white", width = 6, height = 4, units = "in")
ggsave(file.path(output_dir, "partisan_affiliation_distribution.pdf"),
       plot = partisan_plot, device = pdf, bg = "white", width = 6.5, height = 4.5, units = "in")
ggsave(file.path(output_dir, "subscription_price_vs_circulation.pdf"),
       plot = price_circulation_plot, device = pdf, bg = "white", width = 6.5, height = 4.5, units = "in")

cat("Wrote newspaper descriptives to", output_dir, "\n")
cat("Matched LCCN:", n_distinct(newspapers$LCCN), "\n")
print(summary_counts)
print(numeric_summary)
