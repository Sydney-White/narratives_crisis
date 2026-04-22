## isolate Chronicling America newspapers active during 1900-1915

rm(list = ls())
library(tidyverse)
library(lubridate)
library(sf)
library(units)

window_start <- as.Date("1900-01-01")
window_end <- as.Date("1920-12-31")
input_path <- "Data/data_inputs/chronicling-america.csv"
coverage_map_path <- "Data/data_outputs/chronicling_america_1900_1915_county_coverage_map.pdf"
buffer_map_path <- "Data/data_outputs/chronicling_america_1900_1915_100mi_buffer_map.pdf"

parse_chronicling_date <- function(x) {
  x <- str_squish(as.character(x))
  x[x %in% c("", "NA")] <- NA_character_

  out <- rep(as.Date(NA), length(x))

  iso_idx <- !is.na(x) & str_detect(x, "^\\d{4}-\\d{2}-\\d{2}$")
  out[iso_idx] <- ymd(x[iso_idx], quiet = TRUE)

  slash4_idx <- !is.na(x) & str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{4}$")
  out[slash4_idx] <- mdy(x[slash4_idx], quiet = TRUE)

  slash2_idx <- !is.na(x) & str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{2}$")
  if (any(slash2_idx)) {
    parts <- str_split(x[slash2_idx], "/", simplify = TRUE)
    month <- suppressWarnings(as.integer(parts[, 1]))
    day <- suppressWarnings(as.integer(parts[, 2]))
    year2 <- suppressWarnings(as.integer(parts[, 3]))

    # Chronicling America here is historical; 00-30 are 1900s, larger values are 1800s.
    year_full <- ifelse(year2 <= 30, 1900 + year2, 1800 + year2)
    out[slash2_idx] <- make_date(year = year_full, month = month, day = day)
  }

  out
}

extract_issue_year <- function(x) {
  x <- str_squish(as.character(x))
  x[x %in% c("", "NA")] <- NA_character_

  out <- rep(NA_integer_, length(x))

  iso_idx <- !is.na(x) & str_detect(x, "^\\d{4}-\\d{2}-\\d{2}$")
  out[iso_idx] <- suppressWarnings(as.integer(str_sub(x[iso_idx], 1, 4)))

  slash4_idx <- !is.na(x) & str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{4}$")
  out[slash4_idx] <- suppressWarnings(as.integer(str_sub(x[slash4_idx], -4, -1)))

  slash2_idx <- !is.na(x) & str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{2}$")
  if (any(slash2_idx)) {
    year2 <- suppressWarnings(as.integer(str_sub(x[slash2_idx], -2, -1)))
    out[slash2_idx] <- ifelse(year2 <= 30, 1900 + year2, 1800 + year2)
  }

  out
}

clean_name_token <- function(x) {
  x %>%
    str_replace_all("\\[|\\]", "") %>%
    str_replace_all("\\s+", " ") %>%
    str_squish() %>%
    str_replace_all("\\bSt[.]\\b", "Saint") %>%
    str_replace_all("\\bSte[.]\\b", "Sainte") %>%
    na_if("")
}

parse_geo_component <- function(x) {
  suppressWarnings(as.numeric(str_squish(as.character(x))))
}

chronicling_america <- read_csv(input_path, show_col_types = FALSE)
colnames(chronicling_america)
chronicling_america_clean <- chronicling_america %>%
  mutate(
    first_issue_raw = `First Issue`,
    last_issue_raw = `Last Issue`,
    first_issue_date = parse_chronicling_date(`First Issue`),
    last_issue_date = parse_chronicling_date(`Last Issue`),
    first_issue_year = extract_issue_year(`First Issue`),
    last_issue_year = extract_issue_year(`Last Issue`)
  ) %>%
  mutate(
    overlaps_1900_1915 = case_when(
      !is.na(first_issue_date) & !is.na(last_issue_date) ~
        first_issue_date <= window_end & last_issue_date >= window_start,
      is.na(first_issue_date) & !is.na(last_issue_date) ~
        last_issue_date >= window_start,
      !is.na(first_issue_date) & is.na(last_issue_date) ~
        first_issue_date <= window_end,
      !is.na(first_issue_year) & !is.na(last_issue_year) ~
        first_issue_year <= year(window_end) & last_issue_year >= year(window_start),
      is.na(first_issue_year) & !is.na(last_issue_year) ~
        last_issue_year >= year(window_start),
      !is.na(first_issue_year) & is.na(last_issue_year) ~
        first_issue_year <= year(window_end),
      TRUE ~ FALSE
    )
  )

chronicling_america_1900_1915 <- chronicling_america_clean %>%
  filter(overlaps_1900_1915) %>%
  arrange(first_issue_date, last_issue_date, Newspapers)

sf_1900 <- read_sf("Data/Shapefiles/nhgis0001_shapefile_tl2008_us_county_1900/US_county_1900_conflated.shp") %>%
  filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory")) %>%
  st_simplify(dTolerance = 1000, preserveTopology = TRUE)

coverage_county_expanded <- chronicling_america_1900_1915 %>%
  mutate(
    state_join = str_squish(State),
    county_join = County
  ) %>%
  separate_longer_delim(county_join, delim = ",") %>%
  mutate(county_join = clean_name_token(county_join)) %>%
  filter(!is.na(state_join), !is.na(county_join), county_join != "") %>%
  distinct(LCCN, Newspapers, state_join, county_join)

coverage_county <- coverage_county_expanded %>%
  count(state_join, county_join, name = "n_newspapers")

coverage_sf <- sf_1900 %>%
  left_join(
    coverage_county,
    by = c("STATENAM" = "state_join", "NHGISNAM" = "county_join")
  ) %>%
  mutate(n_newspapers = replace_na(n_newspapers, 0L))

coverage_plot <- coverage_sf %>%
  filter(n_newspapers > 0) %>%
  mutate(
    bin = cut(
      n_newspapers,
      breaks = c(0, 1, 2, 5, 10, Inf),
      include.lowest = TRUE,
      labels = c("1", "2", "3-5", "6-10", "11+")
    )
  )

sum(coverage_sf$n_newspapers > 0, na.rm = TRUE) # 760 counties with at least one newspaper 

p <- ggplot() +
  geom_sf(data = coverage_sf, fill = "grey95", color = "white", linewidth = 0.05) +
  geom_sf(data = coverage_plot, aes(fill = bin), color = NA, linewidth = 0.05) +
  coord_sf(crs = 2163) +
  scale_fill_brewer(palette = "PuBuGn", name = "Newspapers") +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "Chronicling America newspaper county coverage, 1900-1920")

ggsave(
  coverage_map_path,
  plot = p,
  device = pdf,
  bg = "white",
  width = 6,
  height = 4.29,
  units = "in"
)

cat("Total newspapers in source:", nrow(chronicling_america), "\n")
cat("Newspapers overlapping 1900-1920:", nrow(chronicling_america_1900_1915), "\n")

print(
  chronicling_america_1900_1915 %>%
    select(Newspapers, LCCN, `First Issue`, `Last Issue`, first_issue_date, last_issue_date) %>%
    slice_head(n = 10)
)

cat("\nRows with unparsed issue dates but retained via year fallback:", "\n")
print(
  chronicling_america_1900_1915 %>%
    filter((is.na(first_issue_date) & !is.na(first_issue_year)) |
             (is.na(last_issue_date) & !is.na(last_issue_year))) %>%
    select(Newspapers, `First Issue`, `Last Issue`, first_issue_year, last_issue_year) %>%
    slice_head(n = 10)
)

# look at 100mi radius around newspaper places ----------------------------

newspaper_places <- chronicling_america_1900_1915 %>%
  separate(`Geo Location`,
           into = c("latitude", "longitude"),
           sep = ",",
           remove = FALSE,
           fill = "right") %>%
  mutate(
    latitude = parse_geo_component(latitude),
    longitude = parse_geo_component(longitude)
  ) %>%
  filter(!State %in% c("Alaska", "Hawaii")) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  distinct(City, State, latitude, longitude, .keep_all = TRUE) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(2163)

buffer_100mi <- newspaper_places %>%
  st_buffer(dist = set_units(50, "miles")) %>%
  summarise(
    n_places = n(),
    geometry = st_union(geometry)
  )

ggplot() +
  geom_sf(data = st_transform(sf_1900, 2163),
          fill = "grey95", color = "white", linewidth = 0.05) +
  geom_sf(
    data = buffer_100mi,
    fill = "#2b8cbe", color = NA, alpha = 0.35
  ) +
  geom_sf(
    data = newspaper_places,
    color = "#08519c", fill = "white", shape = 21, size = 0.5, stroke = 0.15
  ) +
  coord_sf(crs = 2163) +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"
  ) 

ggsave(
  buffer_map_path,
  plot = p,
  device = pdf,
  bg = "white",
  width = 6,
  height = 4.29,
  units = "in"
)

cat("Unique newspaper places with coordinates:", nrow(newspaper_places), "\n")
