rm(list = ls())

library(readr)
library(dplyr)
library(tibble)
library(sf)
library(ggplot2)
library(RColorBrewer)

input_path <- "Data/data_inputs/dataverse_files (12)/us-post-offices.csv"
shape_path_1900 <- "Data/Shapefiles/nhgis0001_shapefile_tl2008_us_county_1900/US_county_1900_conflated.shp"

custom_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(10)

# Read with explicit column types to avoid parser issues from sparse character fields.
post_offices <- read_csv(
  input_path,
  col_types = cols(
    Name = col_character(),
    AltName = col_character(),
    OrigName = col_character(),
    State = col_character(),
    County1 = col_character(),
    County2 = col_character(),
    County3 = col_character(),
    OrigCounty = col_character(),
    Established = col_double(),
    Discontinued = col_double(),
    Continuous = col_logical(),
    StampIndex = col_character(),
    ID = col_double(),
    Coordinates = col_logical(),
    Duration = col_double(),
    GNIS.Match = col_logical(),
    GNIS.Name = col_character(),
    GNIS.County = col_character(),
    GNIS.State = col_character(),
    GNIS.FEATURE_ID = col_double(),
    GNIS.Feature.Class = col_character(),
    GNIS.OrigName = col_character(),
    GNIS.OrigCounty = col_character(),
    GNIS.Latitude = col_double(),
    GNIS.Longitude = col_double(),
    GNIS.ELEV_IN_M = col_double(),
    GNIS.Dist = col_double(),
    Latitude = col_double(),
    Longitude = col_double()
  )
)

unique(post_offices$GNIS.Feature.Class)

normalize_county_name <- function(x) {
  x %>%
    toupper() %>%
    gsub("[^A-Z0-9 ]", "", .) %>%
    gsub("\\bSAINTE\\b", "SAINTE", .) %>%
    gsub("\\bSAINT\\b", "ST", .) %>%
    gsub("\\bSTE\\b", "SAINTE", .) %>%
    gsub("\\bST\\b", "ST", .) %>%
    gsub("\\s+", " ", .) %>%
    trimws()
}

# Keep records that look like actual post offices and were established by 1920.
# Missing GNIS feature class is allowed because many unmatched offices are still valid.
post_offices_pre_1920 <- post_offices %>%
  mutate(
    established_year = suppressWarnings(as.integer(Established)),
    discontinued_year = suppressWarnings(as.integer(Discontinued)),
    county = County1
  ) %>% 
  filter(
    !is.na(State),
    !is.na(county),
    !is.na(established_year),
    established_year <= 1920,
    # keep either NA or post office 
    is.na(GNIS.Feature.Class) | GNIS.Feature.Class == "Post Office",
    is.na(discontinued_year) | discontinued_year >= established_year
  )

# Narrower subset for offices that plausibly operated at some point between 1800 and 1920.
post_offices_active_1800_1920 <- post_offices_pre_1920 %>%
  filter(is.na(discontinued_year) | discontinued_year >= 1800)

cat("Pre-1920 post office records:", nrow(post_offices_pre_1920), "\n")
cat("Active at some point, 1800-1920:", nrow(post_offices_active_1800_1920), "\n")

years <- 1800:1920

postal_counts_by_year <- tibble(year = years) %>%
  rowwise() %>%
  mutate(
    n_post_offices = sum(
      post_offices_active_1800_1920$established_year <= year &
        (is.na(post_offices_active_1800_1920$discontinued_year) |
           post_offices_active_1800_1920$discontinued_year >= year),
      na.rm = TRUE
    )
  ) %>%
  ungroup()

postal_counts_plot <- ggplot(postal_counts_by_year, aes(x = year, y = n_post_offices)) +
  geom_line(color = "black", linewidth = 0.8, na.rm = TRUE) +
  geom_point(color = "black", size = 1.2, na.rm = TRUE) +
  scale_x_continuous(breaks = seq(1800, 1920, by = 20)) +
  labs(
    x = "Year",
    y = "Active post offices",
    title = "Active US Post Offices, 1800-1920"
  ) +
  theme_minimal(base_size = 12)

state_lookup <- c(setNames(state.name, state.abb), DC = "District of Columbia")
region_lookup <- c(
  setNames(as.character(state.region), state.abb),
  DC = "South",
  AK = "West",
  HI = "West",
  `VAy` = "South",
  `MI/OH` = "North Central"
)

post_offices_active_1800_1920 <- post_offices_active_1800_1920 %>%
  mutate(region = unname(region_lookup[State]))

regions <- c("Northeast", "South", "North Central", "West")

postal_counts_by_year_region <- expand.grid(
  year = years,
  region = regions,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  rowwise() %>%
  mutate(
    n_post_offices = sum(
      post_offices_active_1800_1920$region == region &
        post_offices_active_1800_1920$established_year <= year &
        (is.na(post_offices_active_1800_1920$discontinued_year) |
           post_offices_active_1800_1920$discontinued_year >= year),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  mutate(region = factor(region, levels = regions))

postal_counts_plot_by_region <- ggplot(postal_counts_by_year_region, aes(x = year, y = n_post_offices)) +
  geom_line(color = "black", linewidth = 0.7, na.rm = TRUE) +
  facet_wrap(~ region, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(1800, 1920, by = 20)) +
  labs(
    x = "Year",
    y = "Active post offices",
    title = "Active US Post Offices by Region, 1800-1920"
  ) +
  theme_minimal(base_size = 12)

post_offices_active_1900 <- post_offices_active_1800_1920 %>%
  filter(
    established_year <= 1900,
    is.na(discontinued_year) | discontinued_year >= 1900
  ) %>%
  mutate(
    state_name = unname(state_lookup[State]),
    county_join = toupper(county),
    state_join = toupper(state_name)
  ) %>%
  filter(!is.na(state_name))

post_office_counts_1900 <- post_offices_active_1900 %>%
  count(state_join, county_join, name = "n_post_offices_1900")

counties_1900 <- read_sf(shape_path_1900) %>%
  filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory")) %>%
  mutate(
    state_join = toupper(STATENAM),
    county_join = normalize_county_name(NHGISNAM)
  ) %>%
  st_simplify(dTolerance = 1000, preserveTopology = TRUE)

county_crosswalk_1900 <- counties_1900 %>%
  st_drop_geometry() %>%
  distinct(state_join, county_join, ICPSRST, ICPSRCTY)

post_office_counts_1900 <- post_office_counts_1900 %>%
  mutate(county_join = normalize_county_name(county_join))

unmatched_postal_counties_1900 <- post_office_counts_1900 %>%
  anti_join(county_crosswalk_1900, by = c("state_join", "county_join"))

cat("Matched county counts in 1900:", nrow(post_office_counts_1900) - nrow(unmatched_postal_counties_1900), "\n")
cat("Unmatched county counts in 1900:", nrow(unmatched_postal_counties_1900), "\n")

post_office_map_1900 <- counties_1900 %>%
  left_join(post_office_counts_1900, by = c("state_join", "county_join")) %>%
  mutate(n_post_offices_1900 = if_else(is.na(n_post_offices_1900), 0L, n_post_offices_1900))

post_office_summary_1900 <- tibble(
  min_post_offices = min(post_office_map_1900$n_post_offices_1900, na.rm = TRUE),
  max_post_offices = max(post_office_map_1900$n_post_offices_1900, na.rm = TRUE),
  mean_post_offices = mean(post_office_map_1900$n_post_offices_1900, na.rm = TRUE),
  median_post_offices = median(post_office_map_1900$n_post_offices_1900, na.rm = TRUE)
)

quantiles <- quantile(
  post_office_map_1900$n_post_offices_1900,
  c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
  na.rm = TRUE
)

post_office_map_1900 <- post_office_map_1900 %>%
  mutate(
    var_cut = cut(
      n_post_offices_1900,
      breaks = unique(quantiles),
      include.lowest = TRUE
    )
  )

p <- ggplot(post_office_map_1900) +
  geom_sf(aes(fill = var_cut), color = NA, linewidth = 0.05) +
  scale_fill_manual(values = custom_palette, name = "") +
  coord_sf(crs = 2163) +
  labs(
    title = "Post Offices per County in 1900"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  plot = p,
  filename = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures/post_offices_1900.pdf",
  device = pdf, bg = "white", width = 6, height = 4.29, units = "in"
)
