### explore NYC-region bank failures with transportation network (Correia et al.)

rm(list = ls())
library(sf)
library(dplyr)
library(ggplot2)
library(haven)
library(stringr)
library(maps)

# NYC-region states (expand/shrink as needed)
region_states <- c(
  "New York", "New Jersey", "Connecticut",
  "Pennsylvania", "Massachusetts", "Rhode Island"
)

# outputs
out_dir <- "~/Dropbox/Apps/Overleaf/New Independent Var/nyc"
dir.create(path.expand(out_dir), recursive = TRUE, showWarnings = FALSE)

data_output_path <- file.path(path.expand(out_dir), "nyc_region_bank_transport_1907_1908.csv")

# read map/network data ---------------------------------------------------

sf_1900 <- read_sf(
  "Data/Shapefiles/nhgis0001_shapefile_tl2008_us_county_1900/US_county_1900_conflated.shp"
) %>%
  mutate(
    NHGISST = as.numeric(NHGISST),
    NHGISCTY = as.numeric(NHGISCTY)
  ) %>%
  filter(STATENAM %in% region_states)

sf_1900 <- st_simplify(sf_1900, dTolerance = 1000, preserveTopology = TRUE)

rail_1900 <- read_sf("Data/NetworkDatabase_HR_2021/1900/Component_6_1900.shp")
rail_1900 <- st_transform(rail_1900, st_crs(sf_1900))

region_bbox <- st_as_sfc(st_bbox(sf_1900))
rail_1900_region <- st_intersection(rail_1900, region_bbox)

# read Correia failures, parse year/city/state ----------------------------

state_lookup <- c(
  setNames(state.abb, gsub("[^a-z]", "", tolower(state.name))),
  setNames(state.abb, gsub("[^a-z]", "", tolower(state.abb))),
  ala = "AL", ariz = "AZ", ark = "AR", cal = "CA", colo = "CO",
  conn = "CT", del = "DE", fla = "FL", ga = "GA", ill = "IL",
  ind = "IN", kans = "KS", ky = "KY", la = "LA", mass = "MA",
  md = "MD", mich = "MI", minn = "MN", miss = "MS", mo = "MO",
  mont = "MT", neb = "NE", nev = "NV", nh = "NH", nj = "NJ",
  nm = "NM", ny = "NY", nc = "NC", nd = "ND", okla = "OK",
  oreg = "OR", pa = "PA", penna = "PA", penn = "PA", ri = "RI",
  sc = "SC", sd = "SD", dak = "SD", tenn = "TN", tex = "TX",
  va = "VA", vt = "VT", wash = "WA", wva = "WV", wis = "WI", wyo = "WY"
)

banks_1907_1908 <- read_dta("Data/data_inputs/qje-repkit-to-upload/sources/occ-receiverships/receiverships_panel.dta") %>%
  mutate(
    receiver_appt_year = sub(".*((18|19|20)[0-9OISl]{2}).*", "\\1", as.character(date_receiver_appt)),
    receiver_appt_year = ifelse(grepl("^(18|19|20)[0-9OISl]{2}$", receiver_appt_year), receiver_appt_year, NA_character_),
    receiver_appt_year = gsub("O", "0", receiver_appt_year),
    receiver_appt_year = gsub("I", "1", receiver_appt_year),
    receiver_appt_year = gsub("l", "1", receiver_appt_year),
    receiver_appt_year = gsub("S", "5", receiver_appt_year),
    receiver_appt_year = suppressWarnings(as.integer(receiver_appt_year)),
    closed_year = sub(".*((18|19|20)[0-9OISl]{2}).*", "\\1", as.character(date_closed)),
    closed_year = ifelse(grepl("^(18|19|20)[0-9OISl]{2}$", closed_year), closed_year, NA_character_),
    closed_year = gsub("O", "0", closed_year),
    closed_year = gsub("I", "1", closed_year),
    closed_year = gsub("l", "1", closed_year),
    closed_year = gsub("S", "5", closed_year),
    closed_year = suppressWarnings(as.integer(closed_year)),
    failure_year = ifelse(!is.na(receiver_appt_year), receiver_appt_year, closed_year),
    Capital_at_failure = as.numeric(failure_capital),
    Circulation_outstanding_at_failure = as.numeric(circulation_at_failure)
  ) %>%
  filter(failure_year %in% c(1907, 1908)) %>%
  distinct(failure_id, bank_name, failure_year, Capital_at_failure, Circulation_outstanding_at_failure) %>%
  mutate(
    bank_name_clean = str_replace_all(bank_name, "\\.+$", ""),
    bank_name_clean = str_replace_all(bank_name_clean, "K1", "Y"),
    parts = str_split(bank_name_clean, ","),
    n_parts = lengths(parts),
    first_part = sapply(parts, function(v) if (length(v) >= 1) str_squish(v[1]) else NA_character_),
    city_second_last = sapply(parts, function(v) if (length(v) >= 3) str_squish(v[length(v) - 1]) else NA_character_),
    state_raw = sapply(parts, function(v) if (length(v) >= 2) str_squish(v[length(v)]) else NA_character_),
    city_from_in = str_squish(str_match(first_part, ".* in ([A-Za-z .-]+)$")[, 2]),
    city_tail = str_squish(str_match(first_part, "([A-Z][A-Za-z-]+(?:\\s+[A-Z][A-Za-z-]+){0,2})$")[, 2]),
    city = ifelse(
      n_parts >= 3,
      city_second_last,
      ifelse(
        n_parts == 2 & str_detect(first_part, " in "),
        city_from_in,
        ifelse(n_parts == 2, city_tail, NA_character_)
      )
    ),
    city = str_squish(gsub("[^A-Za-z -]", "", city)),
    state_key = gsub("[^a-z]", "", tolower(state_raw)),
    state_abb = unname(state_lookup[state_key]),
    State = state.name[match(state_abb, state.abb)],
    city_key = gsub("[^a-z]", "", tolower(city))
  ) %>%
  select(-parts, -n_parts, -bank_name_clean, -first_part, -city_second_last, -city_from_in, -city_tail) %>%
  filter(State %in% region_states)

# join city/state to coordinates using us.cities (no digitized input) -----

us_city_lookup <- maps::us.cities %>%
  mutate(
    city_name = sub(" [A-Z]{2}$", "", name),
    city_key = gsub("[^a-z]", "", tolower(city_name)),
    state_abb = country.etc
  ) %>%
  transmute(city_key, state_abb, long, lat) %>%
  bind_rows(
    tibble(city_key = "brooklyn", state_abb = "NY", long = -73.9442, lat = 40.6782),
    tibble(city_key = "newyork", state_abb = "NY", long = -74.0060, lat = 40.7128)
  ) %>%
  distinct(city_key, state_abb, .keep_all = TRUE)

banks <- banks %>%
  left_join(us_city_lookup, by = c("city_key", "state_abb")) %>%
  filter(!is.na(long), !is.na(lat))

banks_sf <- st_as_sf(banks, coords = c("long", "lat"), crs = 4326, remove = FALSE)
banks_sf <- st_transform(banks_sf, st_crs(sf_1900))

# distance to nearest rail segment ----------------------------------------

nearest_rail_idx <- st_nearest_feature(banks_sf, rail_1900_region)

banks_sf$dist_to_rail_km <- as.numeric(
  st_distance(banks_sf, rail_1900_region[nearest_rail_idx, ], by_element = TRUE)
) / 1000

# distance to NYC center point (Manhattan reference) ----------------------

nyc_point <- st_sfc(st_point(c(-74.0060, 40.7128)), crs = 4326)
nyc_point <- st_transform(nyc_point, st_crs(banks_sf))

banks_sf$dist_to_nyc_km <- as.numeric(st_distance(banks_sf, nyc_point)) / 1000

# write table -------------------------------------------------------------

banks_export <- banks_sf %>%
  st_drop_geometry() %>%
  select(
    failure_id, State, city, bank_name, failure_year,
    Capital_at_failure,
    Circulation_outstanding_at_failure,
    dist_to_rail_km,
    dist_to_nyc_km,
    state_abb
  )

write.csv(banks_export, data_output_path, row.names = FALSE)

# map: banks + rail network -----------------------------------------------

ggplot() +
  geom_sf(data = sf_1900, fill = "white", color = "grey80", linewidth = 0.1) +
  geom_sf(data = rail_1900_region, color = "grey45", linewidth = 0.2, alpha = 0.8) +
  geom_sf(
    data = banks_sf,
    aes(size = Capital_at_failure, color = dist_to_rail_km),
    alpha = 0.75
  ) +
  scale_size_continuous(name = "Capital at failure", range = c(1.5, 8)) +
  scale_color_viridis_c(name = "Distance to nearest rail (km)") +
  coord_sf() +
  theme_bw() +
  labs(
    x = "",
    y = "",
    title = "NYC Region and Rail Network"
  ) +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  plot = p_map,
  filename = file.path(path.expand(out_dir), "nyc_region_banks_rail_map.pdf"),
  device = pdf,
  bg = "white",
  width = 7.0,
  height = 4.8,
  units = "in"
)

# scatter: distance to NYC vs distance to rail ----------------------------

p_scatter <- ggplot(
  banks_sf %>% st_drop_geometry(),
  aes(x = dist_to_nyc_km, y = dist_to_rail_km)
) +
  geom_point(aes(size = Capital_at_failure), alpha = 0.7, color = "#1E90FF") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +
  scale_size_continuous(name = "Capital at failure", range = c(1.5, 7)) +
  theme_bw() +
  labs(
    x = "Distance to NYC (km)",
    y = "Distance to nearest rail (km)",
    title = "Correia 1907-1908: NYC Distance Structure"
  ) +
  theme(legend.position = "bottom")

ggsave(
  plot = p_scatter,
  filename = file.path(path.expand(out_dir), "nyc_region_distance_scatter.pdf"),
  device = pdf,
  bg = "white",
  width = 6.8,
  height = 4.6,
  units = "in"
)
