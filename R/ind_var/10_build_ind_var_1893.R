## Standalone exploration script: build 1893 bank-failure exposure using 1900 rail

rm(list = ls())

library(sf)
library(tidyverse)
library(data.table)
library(igraph)
library(stringr)
library(purrr)
library(tidygeocoder)
library(maps)
library(ggplot2)

crisis_years <- 1893
crisis_year_suffix <- paste(crisis_years, collapse = "_")
rail_year <- 1900
theta <- 3
epsilon <- 1e-6

full_panel_path <- "Data/data_outputs/receiverships_panel_full.csv"
county_shapefile_path <- "Data/Shapefiles/nhgis0001_shapefile_tl2008_us_county_1900/US_county_1900_conflated.shp"
network_dir <- "Data/NetworkDatabase_HR_2021"

receiverships_output_path <- file.path("Data/data_outputs", paste0("receiverships_panel_", crisis_year_suffix, ".csv"))
county_failures_output_path <- file.path("Data/data_outputs", paste0("county_level_", crisis_year_suffix, "_failures.csv"))
transport_costs_output_path <- file.path("Data/data_outputs", paste0("all_transportation_costs_rail_", rail_year, ".csv"))
exposure_output_path <- file.path("Data/data_outputs", paste0("bank_market_exposure_", crisis_year_suffix, "_rail_", rail_year, ".csv"))
summary_output_path <- file.path("Data/data_outputs", paste0("bank_market_exposure_", crisis_year_suffix, "_rail_", rail_year, "_summary.csv"))
map_output_path <- file.path("Data/data_outputs", paste0("bank_dep_exposure_", crisis_year_suffix, "_rail_", rail_year, "_map.pdf"))

state_lookup <- c(
  setNames(state.abb, gsub("[^a-z]", "", tolower(state.name))),
  setNames(state.abb, gsub("[^a-z]", "", tolower(state.abb))),
  ala = "AL", ariz = "AZ", ark = "AR", cal = "CA", colo = "CO",
  conn = "CT", dak = "SD", del = "DE", fla = "FL", ga = "GA",
  ill = "IL", ind = "IN", kans = "KS", ky = "KY", la = "LA",
  mass = "MA", md = "MD", mich = "MI", minn = "MN", miss = "MS",
  mo = "MO", mont = "MT", neb = "NE", nev = "NV", nh = "NH",
  nj = "NJ", nm = "NM", ny = "NY", nc = "NC", nd = "ND",
  okla = "OK", oreg = "OR", pa = "PA", penna = "PA", penn = "PA",
  ri = "RI", sc = "SC", sd = "SD", tenn = "TN", tex = "TX",
  va = "VA", vt = "VT", wash = "WA", wva = "WV", wis = "WI",
  wyo = "WY"
)

offline_geocode <- function(df) {
  us_city_lookup <- maps::us.cities %>%
    mutate(
      city_name = sub(" [A-Z]{2}$", "", name),
      city_key = gsub("[^a-z]", "", tolower(city_name)),
      state_abb = country.etc
    ) %>%
    transmute(city_key, state_abb, longitude = long, latitude = lat) %>%
    bind_rows(
      tibble(city_key = "brooklyn", state_abb = "NY", longitude = -73.9442, latitude = 40.6782),
      tibble(city_key = "newyork", state_abb = "NY", longitude = -74.0060, latitude = 40.7128)
    ) %>%
    distinct(city_key, state_abb, .keep_all = TRUE)
  
  df %>%
    mutate(
      state_key = gsub("[^a-z]", "", tolower(state_clean)),
      state_abb = unname(state_lookup[state_key]),
      city_key = gsub("[^a-z]", "", tolower(city))
    ) %>%
    left_join(us_city_lookup, by = c("city_key", "state_abb")) %>%
    filter(!is.na(latitude), !is.na(longitude))
}

build_transport_costs <- function(sf_counties, network_dir, rail_year, output_path) {
  message("Building transport costs for rail year ", rail_year, "...")
  
  shp_paths <- list.files(
    network_dir,
    pattern = paste0("Component_.*_", rail_year, "\\.shp$"),
    recursive = TRUE,
    full.names = TRUE
  )
  shp_components <- lapply(shp_paths, st_read, quiet = TRUE)
  target_crs <- st_crs(shp_components[[1]])
  shp_components <- lapply(shp_components, st_transform, crs = target_crs)
  
  shp_all_years <- list.files(
    network_dir,
    pattern = "Component_.*_allyears\\.shp$",
    recursive = TRUE,
    full.names = TRUE
  )
  sf_all_components <- lapply(shp_all_years, st_read, quiet = TRUE)
  sf_all_components <- lapply(sf_all_components, st_transform, crs = target_crs)
  
  sf_all <- bind_rows(shp_components, sf_all_components, .id = "source_file")
  sf_all$length_miles <- as.numeric(st_length(sf_all)) / 1609.344
  
  edges_sf <- sf_all %>% mutate(edge_id = row_number())
  coords <- st_coordinates(edges_sf) %>% data.table()
  edges_dt <- data.table(edges_sf)
  
  coords[, n_group := .N, by = .(L1)]
  coords[, n_index := seq_len(.N), by = .(L1)]
  coords <- coords[!(n_index > 1 & n_index < n_group)]
  coords[, `:=`(n_group = NULL, n_index = NULL)]
  coords[, edge_id := L1]
  coords[, L1 := NULL]
  coords[, `:=`(X = round(X, 4), Y = round(Y, 4))]
  coords[, start_end := rep(c("start", "end"), nrow(coords) / 2)]
  coords[, node_id := as.numeric(as.factor(paste(X, Y)))]
  coords[, node_id := paste0("node_", node_id)]
  
  start_ids <- coords[start_end == "start", .(edge_id, from = node_id)]
  end_ids <- coords[start_end == "end", .(edge_id, to = node_id)]
  nodes_dt <- unique(coords[, .(node_id, X, Y)])
  
  edges_dt <- merge(edges_dt, start_ids, by = "edge_id")
  edges_dt <- merge(edges_dt, end_ids, by = "edge_id")
  
  nodes_df <- as.data.frame(nodes_dt)
  colnames(nodes_df)[1] <- "name"
  
  g <- graph_from_data_frame(
    d = as.data.frame(edges_dt)[, c("from", "to", "length_miles")],
    vertices = nodes_df,
    directed = FALSE
  )
  E(g)$weight <- E(g)$length_miles
  
  sf_nodes <- st_as_sf(nodes_df, coords = c("X", "Y"), crs = st_crs(sf_all))
  comp <- components(g)
  largest_comp <- which.max(comp$csize)
  valid_nodes <- V(g)$name[comp$membership == largest_comp]
  sf_nodes_largest <- sf_nodes[sf_nodes$name %in% valid_nodes, ]
  
  sf_centroids <- st_centroid(sf_counties)
  nearests <- st_nearest_feature(sf_centroids, sf_nodes_largest)
  sf_centroids$node_id <- sf_nodes_largest$name[nearests]
  
  df_centroid <- data.table(sf_centroids)[, .(
    node_id,
    NHGISST = as.numeric(NHGISST),
    NHGISCTY = as.numeric(NHGISCTY)
  )]
  
  centroid_nodes <- unique(df_centroid$node_id)
  dists <- distances(g, v = centroid_nodes, to = centroid_nodes, weights = E(g)$weight)
  dists_long <- as.data.table(as.table(as.matrix(dists)))
  setnames(dists_long, c("from_id", "to_id", "cost"))
  
  dists_long <- merge(
    dists_long,
    df_centroid[, .(to_id = node_id, to_NHGISST = NHGISST, to_NHGISCTY = NHGISCTY)],
    by = "to_id",
    allow.cartesian = TRUE
  )
  
  costs_df <- merge(
    dists_long,
    df_centroid[, .(from_id = node_id, from_NHGISST = NHGISST, from_NHGISCTY = NHGISCTY)],
    by = "from_id",
    allow.cartesian = TRUE
  )
  
  costs_df <- costs_df[is.finite(cost)]
  fwrite(costs_df, output_path)
  costs_df
}

message("Loading existing full panel...")
receiverships_panel_clean <- read.csv(full_panel_path) %>%
  mutate(
    receiver_year = as.numeric(receiver_year),
    receiver_month = as.numeric(receiver_month)
  )

parsed_banks <- receiverships_panel_clean %>%
  mutate(
    parts = str_split(bank_name, ","),
    city_raw = if ("city" %in% names(receiverships_panel_clean)) city else map_chr(parts, ~ if (length(.x) >= 2) str_squish(.x[length(.x) - 1]) else NA_character_),
    state_raw = if ("state_clean" %in% names(receiverships_panel_clean)) state_clean else map_chr(parts, ~ if (length(.x) >= 1) str_squish(.x[length(.x)]) else NA_character_),
    city = if ("city" %in% names(receiverships_panel_clean)) {
      str_squish(as.character(city))
    } else {
      city_raw %>%
        str_replace_all("[^A-Za-z .'-]", "") %>%
        str_squish()
    },
    state_clean = if ("state_clean" %in% names(receiverships_panel_clean)) {
      str_squish(as.character(state_clean))
    } else {
      state_raw %>%
        str_replace_all("[^A-Za-z.]", "") %>%
        str_squish() %>%
        str_to_title()
    }
  )

receiverships_1893 <- parsed_banks %>%
  filter(receiver_year %in% crisis_years)

write.csv(
  receiverships_1893 %>% select(!c(parts, city_raw, state_raw)),
  receiverships_output_path,
  row.names = FALSE
)

message("Geocoding 1893 bank failures...")
geo_1893 <- tryCatch(
  {
    receiverships_1893 %>%
      mutate(query = paste(city, state_clean, sep = ", ")) %>%
      geocode(
        address = query,
        method = "arcgis",
        lat = latitude,
        long = longitude
      ) %>%
      filter(!is.na(latitude), !is.na(longitude))
  },
  error = function(e) {
    message("ArcGIS geocoding unavailable; using offline city/state matching.")
    offline_geocode(receiverships_1893)
  }
)

sf_1900 <- read_sf(county_shapefile_path) %>%
  mutate(
    ICPSRST = as.numeric(ICPSRST),
    ICPSRCTY = as.numeric(ICPSRCTY),
    NHGISST = as.numeric(NHGISST),
    NHGISCTY = as.numeric(NHGISCTY)
  ) %>%
  filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory"))

sf_1900_simple <- st_simplify(sf_1900, dTolerance = 1000, preserveTopology = TRUE)

geo_1893_sf <- geo_1893 %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(2163)

sf_1900_simple_2163 <- st_transform(sf_1900_simple, 2163)

county_failures_1893 <- st_join(geo_1893_sf, sf_1900_simple_2163) %>%
  st_drop_geometry() %>%
  select(!c(parts, city_raw, state_raw))

write.csv(county_failures_1893, county_failures_output_path, row.names = FALSE)

if (file.exists(transport_costs_output_path)) {
  message("Using cached transport costs: ", transport_costs_output_path)
  costs_df <- fread(transport_costs_output_path)
} else if (rail_year == 1900 && file.exists("Data/data_outputs/all_transportation_costs.csv")) {
  message("Copying existing 1900 transport costs into a 1893-specific filename.")
  costs_df <- fread("Data/data_outputs/all_transportation_costs.csv")
  fwrite(costs_df, transport_costs_output_path)
} else {
  costs_df <- build_transport_costs(sf_1900_simple, network_dir, rail_year, transport_costs_output_path)
}

setDT(costs_df)
banks_dt <- as.data.table(county_failures_1893)

max_circ <- max(banks_dt$circulation_at_failure, na.rm = TRUE)
if (!is.finite(max_circ) || max_circ == 0) max_circ <- 1

max_cap <- max(banks_dt$failure_capital, na.rm = TRUE)
if (!is.finite(max_cap) || max_cap == 0) max_cap <- 1

max_dep <- max(banks_dt$deposits_at_suspension, na.rm = TRUE)
if (!is.finite(max_dep) || max_dep == 0) max_dep <- 1

max_assets <- max(banks_dt$total_assets, na.rm = TRUE)
if (!is.finite(max_assets) || max_assets == 0) max_assets <- 1

banks_dt[, w_circ := circulation_at_failure / max_circ]
banks_dt[, w_cap := failure_capital / max_cap]
banks_dt[, w_dep := deposits_at_suspension / max_dep]
banks_dt[, w_assets := total_assets / max_assets]

bank_w_by_county <- banks_dt[
  ,
  .(
    w_circ_county = sum(w_circ, na.rm = TRUE),
    w_cap_county = sum(w_cap, na.rm = TRUE),
    w_dep_county = sum(w_dep, na.rm = TRUE),
    w_assets_county = sum(w_assets, na.rm = TRUE)
  ),
  by = .(
    to_NHGISST = as.numeric(NHGISST),
    to_NHGISCTY = as.numeric(NHGISCTY)
  )
]

cc_costs <- costs_df[, .(from_NHGISST, from_NHGISCTY, to_NHGISST, to_NHGISCTY, cost)]
cc_costs <- merge(
  cc_costs,
  bank_w_by_county,
  by = c("to_NHGISST", "to_NHGISCTY"),
  all.x = TRUE
)

cc_costs[is.na(w_circ_county), w_circ_county := 0]
cc_costs[is.na(w_cap_county), w_cap_county := 0]
cc_costs[is.na(w_dep_county), w_dep_county := 0]
cc_costs[is.na(w_assets_county), w_assets_county := 0]

cc_costs[, contrib_circ := w_circ_county * ((cost + epsilon)^(-theta))]
cc_costs[, contrib_cap := w_cap_county * ((cost + epsilon)^(-theta))]
cc_costs[, contrib_dep := w_dep_county * ((cost + epsilon)^(-theta))]
cc_costs[, contrib_assets := w_assets_county * ((cost + epsilon)^(-theta))]

exposure_df <- cc_costs[
  ,
  .(
    bank_circ_exposure = sum(contrib_circ, na.rm = TRUE),
    bank_cap_exposure = sum(contrib_cap, na.rm = TRUE),
    bank_dep_exposure = sum(contrib_dep, na.rm = TRUE),
    bank_assets_exposure = sum(contrib_assets, na.rm = TRUE)
  ),
  by = .(from_NHGISST, from_NHGISCTY)
]

exposure_df[, ln_bank_circ_exposure := log(bank_circ_exposure + 1)]
exposure_df[, ln_bank_cap_exposure := log(bank_cap_exposure + 1)]
exposure_df[, ln_bank_dep_exposure := log(bank_dep_exposure + 1)]
exposure_df[, ln_bank_assets_exposure := log(bank_assets_exposure + 1)]

write.csv(exposure_df, exposure_output_path, row.names = FALSE)

summary_df <- tibble(
  metric = c(
    "num_1893_failures",
    "num_geocoded_1893_failures",
    "num_failed_counties_1893",
    "num_counties_with_positive_dep_exposure",
    "max_dep_exposure",
    "median_dep_exposure",
    "max_ln_dep_exposure"
  ),
  value = c(
    nrow(receiverships_1893),
    nrow(geo_1893),
    nrow(bank_w_by_county),
    sum(exposure_df$bank_dep_exposure > 0, na.rm = TRUE),
    max(exposure_df$bank_dep_exposure, na.rm = TRUE),
    median(exposure_df$bank_dep_exposure, na.rm = TRUE),
    max(exposure_df$ln_bank_dep_exposure, na.rm = TRUE)
  )
)

write.csv(summary_df, summary_output_path, row.names = FALSE)

dep_map_sf <- exposure_df %>%
  left_join(
    sf_1900_simple %>%
      st_drop_geometry() %>%
      select(NHGISST, NHGISCTY),
    by = c("from_NHGISST" = "NHGISST", "from_NHGISCTY" = "NHGISCTY")
  ) %>%
  left_join(
    sf_1900_simple,
    by = c("from_NHGISST" = "NHGISST", "from_NHGISCTY" = "NHGISCTY")
  ) %>%
  st_as_sf()

quantiles <- quantile(dep_map_sf$ln_bank_dep_exposure, probs = seq(0, 1, 0.1), na.rm = TRUE)
dep_map_sf <- dep_map_sf %>%
  mutate(dep_bin = cut(ln_bank_dep_exposure, breaks = unique(quantiles), include.lowest = TRUE))

custom_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(11)

ggplot() +
  geom_sf(data = dep_map_sf, aes(fill = dep_bin), color = NA, linewidth = 0.1) +
  coord_sf(crs = 2163) +
  scale_fill_manual(values = custom_palette, name = "Log dep exposure") +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom"
  ) +
  labs(title = "1893 deposit exposure with 1900 rail network")

# ggsave(map_output_path, plot = p, device = pdf, bg = "white", width = 6, height = 4.29, units = "in")
