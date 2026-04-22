### Document starts with DH shapefiles, need to use terminal to unzip .rar file:
# Creates two output data frames:
# 1 is all_transportation_costs.csv
# 2 is the bank exposure csv 

# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(sf, tidyverse, data.table, igraph, tidygraph, readxl)

rm(list = ls())
library(sf)
library(tidyverse)
library(data.table)
library(igraph)
library(tidygraph)
library(readxl)
library(RColorBrewer)
custom_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(10)

# read data ---------------------------------------------------------------

sf_1900 <- read_sf("Data/Shapefiles/nhgis0001_shapefile_tl2008_us_county_1900/US_county_1900_conflated.shp")
base_dir <- "Data/NetworkDatabase_HR_2021/"
shp_paths_1900 <- list.files(base_dir, pattern = "Component_.*_1900\\.shp$", recursive = TRUE, full.names = TRUE)
shp_1900_components <- lapply(shp_paths_1900, st_read, quiet = TRUE)
target_crs <- st_crs(shp_1900_components[[1]])
shp_1900_components <- lapply(shp_1900_components, st_transform, crs = target_crs)

shp_all_years <- list.files(base_dir, pattern = "Component_.*_allyears\\.shp$", recursive = TRUE, full.names = TRUE)
sf_all_components <- lapply(shp_all_years, st_read, quiet = TRUE)
sf_all_components <- lapply(sf_all_components, st_transform, crs = target_crs)

sf_all <- bind_rows(shp_1900_components, sf_all_components, .id = "source_file")

mile <- 1609.344
sf_all$length_miles <- as.numeric(st_length(sf_all)) / mile

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
end_ids   <- coords[start_end == "end",   .(edge_id, to   = node_id)]
nodes_dt  <- unique(coords[, .(node_id, X, Y)])

edges_dt  <- merge(edges_dt, start_ids, by = "edge_id")
edges_dt  <- merge(edges_dt, end_ids,   by = "edge_id")

nodes_df <- nodes_dt %>% as.data.frame()
colnames(nodes_df)[1] <- "name"

edges_df <- edges_dt %>% as.data.frame()
edges_for_graph <- edges_df[, c("from", "to", "length_miles")]

g <- graph_from_data_frame(d = edges_for_graph, vertices = nodes_df, directed = FALSE)
E(g)$weight <- E(g)$length_miles # weights is just the length in miles -- distance 

sf_nodes <- st_as_sf(nodes_df, coords = c("X", "Y"), crs = st_crs(sf_all))

comp <- components(g)
largest_comp <- which.max(comp$csize)
valid_nodes <- V(g)$name[comp$membership == largest_comp]
sf_nodes_largest <- sf_nodes[sf_nodes$name %in% valid_nodes, ]

sf_1900_centroids <- st_centroid(sf_1900)
sf_centroids_temp <- sf_1900_centroids

nearests <- st_nearest_feature(sf_centroids_temp, sf_nodes_largest)
sf_centroids_temp$node_id <- sf_nodes_largest$name[nearests]

df_centroid <- data.table(sf_centroids_temp)[, .(node_id, NHGISST = as.numeric(NHGISST), 
                                                 NHGISCTY = as.numeric(NHGISCTY))]
centroid_nodes <- unique(df_centroid$node_id)

## may take some time to run 
dists <- distances(g, v = centroid_nodes, 
                   to = centroid_nodes, 
                   weights = E(g)$weight)
dists <- as.matrix(dists)

dists_long <- as.data.table(as.table(dists))
setnames(dists_long, c("from_id", "to_id", "cost"))

dists_long <- merge(dists_long,
                    df_centroid[, .(to_id = node_id, to_NHGISST = NHGISST, to_NHGISCTY = NHGISCTY)],
                    by = "to_id",
                    allow.cartesian = TRUE)

costs_df <- merge(dists_long,
                  df_centroid[, .(from_id = node_id, from_NHGISST = NHGISST, from_NHGISCTY = NHGISCTY)],
                  by = "from_id",
                  allow.cartesian = TRUE)

costs_df <- costs_df[is.finite(cost)]
fwrite(costs_df, "Data/data_outputs/all_transportation_costs.csv")

# now utilize bank data (Correia et al., 1907-1908) ----------------------

failed_banks_crisis <- read.csv("Data/data_outputs/county_level_1907_failures.csv")
jf_crosswalk <- sf_1900 %>%
  st_drop_geometry() %>%
  transmute(
    ICPSRST = as.numeric(ICPSRST),
    ICPSRCTY = as.numeric(ICPSRCTY),
    NHGISST = as.numeric(NHGISST),
    NHGISCTY = as.numeric(NHGISCTY)
  ) %>%
  filter(!is.na(ICPSRST), !is.na(ICPSRCTY)) %>%
  distinct()

jf_1900 <- read.csv("Data/data_outputs/jaremski_fishback.csv") %>%
  filter(year == 1900) %>%
  mutate(
    state_fips = as.numeric(state_fips),
    county_fips = as.numeric(county_fips)
  ) %>%
  left_join(
    jf_crosswalk,
    by = c("state_fips" = "ICPSRST", "county_fips" = "ICPSRCTY")
  ) %>%
  transmute(
    from_NHGISST = as.numeric(NHGISST),
    from_NHGISCTY = as.numeric(NHGISCTY),
    num_total_banks_1900 = as.numeric(num_total_banks)
  )

colnames(failed_banks_crisis)
setDT(costs_df)
banks_dt <- as.data.table(failed_banks_crisis)

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

cc_costs <- costs_df[, .(from_NHGISST, from_NHGISCTY,
                         to_NHGISST, to_NHGISCTY, cost)]

head(cc_costs)
cc_costs <- merge(
  cc_costs,
  bank_w_by_county,
  by = c("to_NHGISST", "to_NHGISCTY"),
  all.x = TRUE
)

head(cc_costs)

cc_costs[is.na(w_circ_county), w_circ_county := 0]
cc_costs[is.na(w_cap_county), w_cap_county := 0]
cc_costs[is.na(w_dep_county), w_dep_county := 0]
cc_costs[is.na(w_assets_county), w_assets_county := 0]

head(cc_costs)

theta <- 3
epsilon <- 1e-6

head(bank_w_by_county)
nrow(bank_w_by_county)
cc_costs[, contrib_circ := w_circ_county * ((cost + epsilon)^(-theta))]
cc_costs[, contrib_cap := w_cap_county * ((cost + epsilon)^(-theta))]
cc_costs[, contrib_dep := w_dep_county * ((cost + epsilon)^(-theta))]
cc_costs[, contrib_assets := w_assets_county * ((cost + epsilon)^(-theta))]
head(cc_costs)

exposure_df <- cc_costs[,.(
    bank_circ_exposure = sum(contrib_circ, na.rm = TRUE),
    bank_cap_exposure = sum(contrib_cap, na.rm = TRUE),
    bank_dep_exposure_raw = sum(contrib_dep, na.rm = TRUE),
    bank_assets_exposure = sum(contrib_assets, na.rm = TRUE)
  ),
  by = .(from_NHGISST, from_NHGISCTY)
]

exposure_df <- exposure_df %>% 
  full_join(jf_1900,  by = c("from_NHGISST", "from_NHGISCTY")) 

exposure_df <- as.data.table(exposure_df)
exposure_df[is.na(num_total_banks_1900), num_total_banks_1900 := 0]
## need to not drop places -- set pmax to 1 
exposure_df[, adjusted_bank_dep_exposure := bank_dep_exposure_raw / pmax(num_total_banks_1900, 1)]

exposure_df[, ln_bank_circ_exposure := log(bank_circ_exposure + 1)]
exposure_df[, ln_bank_cap_exposure := log(bank_cap_exposure + 1)]
exposure_df[, ln_bank_assets_exposure := log(bank_assets_exposure + 1)]
exposure_df[, ln_bank_dep_exposure_raw := log(bank_dep_exposure_raw + 1)]

exposure_df_output <- exposure_df[
  ,
  .(
    from_NHGISST,
    from_NHGISCTY,
    num_total_banks_1900,
    bank_circ_exposure,
    bank_cap_exposure,
    bank_dep_exposure_raw, 
    adjusted_bank_dep_exposure, # new measure with the 1900 bank count 
    bank_assets_exposure,
    ln_bank_circ_exposure,
    ln_bank_cap_exposure,
    ln_bank_dep_exposure_raw,
    ln_bank_assets_exposure
  )
]

write.csv(exposure_df_output, "Data/data_outputs/bank_market_exposure.csv", row.names = FALSE)
