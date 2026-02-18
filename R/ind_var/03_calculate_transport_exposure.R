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

shp_paths_1900 <- list.files(
  base_dir,
  pattern = "Component_.*_1900\\.shp$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

length(shp_paths_1900)
shp_1900_components <- lapply(shp_paths_1900, st_read, quiet = TRUE)

target_crs <- st_crs(shp_1900_components[[1]])
shp_1900_components <- lapply(shp_1900_components, st_transform, crs = target_crs)

### all years

shp_all_years <- list.files(
  base_dir,
  pattern = "Component_.*_allyears\\.shp$",
  recursive = TRUE,
  full.names = TRUE
)

sf_all_components <- lapply(shp_all_years, st_read, quiet = TRUE)

target_crs <- st_crs(sf_all_components[[1]])
sf_all_components <- lapply(sf_all_components, st_transform, crs = target_crs)

# bind into one sf object
sf_all <- bind_rows(shp_1900_components,
                    sf_all_components, .id = "source_file")

sf_all %>%
  st_drop_geometry() %>%
  count(Id)
colnames(sf_all)

# -------------------------------------------------------------------------

mile        <- 1609.344

sf_all$length_miles <- as.numeric(st_length(sf_all)) / mile
sf_all$mean_near_mi <- sf_all$MEAN_NEAR_ / mile

unique(sf_all$Id)

# re-calculate transport costs -------------------------------------------------

NetworkConstruction_tables <- read_excel("Data/NetworkDatabase_HR_2021/ReadMe_GIS_NetworkConstruction_tables.xlsx")

# first load in and parse the railroad components (time variant)
# then make those into a non-weighted graph.
# finally, map the county centroids to the nodes nearest to them on the
# transport network graph

edges <- sf_all %>%
  mutate(edge_id = row_number()) # assign one edge to each line-string

nodes <- st_coordinates(edges) %>%
  data.table()

edges <- data.table(edges)
nodes[, n_group := .N, by = .(L1)]
nodes[, n_index := seq_len(.N), by = .(L1)]
nodes <- nodes[!(n_index > 1 & n_index < n_group)]
nodes[, `:=`(n_group = NULL, n_index = NULL)]

nodes[, edge_id := L1]
nodes[, L1 := NULL]

nodes[, `:=`(X = round(X, 4), Y = round(Y, 4))]
nodes[, start_end := rep(c("start", "end"), nrow(nodes) / 2)]
nodes[, node_id := as.numeric(as.factor(paste(X, Y)))]
nodes[, node_id := paste0("node_", node_id)]

# read in the nodes and do some clean up
start_ids <- nodes[start_end == "start", .(edge_id, from = node_id)]
end_ids <- nodes[start_end == "end", .(edge_id, to = node_id)]
nodes <- unique(nodes[, .(node_id, X, Y)])

# a full join of start/end nodes into the edge list
edges <- merge(edges, start_ids, by = "edge_id")

edges <- merge(edges, end_ids, by = "edge_id")

tbg <- tbl_graph(nodes = nodes, edges = edges, node_key = "node_id", directed = F)

comps <- components(tbg)
summary(comps$csize) ### shows the national connectivity. some places are not connected at all

#
sf_nodes <- st_as_sf(nodes, coords = c("X", "Y"),
                     crs = st_crs(sf_all))

### second component

sf_1900_centroids <- st_centroid(sf_1900)

sf_centroids_temp <- copy(sf_1900_centroids)
# find node of transport graph nearest to county centroids
nearests <- st_nearest_feature(sf_centroids_temp, sf_nodes)
nearest_nodes <- sf_nodes$node_id[nearests]
sf_centroids_temp$node_id <- nearest_nodes
df_centroid <- data.table(
  sf_centroids_temp)[,.(node_id, NHGISST = as.numeric(NHGISST),
                        NHGISCTY = as.numeric(NHGISCTY))]


st_crs(sf_1900_centroids) == st_crs(sf_nodes)
head(V(tbg)$name)
head(V(tbg)$node_id)

V(tbg)$name <- V(tbg)$node_id ### rename name of vertices
head(V(tbg)$name)

centroid_nodes <- unique(df_centroid$node_id)

## finds least cost path between all county centroids
dists <- igraph::distances(
  tbg,
  v  = centroid_nodes,
  to = centroid_nodes 
)

dists <- as.matrix(dists) ## force into matrix
is.null(rownames(dists))
is.null(colnames(dists))
dists_long <- as.data.table(as.table(dists))
setnames(dists_long, c("from_id", "to_id", "cost"))
head(dists_long)

dists_long <- merge( ### ORIGIN COUNTIES
  dists_long,
  df_centroid[, .(to_id = node_id, to_NHGISST = NHGISST, to_NHGISCTY = NHGISCTY)],
  by = "to_id",
  allow.cartesian = TRUE
)

# origin counties
costs_df <- merge(
  dists_long,
  df_centroid[, .(from_id = node_id, from_NHGISST = NHGISST, from_NHGISCTY = NHGISCTY)],
  by = "from_id",
  allow.cartesian = TRUE
)

costs_df <- costs_df[is.finite(cost)]

fwrite(costs_df, "Data/data_outputs/all_transportation_costs.csv")

# now utilize bank data ---------------------------------------------------

df_banks_non_sf <- read.csv("Data/data_outputs/digitized_bank_failures.csv") %>% 
  mutate(Circulation_outstanding_at_failure = as.numeric(Circulation_outstanding_at_failure)) %>% 
  filter(failure_year %in% c(1907:1908)) %>% 
  filter(!is.na(X_CENTROID)) # drop two missing 

banks_sf <- df_banks_non_sf %>%
  st_as_sf(coords = c("X_CENTROID", "Y_CENTROID"), crs = st_crs(sf_nodes), remove = FALSE)

bank_nearest <- st_nearest_feature(banks_sf, sf_nodes)
banks_sf$node_id <- sf_nodes$node_id[bank_nearest]

setDT(costs_df)
banks_dt <- as.data.table(df_banks_non_sf)

banks_dt[, circ := as.numeric(Circulation_outstanding_at_failure)]
banks_dt[, cap  := as.numeric(Capital_at_failure)]

banks_dt[, w_circ := circ / max(circ, na.rm = TRUE)]
banks_dt[, w_cap  := cap  / max(cap,  na.rm = TRUE)]

bank_w_by_county <- banks_dt[
  , .(
    w_circ_county = sum(w_circ, na.rm = TRUE),
    w_cap_county  = sum(w_cap,  na.rm = TRUE)
  ),
  by = .(to_NHGISST = as.numeric(STATE), to_NHGISCTY = as.numeric(COUNTY))
]

cc_costs <- costs_df[, .(from_NHGISST, from_NHGISCTY, to_NHGISST, to_NHGISCTY, cost)]

cc_costs <- merge(
  cc_costs,
  bank_w_by_county,
  by = c("to_NHGISST", "to_NHGISCTY"),
  all.x = TRUE
)

cc_costs[is.na(w_circ_county), w_circ_county := 0]
cc_costs[is.na(w_cap_county),  w_cap_county  := 0]

theta <- 3
epsilon <- 1e-6

cc_costs[, contrib_circ := w_circ_county * ((cost + epsilon)^(-theta))]
cc_costs[, contrib_cap  := w_cap_county  * ((cost + epsilon)^(-theta))]

exposure_df <- cc_costs[
  , .(
    bank_circ_exposure = sum(contrib_circ, na.rm = TRUE),
    bank_cap_exposure  = sum(contrib_cap,  na.rm = TRUE)
  ),
  by = .(from_NHGISST, from_NHGISCTY)
]

exposure_df[, ln_bank_circ_exposure := log(bank_circ_exposure + 1)]
exposure_df[, ln_bank_cap_exposure  := log(bank_cap_exposure + 1)]

exposure_df_output <- exposure_df[, .(
  from_NHGISST, from_NHGISCTY,
  ln_bank_circ_exposure,
  ln_bank_cap_exposure
)]

write.csv(exposure_df_output, "Data/data_outputs/bank_market_exposure.csv", row.names = FALSE)

# plot  -------------------------------------------------------------------

sf_1900 <- read_sf(paste0("Data/Shapefiles/", "nhgis0001_shapefile_tl2008_us_county_1900/",
                          "US_county_1900_conflated.shp")) %>%
  mutate(NHGISST = as.numeric(NHGISST)) %>%
  mutate(NHGISCTY = as.numeric(NHGISCTY)) %>%
  filter(STATENAM != "Alaska Territory") %>%
  st_as_sf()
sf_1900 <- st_simplify(sf_1900,
                       dTolerance = 1000, preserveTopology = TRUE) # smooth shape files so code runs faster

exposure_df <- exposure_df %>%
  left_join(sf_1900, by = c("from_NHGISST" = "NHGISST", "from_NHGISCTY" = "NHGISCTY")) %>% # merge with shapefile
  st_as_sf() %>%
  filter(STATENAM != "Alaska Territory") %>% # remove alaska for mapping purposes
  filter(STATENAM != "Hawaii Territory") 
class(exposure_df) 

quantile <- quantile(exposure_df$ln_bank_cap_exposure, c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                              0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE)

exposure_df <- exposure_df %>%
  mutate(var_cut = cut(ln_bank_cap_exposure, breaks = unique(quantile))) %>% 
  st_as_sf()

p <- ggplot() + # visualizes bank exposure transport costs by quantile
  geom_sf(
    data = exposure_df, aes(fill = var_cut), color = NA,
    size = 0.1
  ) +
  scale_fill_manual(values = custom_palette, name = "") +
  guides(fill = guide_legend(title = "Circ Exposure", nrow = 3))+
  coord_sf(crs = 2163) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.key.size = unit(0.4, "lines"),
    panel.border = element_blank(),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.position = "bottom"
  )

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/New Independent Var/crisis_shock/circ_exposure.pdf",
       device = pdf, bg = "white", width = 6, height = 4.29, units = "in")
