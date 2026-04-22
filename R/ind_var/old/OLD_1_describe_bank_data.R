# ### describe bank data
# 
# rm(list = ls())
# library(haven)
# library(dplyr)
# library(stringr)
# library(sf)
# library(ggplot2)
# 
# # load data ---------------------------------------------------------------
# 
# state_lookup <- c(
#   setNames(state.abb, gsub("[^a-z]", "", tolower(state.name))),
#   setNames(state.abb, gsub("[^a-z]", "", tolower(state.abb))),
#   ala = "AL", ariz = "AZ", ark = "AR", cal = "CA", colo = "CO",
#   conn = "CT", del = "DE", fla = "FL", ga = "GA", ill = "IL",
#   ind = "IN", kans = "KS", ky = "KY", la = "LA", mass = "MA",
#   md = "MD", mich = "MI", minn = "MN", miss = "MS", mo = "MO",
#   mont = "MT", neb = "NE", nev = "NV", nh = "NH", nj = "NJ",
#   nm = "NM", ny = "NY", nc = "NC", nd = "ND", okla = "OK",
#   oreg = "OR", pa = "PA", penna = "PA", penn = "PA", ri = "RI",
#   sc = "SC", sd = "SD", dak = "SD", tenn = "TN", tex = "TX",
#   va = "VA", vt = "VT", wash = "WA", wva = "WV", wis = "WI", wyo = "WY"
# )
# 
# receiverships_panel <- read_dta(
#   "Data/data_inputs/qje-repkit-to-upload/sources/occ-receiverships/receiverships_panel.dta"
# ) %>%
#   mutate(
#     receiver_appt_year = sub(".*((18|19|20)[0-9OISl]{2}).*", "\\1", as.character(date_receiver_appt)),
#     receiver_appt_year = ifelse(grepl("^(18|19|20)[0-9OISl]{2}$", receiver_appt_year), receiver_appt_year, NA_character_),
#     receiver_appt_year = gsub("O", "0", receiver_appt_year),
#     receiver_appt_year = gsub("I", "1", receiver_appt_year),
#     receiver_appt_year = gsub("l", "1", receiver_appt_year),
#     receiver_appt_year = gsub("S", "5", receiver_appt_year),
#     receiver_appt_year = suppressWarnings(as.integer(receiver_appt_year)),
#     receiver_appt_year = ifelse(!is.na(receiver_appt_year) & receiver_appt_year >= 1800 & receiver_appt_year <= 2050, receiver_appt_year, NA_integer_),
#     closed_year = sub(".*((18|19|20)[0-9OISl]{2}).*", "\\1", as.character(date_closed)),
#     closed_year = ifelse(grepl("^(18|19|20)[0-9OISl]{2}$", closed_year), closed_year, NA_character_),
#     closed_year = gsub("O", "0", closed_year),
#     closed_year = gsub("I", "1", closed_year),
#     closed_year = gsub("l", "1", closed_year),
#     closed_year = gsub("S", "5", closed_year),
#     closed_year = suppressWarnings(as.integer(closed_year)),
#     closed_year = ifelse(!is.na(closed_year) & closed_year >= 1800 & closed_year <= 2050, closed_year, NA_integer_),
#     failure_year = ifelse(!is.na(receiver_appt_year), receiver_appt_year, closed_year),
#     failure_capital = as.numeric(failure_capital),
#     circulation_at_failure = as.numeric(circulation_at_failure),
#     deposits_at_suspension = as.numeric(deposits_at_suspension)
#   ) %>%
#   filter(!is.na(failure_year), failure_year >= 1865, failure_year <= 1914) %>%
#   distinct(
#     failure_id, bank_name, failure_year,
#     failure_capital, circulation_at_failure, deposits_at_suspension
#   )
# 
# # build county crosswalk using existing digitized location file -----------
# 
# location_lookup <- read.csv("Data/data_inputs/digitized_bank_failures.csv") %>%
#   transmute(
#     city_key = gsub("[^a-z]", "", tolower(as.character(city))),
#     state_key = gsub("[^a-z]", "", tolower(as.character(State))),
#     NHGISST = as.numeric(NHGISST),
#     NHGISCTY = as.numeric(NHGISCTY)
#   ) %>%
#   filter(!is.na(city_key), city_key != "", !is.na(state_key), state_key != "") %>%
#   filter(!is.na(NHGISST), !is.na(NHGISCTY)) %>%
#   count(city_key, state_key, NHGISST, NHGISCTY, sort = TRUE) %>%
#   group_by(city_key, state_key) %>%
#   slice_head(n = 1) %>%
#   ungroup() %>%
#   select(-n)
# 
# # failures in 1907-08 -----------------------------------------------------
# 
# df_joined_non_sf <- receiverships_panel %>%
#   filter(failure_year %in% c(1907, 1908)) %>%
#   mutate(parts = str_split(bank_name, ",")) %>%
#   mutate(
#     city = sapply(parts, function(v) if (length(v) >= 2) str_squish(v[length(v) - 1]) else NA_character_),
#     state_raw = sapply(parts, function(v) if (length(v) >= 1) str_squish(v[length(v)]) else NA_character_),
#     city = str_squish(gsub("[^A-Za-z -]", "", city)),
#     state_abb = unname(state_lookup[gsub("[^a-z]", "", tolower(state_raw))]),
#     state_full = state.name[match(state_abb, state.abb)],
#     city_key = gsub("[^a-z]", "", tolower(city)),
#     state_key = gsub("[^a-z]", "", tolower(state_full))
#   ) %>%
#   left_join(location_lookup, by = c("city_key", "state_key")) %>%
#   select(-parts, -city_key, -state_key)
# 
# sf_1900 <- read_sf(
#   "Data/Shapefiles/nhgis0001_shapefile_tl2008_us_county_1900/US_county_1900_conflated.shp"
# ) %>%
#   mutate(NHGISST = as.numeric(NHGISST)) %>%
#   mutate(NHGISCTY = as.numeric(NHGISCTY)) %>%
#   filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory"))
# 
# sf_1900 <- st_simplify(sf_1900, dTolerance = 1000, preserveTopology = TRUE)
# 
# df_joined <- df_joined_non_sf %>%
#   full_join(sf_1900, by = c("NHGISST", "NHGISCTY"))
# 
# county_agg <- df_joined %>%
#   st_drop_geometry() %>%
#   group_by(NHGISST, NHGISCTY) %>%
#   summarize(
#     cap_fail = sum(failure_capital, na.rm = TRUE),
#     circ_fail = sum(circulation_at_failure, na.rm = TRUE),
#     dep_fail = sum(deposits_at_suspension, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# county_points <- st_centroid(sf_1900) %>%
#   left_join(county_agg, by = c("NHGISST", "NHGISCTY"))
# 
# out_dir <- "~/Dropbox/Apps/Overleaf/New Independent Var/correia_output"
# 
# # map: capital ------------------------------------------------------------
# 
# p <- ggplot() +
#   geom_sf(data = sf_1900, fill = "white", color = "grey85", size = 0.1) +
#   geom_sf(
#     data = county_points %>% filter(!is.na(cap_fail), cap_fail > 0),
#     aes(size = cap_fail),
#     color = "#1E90FF",
#     alpha = 0.7
#   ) +
#   scale_size_continuous(name = "Capital at failure", range = c(2, 10)) +
#   coord_sf(crs = 2163) +
#   theme_bw() +
#   theme(
#     axis.ticks = element_blank(),
#     panel.border = element_rect(color = NA, fill = NA),
#     legend.title = element_text(size = 8),
#     legend.text = element_text(size = 8),
#     legend.position = "bottom"
#   )
# 
# ggsave(plot = p,
#   filename = file.path(out_dir, "capital_fail.pdf"),
#   device = pdf,
#   bg = "white",
#   width = 4.88,
#   height = 3.34,
#   units = "in"
# )
# 
# # map: circulation --------------------------------------------------------
# 
# p <- ggplot() +
#   geom_sf(data = sf_1900, fill = "white", color = "grey85", size = 0.1) +
#   geom_sf(
#     data = county_points %>% filter(!is.na(circ_fail), circ_fail > 0),
#     aes(size = circ_fail),
#     color = "#1E90FF",
#     alpha = 0.7
#   ) +
#   scale_size_continuous(name = "Circulation at failure", range = c(2, 10)) +
#   coord_sf(crs = 2163) +
#   theme_bw() +
#   theme(
#     axis.ticks = element_blank(),
#     panel.border = element_rect(color = NA, fill = NA),
#     legend.title = element_text(size = 8),
#     legend.text = element_text(size = 8),
#     legend.position = "bottom"
#   )
# 
# ggsave(
#   plot = p,
#   filename = file.path(out_dir, "circulation_locate.pdf"),
#   device = pdf,
#   bg = "white",
#   width = 4.88,
#   height = 3.34,
#   units = "in"
# )
# 
# # map: deposits -----------------------------------------------------------
# 
# p <- ggplot() +
#   geom_sf(data = sf_1900, fill = "white", color = "grey85", size = 0.1) +
#   geom_sf(
#     data = county_points %>% filter(!is.na(dep_fail), dep_fail > 0),
#     aes(size = dep_fail),
#     color = "#1E90FF",
#     alpha = 0.7
#   ) +
#   scale_size_continuous(name = "Deposits at suspension", range = c(2, 10)) +
#   coord_sf(crs = 2163) +
#   theme_bw() +
#   theme(
#     axis.ticks = element_blank(),
#     panel.border = element_rect(color = NA, fill = NA),
#     legend.title = element_text(size = 8),
#     legend.text = element_text(size = 8),
#     legend.position = "bottom"
#   )
# 
# ggsave(
#   plot = p,
#   filename = file.path(out_dir, "deposits_fail.pdf"),
#   device = pdf,
#   bg = "white",
#   width = 4.88,
#   height = 3.34,
#   units = "in"
# )
# 
# # time series -------------------------------------------------------------
# 
# df_counts <- receiverships_panel %>%
#   group_by(failure_year) %>%
#   summarise(n_failures = n_distinct(failure_id), .groups = "drop")
# 
# panic_start <- 1907
# panic_end <- 1908
# 
# p <- ggplot(df_counts, aes(x = failure_year, y = n_failures)) +
#   geom_line(color = "black", linewidth = 0.5) +
#   geom_point(color = "black", size = 0.5) +
#   annotate(
#     "rect",
#     xmin = panic_start,
#     xmax = panic_end,
#     ymin = -Inf, ymax = Inf,
#     alpha = 0.2,
#     fill = "grey70"
#   ) +
#   theme_bw(base_family = "serif") +
#   labs(
#     x = "",
#     y = "No. banks failed",
#     title = ""
#   )
# 
# ggsave(
#   plot = p,
#   filename = file.path(out_dir, "failed_banks_year.pdf"),
#   device = pdf,
#   bg = "white",
#   width = 4.88,
#   height = 3.34,
#   units = "in"
# )
# 
# message(
#   "Mapped ",
#   sum(!is.na(df_joined_non_sf$NHGISST) & !is.na(df_joined_non_sf$NHGISCTY)),
#   " / ",
#   nrow(df_joined_non_sf),
#   " Correia failures (1907-1908) to counties."
# )
