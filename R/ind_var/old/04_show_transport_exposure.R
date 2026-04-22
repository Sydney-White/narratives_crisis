## 04 show transport exposure

rm(list = ls())
library(sf)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

custom_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(10)

exposure_df <- read.csv("Data/data_outputs/bank_market_exposure.csv")

sf_1900 <- read_sf("Data/Shapefiles/nhgis0001_shapefile_tl2008_us_county_1900/US_county_1900_conflated.shp") %>%
  mutate(
    NHGISST = as.numeric(NHGISST),
    NHGISCTY = as.numeric(NHGISCTY)
  ) %>%
  filter(STATENAM != "Alaska Territory") %>%
  filter(STATENAM != "Hawaii Territory") %>%
  st_as_sf()

sf_1900 <- st_simplify(sf_1900, dTolerance = 1000, preserveTopology = TRUE)

exposure_sf <- exposure_df %>%
  left_join(sf_1900, by = c("from_NHGISST" = "NHGISST", "from_NHGISCTY" = "NHGISCTY")) %>%
  st_as_sf()

if (!("ln_bank_circ_exposure" %in% names(exposure_sf)) && ("bank_circ_exposure" %in% names(exposure_sf))) {
  exposure_sf$ln_bank_circ_exposure <- log(exposure_sf$bank_circ_exposure + 1)
}
if (!("ln_bank_cap_exposure" %in% names(exposure_sf)) && ("bank_cap_exposure" %in% names(exposure_sf))) {
  exposure_sf$ln_bank_cap_exposure <- log(exposure_sf$bank_cap_exposure + 1)
}
if (!("ln_bank_dep_exposure" %in% names(exposure_sf)) && ("bank_dep_exposure" %in% names(exposure_sf))) {
  exposure_sf$ln_bank_dep_exposure <- log(exposure_sf$bank_dep_exposure + 1)
}
if (!("ln_bank_assets_exposure" %in% names(exposure_sf)) && ("bank_assets_exposure" %in% names(exposure_sf))) {
  exposure_sf$ln_bank_assets_exposure <- log(exposure_sf$bank_assets_exposure + 1)
}

# overleaf_crisis_shock <- "~/Dropbox/Apps/Overleaf/New Independent Var/crisis_shock"
overleaf_crisis_shock <- "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/figures"

dir.create(path.expand(overleaf_crisis_shock), recursive = TRUE, showWarnings = FALSE)

# circulation exposure ----------------------------------------------------

quantile <- quantile(
  exposure_sf$ln_bank_circ_exposure,
  c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
  na.rm = TRUE
)

exposure_plot <- exposure_sf %>%
  mutate(var_cut = cut(ln_bank_circ_exposure, breaks = unique(quantile), include.lowest = TRUE)) %>%
  st_as_sf()

ggplot() +
  geom_sf(data = exposure_plot, aes(fill = var_cut), color = NA, size = 0.1) +
  scale_fill_manual(values = custom_palette, name = "") +
  guides(fill = guide_legend(title = "Circ Exposure", nrow = 3)) +
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

ggsave(plot = p, filename = file.path(path.expand(overleaf_crisis_shock), "circ_exposure.pdf"),
       device = pdf, bg = "white", width = 6, height = 4.29, units = "in")

# capital exposure --------------------------------------------------------

quantile <- quantile(
  exposure_sf$ln_bank_cap_exposure,
  c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
  na.rm = TRUE
)

exposure_plot <- exposure_sf %>%
  mutate(var_cut = cut(ln_bank_cap_exposure, breaks = unique(quantile), include.lowest = TRUE)) %>%
  st_as_sf()

ggplot() +
  geom_sf(data = exposure_plot, aes(fill = var_cut), color = NA, size = 0.1) +
  scale_fill_manual(values = custom_palette, name = "") +
  guides(fill = guide_legend(title = "Cap Exposure", nrow = 3)) +
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

ggsave(plot = p, filename = file.path(path.expand(overleaf_crisis_shock), "cap_exposure.pdf"),
       device = pdf, bg = "white", width = 6, height = 4.29, units = "in")

# deposits exposure -------------------------------------------------------

if ("ln_bank_dep_exposure" %in% names(exposure_sf)) {
  quantile <- quantile(
    exposure_sf$ln_bank_dep_exposure,
    c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
    na.rm = TRUE
  )
  
  exposure_plot <- exposure_sf %>%
    mutate(var_cut = cut(ln_bank_dep_exposure, breaks = unique(quantile), include.lowest = TRUE)) %>%
    st_as_sf()
  
  p <- ggplot() +
    geom_sf(data = exposure_plot, aes(fill = var_cut), color = NA, size = 0.1) +
    scale_fill_manual(values = custom_palette, name = "") +
    guides(fill = guide_legend(title = "Deposits Exposure", nrow = 3)) +
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
  
  ggsave(plot = p, filename = file.path(path.expand(overleaf_crisis_shock), "dep_exposure.pdf"),
         device = pdf, bg = "white", width = 6, height = 4.29, units = "in")
} else {
  message("Skipping deposits map: ln_bank_dep_exposure not found in bank_market_exposure.csv")
}

# assets exposure ---------------------------------------------------------

if ("ln_bank_assets_exposure" %in% names(exposure_sf)) {
  quantile <- quantile(
    exposure_sf$ln_bank_assets_exposure,
    c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
    na.rm = TRUE
  )
  
  exposure_plot <- exposure_sf %>%
    mutate(var_cut = cut(ln_bank_assets_exposure, breaks = unique(quantile), include.lowest = TRUE)) %>%
    st_as_sf()
  
  p <- ggplot() +
    geom_sf(data = exposure_plot, aes(fill = var_cut), color = NA, size = 0.1) +
    scale_fill_manual(values = custom_palette, name = "") +
    guides(fill = guide_legend(title = "Assets Exposure", nrow = 3)) +
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
  
  ggsave(plot = p, filename = file.path(path.expand(overleaf_crisis_shock), "assets_exposure.pdf"),
         device = pdf, bg = "white", width = 6, height = 4.29, units = "in")
} else {
  message("Skipping assets map: ln_bank_assets_exposure not found in bank_market_exposure.csv")
}
