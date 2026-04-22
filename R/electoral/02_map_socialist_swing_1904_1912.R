rm(list = ls())

library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(RColorBrewer)

elections <- read_csv("Data/data_outputs/clean_election_data.csv", show_col_types = FALSE)

sf_1900 <- read_sf(
  "Data/Shapefiles/nhgis0001_shapefile_tl2008_us_county_1900/US_county_1900_conflated.shp"
) %>%
  mutate(
    ICPSRST = as.numeric(ICPSRST),
    ICPSRCTY = as.numeric(ICPSRCTY)
  ) %>%
  filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory")) %>%
  st_simplify(dTolerance = 1000, preserveTopology = TRUE)

socialist_map_df <- elections %>%
  select(
    icpsr_state_code,
    county_identification_no,
    socialist_swing_1904_1912
  ) %>%
  left_join(
    sf_1900,
    by = c(
      "icpsr_state_code" = "ICPSRST",
      "county_identification_no" = "ICPSRCTY"
    )
  ) %>%
  st_as_sf()

custom_palette <- colorRampPalette(brewer.pal(9, "Blues"))(10)

quantiles <- quantile(
  socialist_map_df$socialist_swing_1904_1912,
  c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
  na.rm = TRUE
)

socialist_map_df <- socialist_map_df %>%
  mutate(
    var_cut = cut(
      socialist_swing_1904_1912,
      breaks = unique(quantiles),
      include.lowest = TRUE
    )
  )

socialist_support_map_1904_1912 <- ggplot() +
  geom_sf(data = sf_1900, fill = "white", color = NA) +
  geom_sf(data = socialist_map_df, aes(fill = var_cut), color = NA) +
  scale_fill_manual(values = custom_palette, name = "") +
  guides(fill = guide_legend(title = "Decile")) +
  coord_sf(expand = FALSE) +
  labs(title = "Socialist Vote Swing, 1904-1912") +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.border = element_rect(color = NA),
    legend.key.size = grid::unit(0.6, "lines"),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.position = "right"
  )

print(socialist_support_map_1904_1912)
