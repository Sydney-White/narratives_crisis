### show change in Republican vote share 1904-1908 

rm(list = ls())
library(tidyverse)
library(RColorBrewer)
library(data.table)
library(sf)
library(stargazer)

# load data ---------------------------------------------------------------

elections <- read.csv("Data/data_outputs/clean_election_data.csv")

## 1900 shapefiles 
sf_1900 <- read_sf(paste0("Data/data_inputs/Shapefiles/", "nhgis0001_shapefile_tl2008_us_county_1900/",
                          "US_county_1900_conflated.shp")) %>%
  mutate(ICPSRST = as.numeric(ICPSRST)) %>%
  mutate(ICPSRCTY = as.numeric(ICPSRCTY)) %>%
  filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory"))
sf_1900 <- st_simplify(sf_1900, dTolerance = 1000, preserveTopology = TRUE) # smooth shape files so code runs faster

## try merge 

elections_1900 <- elections %>% 
  full_join(sf_1900, by = c("icpsr_state_code" = "ICPSRST", 
                            "county_identification_no" = "ICPSRCTY")) %>% 
  st_as_sf()

hist(elections_1900$socialist_general_swing_1904_1908)


# look at new york --------------------------------------------------------

elections_1900_NY <- elections_1900 %>% 
  filter(NHGISNAM == "New York") 
st_geometry(elections_1900_NY) <- NULL
class(elections_1900_NY)

stargazer(t(elections_1900_NY), summary = F, header = F, 
          out = "~/Dropbox/Apps/Overleaf/New York Case Study/elections_nyc.tex")

# show republican -------------------------------------------------------------------------

custom_palette <- colorRampPalette(
  brewer.pal(9, "Greens"))(10)

quantile <- quantile(elections_1900$anti_rep_swing_1904_1908, c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                              0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE)

elections_1900 <- elections_1900 %>%
  mutate(var_cut = cut(anti_rep_swing_1904_1908, breaks = unique(quantile), include.lowest = TRUE)) %>%
  mutate(var_cut = as.factor(var_cut))

p <- ggplot() +
  geom_sf(data = sf_1900, fill = "white", color = NA) +
  geom_sf(data = elections_1900, aes(fill = var_cut), color = NA) +
  scale_fill_manual(values = custom_palette, name = "") +
  guides(fill = guide_legend(title = "Decile")) +
  coord_sf(expand = FALSE) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text  = element_blank(),
    legend.key.size = unit(0.6, "lines"),
    axis.title = element_blank(),
    panel.border = element_rect(color = NA),
    legend.title = element_text(size = 8),
    legend.text  = element_text(size = 8),
    legend.position = "right"
  )

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/HPE Final Project/figures/rep_shift_1904_1908.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

# show socialist -------------------------------------------------------------------------

custom_palette <- colorRampPalette(brewer.pal(9, "Blues"))(10)

quantile <- quantile(elections_1900$socialist_general_swing_1904_1908, c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                                   0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE)

elections_1900 <- elections_1900 %>%
  mutate(var_cut = cut(socialist_general_swing_1904_1908, breaks = unique(quantile), include.lowest = TRUE)) %>%
  mutate(var_cut = as.factor(var_cut))

p <- ggplot() +
  geom_sf(data = sf_1900, fill = "white", color = NA) +
  geom_sf(data = elections_1900, aes(fill = var_cut), color = NA) +
  scale_fill_manual(values = custom_palette, name = "") +
  guides(fill = guide_legend(title = "Decile")) +
  coord_sf(expand = FALSE) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text  = element_blank(),
    legend.key.size = unit(0.6, "lines"),
    axis.title = element_blank(),
    panel.border = element_rect(color = NA),
    legend.title = element_text(size = 8),
    legend.text  = element_text(size = 8),
    legend.position = "right"
  )

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/HPE Final Project/figures/socialist_shift_1904_1908.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")
