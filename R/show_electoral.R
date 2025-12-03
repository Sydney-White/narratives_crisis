### show change in Republican vote share 1904-1908 

library(tidyverse)
library(janitor)
library(labelled)
library(haven)
library(RColorBrewer)
library(data.table)
library(sf)
rm(list = ls())

# load data ---------------------------------------------------------------

elections <- read_dta("Data/data_inputs/County-Election Data/DS0001/08611-0001-Data.dta")

elections <- labelled::foreign_to_labelled(elections) # makes the stata labels into R
elections <- sjlabelled::label_to_colnames(elections)
colnames(elections) <- tolower(colnames(elections))
colnames(elections) <- janitor::make_clean_names(colnames(elections))

colnames(elections) 

elections <- elections %>% 
  select(icpsr_state_code, county_name, county_identification_no, 
         x1900_pres_dem_percent:x1912_pres_ttl_vote)

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
                            "county_identification_no" = "ICPSRCTY"))

# sf_1910 <- read_sf(paste0("Data/data_inputs/", "nhgis0001_shapefile_tl2008_us_county_1910/",
#                           "US_county_1910_conflated.shp")) %>%
#   mutate(NHGISST = as.numeric(NHGISST)) %>%
#   mutate(NHGISCTY = as.numeric(NHGISCTY)) %>%
#   filter(STATENAM != "Alaska Territory")
# sf_1910 <- st_simplify(sf_1910, dTolerance = 1000, preserveTopology = TRUE) # smooth shape files so code runs faster

# 1904 election -------------------------------------------------------------------------

elections_1900 <- elections_1900 %>%
  st_as_sf() %>% 
  filter(x1904_pres_rep_percent <= 100)

custom_palette <- colorRampPalette(brewer.pal(9, "Reds"))(10)

quantile <- quantile(elections_1900$x1904_pres_rep_percent, c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                              0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE)

elections_1900 <- elections_1900 %>%
  mutate(var_cut = cut(x1904_pres_rep_percent, breaks = unique(quantile), include.lowest = TRUE)) %>%
  mutate(var_cut = as.factor(var_cut))

ggplot() +
  geom_sf(data=sf_1900, fill = "white") +
  geom_sf(data=elections_1900, aes(fill=var_cut)) +
  theme_bw() +
  scale_fill_manual(values = custom_palette, name = "WHHI Decile") +
  guides(fill=guide_legend(title=expression("Democratic Share"))) +
  theme(
    axis.ticks = element_blank(),
    panel.border = element_rect(color = NA, fill = NA),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.position = "bottom")
ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/HPE Final Project/figures/rep_map_1908.pdf",
       device = png, bg = "white", width = 4.88, height = 3.34, units = "in")

# 1908 election -----------------------------------------------------------

elections_1900 <- elections_1900 %>%
  st_as_sf() %>% 
  filter(x1908_pres_rep_percent <= 100)

custom_palette <- colorRampPalette(brewer.pal(9, "Reds"))(10)

quantile <- quantile(elections_1900$x1908_pres_rep_percent, c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                      0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE)

elections_1900 <- elections_1900 %>%
  mutate(var_cut = cut(x1908_pres_rep_percent, breaks = unique(quantile), include.lowest = TRUE)) %>%
  mutate(var_cut = as.factor(var_cut))

ggplot() +
  geom_sf(data=sf_1900, fill = "white") +
  geom_sf(data=elections_1900, aes(fill=var_cut)) +
  theme_bw() +
  scale_fill_manual(values = custom_palette, name = "WHHI Decile") +
  guides(fill=guide_legend(title=expression("Democratic Share"))) +
  theme(
    axis.ticks = element_blank(),
    panel.border = element_rect(color = NA, fill = NA),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.position = "bottom")
ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/HPE Final Project/figures/rep_map_1908.pdf",
       device = png, bg = "white", width = 4.88, height = 3.34, units = "in")


# -------------------------------------------------------------------------

elections_1900 <- elections_1900 %>%
  mutate(
    # rep_2p_1900 = x1900_pres_rep_percent /
    #   (x1900_pres_rep_percent + x1900_pres_dem_percent) * 100,
    # rep_2p_1904 = x1904_pres_rep_percent /
    #   (x1904_pres_rep_percent + x1904_pres_dem_percent) * 100,
    # rep_2p_swing_1900_1904 = rep_2p_1904 - rep_2p_1900,
    rep_general_swing_1904_1908 = x1908_pres_rep_percent - x1904_pres_rep_percent
  ) %>%
  st_as_sf() %>%
  filter(x1904_pres_rep_percent <= 100)

custom_palette <- colorRampPalette(brewer.pal(9, "Reds"))(10)

quantile <- quantile(elections_1900$rep_general_swing_1904_1908, c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                              0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE)

elections_1900 <- elections_1900 %>%
  mutate(var_cut = cut(rep_general_swing_1904_1908, breaks = unique(quantile), include.lowest = TRUE)) %>%
  mutate(var_cut = as.factor(var_cut))

p <- ggplot() +
  geom_sf(data=sf_1900, fill = "white") +
  geom_sf(data=elections_1900, aes(fill=var_cut)) +
  theme_bw() +
  scale_fill_manual(values = custom_palette, name = "Rep Decile") +
  guides(fill=guide_legend(title=expression("Rep. Shift, 1904-1908"))) +
  theme(
    axis.ticks = element_blank(),
    panel.border = element_rect(color = NA, fill = NA),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.position = "bottom")
ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/HPE Final Project/figures/rep_shift_1904_1908.pdf",
       device = png, bg = "white", width = 4.88, height = 3.34, units = "in")
