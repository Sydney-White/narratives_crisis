#### re-merge stage 2 and look 

rm(list = ls())
library(digest)
library(readr)
library(fuzzyjoin)
library(data.table)
library(stringdist)
library(sf)
library(tidyverse)

# load data ---------------------------------------------------------------

stage2_output <- fread("Data/data_outputs/classified_stage2_output.csv") 
sampled_articles <- fread("Data/python_data_outputs/sampled_articles.csv")
bert <- fread("Data/data_outputs/bert_blame_probabilities_full.csv")

sf_1900 <- read_sf(paste0("Data/Shapefiles/", "nhgis0001_shapefile_tl2008_us_county_1900/",
                          "US_county_1900_conflated.shp")) %>%
  mutate(NHGISST = as.numeric(NHGISST)) %>%
  mutate(NHGISCTY = as.numeric(NHGISCTY)) %>%
  filter(!STATENAM %in% c("Alaska Territory", "Hawaii Territory"))
sf_1900 <- st_simplify(sf_1900, dTolerance = 1000, preserveTopology = TRUE) # smooth shape files so code runs faster

## get chronicling america 
chronicling_america <- read_csv("Data/data_inputs/chronicling-america.csv") %>% 
  separate(`Geo Location`, into = c("latitude", "longitude"),
           sep = ",", remove = FALSE, fill = "right") %>%
  mutate(
    latitude  = as.numeric(str_trim(latitude)),
    longitude = as.numeric(str_trim(longitude))
  ) %>%
  filter(!is.na(latitude), !is.na(longitude))

# normalize text for join -------------------------------------------------

normalize_text <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9 ]", " ", x)    
  x <- gsub("\\s+", " ", x)           
  trimws(x)
}
sampled_articles$norm <- normalize_text(sampled_articles$excerpt)
stage2_output$norm    <- normalize_text(stage2_output$excerpt)
bert$norm    <- normalize_text(bert$excerpt)

aligned <- stage2_output %>%  ## missing some !!! 
  left_join(sampled_articles,  by = c("norm")) %>% 
  left_join(bert, by = c("norm"))

sum(is.na(aligned$bert_prob)) ## half are missing?? or can't merge 

# show distribution of classification -------------------------------------

plot_df <- stage2_output %>%
  count(label) %>%
  mutate(
    percent = round(100 * n / sum(n), 1),
    label = factor(label, levels = rev(sort(unique(label))))  # reverse order for plotting
  )


p <- ggplot(plot_df, aes(x = label, y = n)) +
  geom_col(fill = "#2A7F7F", alpha = 0.8) +
  geom_text(aes(label = paste0(percent, "%")), 
            hjust = -0.1, size = 3.8, family = "serif") +
  coord_flip() +
  theme_bw() +
  labs(x = "", y = "Count of Articles", title = "") +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  ) +
  ylim(0, max(plot_df$n) * 1.25)   

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/HPE Final Project/figures/dist_classification.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

# drop NA for now ---------------------------------------------------------

aligned <- aligned %>% 
  filter(!is.na(file)) %>% 
  mutate(year = 1907, 
         lccn_id = str_extract(file, "sn[0-9]{8}")) 

chronicling_america_sf <- st_as_sf(
  chronicling_america,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

sf_1900 <- st_transform(sf_1900, 4326)

chronicling_with_county <- st_join( ## spatial join 
  chronicling_america_sf,
  sf_1900,
  join = st_within,
  left = TRUE
)

chronicling_with_county_df <- st_drop_geometry(chronicling_with_county)

# now make into county-level dataset --------------------------------------

classified_articles_1907 <- aligned %>%
  full_join(chronicling_with_county, by = c("lccn_id" = "LCCN")) %>%
  mutate(
    date_clean = str_replace(date, " .*$", ""),
    date_clean = as.Date(date_clean)
  ) %>%   rename(llm_classification = label)

# keep only financial narratives for denominator 
financial_narratives <- classified_articles_1907 %>%
  filter(llm_classification != "6. Miscellaneous / Not Interpretable")

total_financial_monthly <- financial_narratives %>%
  group_by(STATENAM, NHGISNAM, NHGISST, NHGISCTY, month) %>%    # county-level
  summarise(total_financial = n(), .groups = "drop") %>% 
  mutate(month = as.Date(month))

blame_monthly_llm <- financial_narratives %>%
  filter(llm_classification == "3. Domestic Blame / Scandal Framing") %>%
  group_by(STATENAM, NHGISNAM, NHGISST, NHGISCTY, month) %>%
  summarise(blame_articles_llm = n(), .groups = "drop") %>% 
  mutate(month = as.Date(month))

blame_monthly_bert <- financial_narratives %>%
  group_by(STATENAM, NHGISNAM, NHGISST, NHGISCTY, month) %>%
  summarise(
    bert_blame_mean = mean(bert_prob, na.rm = TRUE),
    bert_blame_sum  = sum(bert_prob,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(month = as.Date(month))

counties_with_articles <- total_financial_monthly %>%
  group_by(STATENAM, NHGISNAM, NHGISST, NHGISCTY) %>%
  summarise(any_financial = any(total_financial > 0), .groups = "drop") %>%
  filter(any_financial) %>%
  select(STATENAM, NHGISNAM, NHGISST, NHGISCTY)

all_months <- seq(as.Date("1907-01-01"), as.Date("1907-12-01"), by = "1 month")

restricted_grid <- expand_grid(
  counties_with_articles,
  month = all_months
)

panel <- total_financial_monthly %>%
  left_join(blame_monthly_llm, by = c("STATENAM","NHGISNAM","NHGISST","NHGISCTY","month")) %>%
  left_join(blame_monthly_bert, by = c("STATENAM","NHGISNAM","NHGISST","NHGISCTY","month"))

panel <- panel %>%
  mutate(
    total_financial = replace_na(total_financial, 0),
    blame_articles_llm = replace_na(blame_articles_llm, 0),
    bert_blame_sum = replace_na(bert_blame_sum, 0),
    
    llm_blame_share = if_else(
      total_financial == 0,
      NA_real_,
      blame_articles_llm / total_financial
    ),
    
    bert_blame_share = if_else(
      total_financial == 0,
      NA_real_,
      bert_blame_sum / total_financial
    )
  )

write.csv(panel, "Data/data_outputs/blame_share_monthly.csv")

# show blame share by month  ----------------------------------------------

monthly_trends <- panel %>%
  group_by(month) %>%
  summarise(
    llm_blame_share = mean(llm_blame_share, na.rm = TRUE),
    bert_blame_share = mean(bert_blame_mean, na.rm = TRUE),
    .groups = "drop"
  )

monthly_trends_long <- monthly_trends %>%
  pivot_longer(
    cols = c(llm_blame_share, bert_blame_share),
    names_to = "measure",
    values_to = "value"
  )

p <- ggplot(monthly_trends_long, aes(x = month, y = value, color = measure)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 0.5) + 
  scale_color_manual(
    values = c("llm_blame_share" = "gray40",
               "bert_blame_share" = "black"),
    labels = c("LLM blame share", "BERT blame probability")
  ) +
  labs(
    x = "",
    y = "Average blame intensity",
    color = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/Financial Crisis Narratives - LLM/figures/bert_versus_llm.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

cor(
  monthly_trends$llm_blame_share,
  monthly_trends$bert_blame_share,
  use = "complete.obs"
)

panel %>%
  group_by(month) %>%
  summarise(total_financial = sum(total_financial, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = total_financial)) +
  geom_line() +
  theme_minimal() +
  labs(y = "Total financial articles")

