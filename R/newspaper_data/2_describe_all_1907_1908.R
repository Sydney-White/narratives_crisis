### describe all 1907-1908 newspapers 

library(readr)
library(tidyverse)
library(RColorBrewer)
library(data.table)
library(stargazer)
rm(list = ls())

# show monthly time series  -----------------------------------------------

all_articles <- fread("Data/data_outputs/financial_articles_all.csv")
gc() 
df_dates <- all_articles %>%
  filter(date >= as.Date("1907-01-01"),
         date <= as.Date("1908-06-30")) %>%
  count(date, name = "articles_per_day") %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(avg_articles_per_day = mean(articles_per_day))
df_dates <- df_dates %>%
  mutate(month = as.Date(month))

panic_start <- as.Date("1907-10-01")
panic_end   <- as.Date("1908-01-31")

p <- ggplot(df_dates, aes(month, avg_articles_per_day)) +
  geom_line() +
  geom_point() +
  annotate(
    "rect",
    xmin = panic_start,
    xmax = panic_end,
    ymin = -Inf, ymax = Inf,
    alpha = 0.2,
    fill = "grey70"
  ) + 
  theme_bw(base_family = "serif") +
  scale_x_date(
    breaks = seq(as.Date("1907-01-01"), as.Date("1908-6-01"), by = "3 months"),
    date_labels = "%b\n%Y"
  )+
  labs(x = "", y = "Avg Articles Per Day")

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/HPE Final Project/figures/finance_articles_1907.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

# show location of newspaper coverage -------------------------------------

unique_papers <- all_articles %>% 
  distinct(Newspapers, NHGISNAM, STATENAM) 

unique_count <- unique_papers %>% 
  group_by(NHGISNAM, STATENAM) %>% 
  summarise(n_newspapers = n(), .groups = "drop") %>% 
  arrange(desc(n_newspapers)) %>% 
  filter(NHGISNAM != "") %>% 
  head(20) 
colnames(unique_count) <- c("County", "State", "No. Newspapers")

stargazer(
  unique_count, summary = FALSE, rownames = T,
  type = "latex", header = FALSE,
  label = "tab:newspaper_coverage",
  title = "Top 20 Counties with Highest Number of Newspapers (in
  the 1907-1908 financial sample.)",
  out = "~/Dropbox/Apps/Overleaf/HPE Final Project/appendix_tables/newspaper_coverage.tex"
)

# ethnicity -------------------------------------------------------------------------

## articles with ethnicity do not have a date 
ethnicity_count <- all_articles %>%
  count(Ethnicity, name = "articles") %>%  
  arrange(desc(articles)) %>%
  slice_head(n = 15) %>% 
  filter(!is.na(Ethnicity))

stargazer(
  ethnicity_count, summary = FALSE, rownames = FALSE,
  type = "latex", header = FALSE,
  label = "tab:ethnically_coded_articles",
  title = "Top 15 Ethnicities by Article Count (1907–1908)",
  out = "~/Dropbox/Apps/Overleaf/HPE Final Project/tables/ethnically_coded_articles.tex"
)

sum(is.na(all_articles$Ethnicity))
eth_month <- all_articles %>%
  mutate(date_clean = as.Date(str_replace(date, " .*$", "")),
         month = floor_date(date_clean, "month")) %>%
  count(month, Ethnicity, name = "articles") %>% 
  filter(Ethnicity != "") %>% # no ethnicity marker 
  group_by(Ethnicity) %>%
  mutate(total_articles = sum(articles)) %>%
  ungroup() %>%
  filter(total_articles >= 200)

distinct_cols <- c(
  "#1E90FF", # dodger blue
  "#000000", # black
  "#E60026", # red
  "#0047AB", # cobalt blue
  "#008000", # green
  "#FF8C00", # dark orange
  "#800080", # purple
  "#A52A2A", # brown
  "#00CED1", # dark turquoise
  "#FFD700", # gold
  "#708090", # slate gray
  "#FF1493", # deep pink
  "#2E8B57", # sea green
  "brown",
  "#B22222"  # firebrick
)

eth_month <- eth_month %>%
  group_by(Ethnicity) %>%
  mutate(index = articles / mean(articles)) %>%
  ungroup()

p <- ggplot(eth_month, aes(x = month, y = articles)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~ Ethnicity, scales = "free_y") +
  scale_x_date(
    breaks = as.Date(c("1907-01-01", "1908-01-01")),
    date_labels = "%b %Y",
    expand = expansion(mult = c(0.02, 0.02))
  )+
  annotate(
    "rect",
    xmin = as.Date("1907-10-01"),
    xmax = as.Date("1908-02-01"),
    ymin = -Inf,
    ymax = Inf,
    fill = "grey80",
    alpha = 0.4
  ) +
  labs(
    x = "Month",
    y = "Monthly number of articles"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 11),
    panel.spacing = unit(1, "lines")
  )

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/HPE Final Project/figures/ethnic_articles_by_month.pdf",
       device = pdf, bg = "white", width = 6, height = 5, units = "in")

# consider geographic locations  ------------------------------------------------

region_map_full <- list(
  "Northeast" = c("Maine","New Hampshire","Vermont","Massachusetts","Rhode Island","Connecticut",
                  "New York","New Jersey","Pennsylvania"),
  
  "Midwest"   = c("Ohio","Indiana","Illinois","Michigan","Wisconsin",
                  "Minnesota","Iowa","Missouri","North Dakota","South Dakota",
                  "Nebraska","Kansas"),
  
  "South"     = c("Delaware","Maryland","District of Columbia","Virginia","West Virginia",
                  "North Carolina","South Carolina","Georgia","Florida",
                  "Kentucky","Tennessee","Mississippi","Alabama",
                  "Oklahoma","Texas","Arkansas","Louisiana"),
  
  "West"      = c("Montana","Idaho","Wyoming","Colorado","New Mexico","Arizona","Utah","Nevada",
                  "Washington","Oregon","California")
)

state_to_region <- stack(region_map_full)
colnames(state_to_region) <- c("state","region")

df_geo <- all_articles %>%
  inner_join(state_to_region, by = c("State" = "state")) %>%
  mutate(month = as.Date(format(as.Date(date), "%Y-%m-01"))) %>%
  count(region, month, name = "articles")

papers_per_region <- all_articles %>% ## consider OVERALL number of newspapers -- perhaps south had more  
  distinct(State, Newspapers) %>% 
  inner_join(state_to_region, by = c("State" = "state")) %>%
  count(region, name = "n_papers")

df_geo <- all_articles %>% ### merge back in 
  inner_join(state_to_region, by = c("State" = "state")) %>%
  mutate(month = as.Date(format(as.Date(date), "%Y-%m-01"))) %>%
  count(region, month, name = "articles") %>%
  left_join(papers_per_region, by = "region") %>%
  mutate(articles_per_paper = articles / n_papers)

p <- ggplot(df_geo, aes(x = month, y = articles_per_paper, color = region)) +
  geom_line(size = 0.9) +
  geom_point(size = 1.4) +
  scale_color_manual(values = distinct_cols) +
  theme_bw() +
  annotate(
    "rect",
    xmin = panic_start,
    xmax = panic_end,
    ymin = -Inf, ymax = Inf,
    alpha = 0.2,
    fill = "grey70"
  ) + 
  scale_x_date(
    breaks = seq(as.Date("1907-01-01"), as.Date("1908-12-01"), by = "3 months"),
    date_labels = "%b\n%Y"
  )+
  labs(x = "", y = "Finance Articles Per Newspaper", color = "Region") +
  theme(
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 10),
    legend.key.size = unit(0.4, "lines"),
    panel.grid.minor = element_blank()
  )

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/HPE Final Project/figures/regions_articles_month.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

# -------------------------------------------------------------------------

table(all_articles$Languages) ## some of these are bilingual... 
lang_tab <- all_articles %>%
  count(Languages, name = "no_articles") %>%
  arrange(desc(no_articles)) %>% 
  mutate(Languages = ifelse(Languages == "", "Unspecified", Languages)) %>% 
  filter(no_articles >= 30)

stargazer(
  lang_tab, summary = FALSE, rownames = FALSE,
  type = "latex", header = FALSE,
  label = "tab:non_english_language",
  title = "Number of articles by language. 
  Non-English language newspapers still showed up in the initial keyword search.
  In certain cases, newspapers labeled as one language would still publish English to 
  their audience, or it may be that the keyword search hit the proper nouns. 
  Need to work on this as it creates bias. Languages with over 30 articles are kept in this table.",
  out = "~/Dropbox/Apps/Overleaf/HPE Final Project/tables/non_english_language.tex"
)

stargazer(
  lang_tab, summary = FALSE, rownames = FALSE,
  type = "latex", header = FALSE,
  font.size = "tiny",
  label = "tab:non_english_language",
  title = "Number of articles by language. 
  Non-English language newspapers still showed up in the initial keyword search.
  In certain cases, newspapers labeled as one language would still publish English to 
  their audience, or it may be that the keyword search hit the proper nouns. 
  Need to work on this as it creates bias. Languages with over 30 articles are kept in this table.",
  out = "~/Dropbox/Apps/Overleaf/HPE Final Project/pres_only/non_english_language.tex"
)

# consider french language ------------------------------------------------

french_articles <- all_articles %>% 
  filter(Languages == "French")

french_articles$article

# number of unique newspapers ---------------------------------------------

unique_newspapers <- as.data.frame(unique(all_articles$Newspapers))
