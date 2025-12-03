#### data from reinhart and rogoff 2014 

rm(list = ls())
library(readr)
library(readxl)

# load data ---------------------------------------------------------------

reinhart_rogoff_us <- read_excel("~/Dropbox/Columbia/Spring 2025/Game Theory/R/P2014_1107_Reinhart_Rogoff_data..xls", 
                                               sheet = "US")
reinhart_rogoff_us <- reinhart_rogoff_us %>% 
  select(1, 7)  # hard code 
colnames(reinhart_rogoff_us) <- c("year", "dev_prior_peak")
reinhart_rogoff_us <- reinhart_rogoff_us %>% 
  mutate(year = as.numeric(year), 
         dev_prior_peak = as.numeric(dev_prior_peak)) %>% 
  filter(!is.na(year))

min(reinhart_rogoff_us$year)
year_breaks <- seq(min(reinhart_rogoff_us$year), 
                   max(reinhart_rogoff_us$year), by = 15)

annotated_years <- data.frame(
  year = c(1815, 1839, 1873, 1893, 1907, 1920, 1931, 2009),
  label = c("1814–1817", "1837–1840s", "1873", "1893", "1907", "1920", "1929–1933", "2008–2009"),
  y = c(-6, -5, -4, -13, -10, -7, -28, -5)  # y-positions for labels
)
annotated_years$label <- gsub("–", "-", annotated_years$label)

p <- ggplot(reinhart_rogoff_us, aes(x = year, y = dev_prior_peak)) +
  geom_area(fill = "#4B6C8A", alpha = 0.8) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  scale_y_continuous(name = "Percent", limits = c(-30, 10), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1790, 2010, by = 15), expand = c(0, 0)) +
  geom_text(data = annotated_years,
            aes(x = year, y = y, label = label),
            inherit.aes = FALSE,
            size = 3) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 16)
  )

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/Competing Narratives of Financial Crises/figures/us_banking_crisis_gdp.pdf",
       device = pdf, bg = "white", width = 6, height = 3.34, units = "in")

# table 1 -----------------------------------------------------------------

reinhart_table_1 <- read_excel("~/Dropbox/Columbia/Spring 2025/Game Theory/R/P2014_1107_Reinhart_Rogoff_data..xls", 
                                 sheet = "Table_1") 

reinhart_table_1 <- reinhart_table_1 %>% 
  select(1:3, 7)
colnames(reinhart_table_1) <- c("index", "year", "country", "severity_index")

reinhart_table_1 <- reinhart_table_1 %>% 
  slice(5:39) %>% 
  mutate(severity_index = as.numeric(severity_index), 
         year_country = paste(year, country),
         year_country = fct_reorder(year_country, severity_index))
        
p <- ggplot(reinhart_table_1, aes(x = year_country, y = severity_index)) +
  geom_col(fill = "#4B6C8A") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Severity Index",
    title = "", 
    family = "serif"
  ) +
  scale_y_continuous(limits = c(0, 70), expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8)
  )
ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/Competing Narratives of Financial Crises/figures/worst_crises_severity.pdf",
       device = pdf, bg = "white", width = 4.88, height = 5, units = "in")
