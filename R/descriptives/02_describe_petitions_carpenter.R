### look at petitions  

rm(list = ls())
library(tidyverse)
library(stargazer)
library(stringr)
library(xtable)

# load data ---------------------------------------------------------------

load("Data/data_inputs/petitions_carpenter/petitions.RData")

petitions <- congress.final %>% 
  filter(congress %in% c(59:63)) %>% 
  filter(topic_label %in% c("Banking and Finance"))

### first need to narrow down to ones actuallt related to banking - owens, aldrich, bank, 

petitions_qs <- petitions %>%
  mutate(
    quarter = paste0(year(date), " Q", quarter(date))
  ) %>%
  count(state, quarter) %>%
  arrange(quarter, desc(n))

bank_keywords <- c(
  "bank", "banking", "currency", "monetary",
  "postal savings", "postal-savings", "savings bank",
  "deposit", "guarant", "guaranty",
  "Aldrich", "National Monetary",
  "central bank", "credit note", "loan",
  "capital", "surplus"
)

petitions <- petitions %>%
  mutate(
    prayer_clean = str_to_lower(prayer),
    relevant = str_detect(prayer_clean, str_c(bank_keywords, collapse = "|"))
  ) %>% 
  filter(relevant==TRUE)

petitions$prayer
petitions$petitioner

petitions_small <- petitions %>% 
  select(date, petitioner, prayer)

write.csv(petitions_small, "Data/data_outputs/banking_petitions.csv")

# parse date
df <- petitions_small %>%
  mutate(date = ymd(date))

# classify petitioner group
df <- df %>%
  mutate(
    group = case_when(
      str_detect(str_to_lower(petitioner), "chamber|commercial club|board of trade") ~
        "Chambers / Business Associations",
      str_detect(str_to_lower(petitioner), "bank|banks|clearing house") ~
        "Banks / Financial Institutions",
      str_detect(str_to_lower(petitioner), "citizen|citizens") ~
        "Citizens",
      TRUE ~
        "Other / Mixed"
    )
  )

# classify issue type from prayer text
df <- df %>%
  mutate(
    issue = case_when(
      str_detect(str_to_lower(prayer), "postal savings") ~
        "Postal savings",
      str_detect(str_to_lower(prayer), "against|opposition") ~
        "Opposition",
      str_detect(str_to_lower(prayer), "amend|amendment") ~
        "Amendments to law",
      str_detect(str_to_lower(prayer), "reform|system|legislation") ~
        "Systemic banking reform",
      TRUE ~
        "Other"
    )
  )

# construct petition issue × petitioner group table
issue_table <- df %>%
  count(group, issue) %>%
  pivot_wider(
    names_from = issue,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(group)

stargazer(
  issue_table,
  summary = FALSE,
  rownames = FALSE,
  type = "latex",
  title = "Petition Issue by Petitioner Group",
  out = "~/Dropbox/Apps/Overleaf/1907 Bankers Panic/appendix_tables/reform_by_group.tex", 
  digits = 0
)
