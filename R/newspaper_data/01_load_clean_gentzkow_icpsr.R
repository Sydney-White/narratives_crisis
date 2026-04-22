rm(list = ls())

library(readr)
library(dplyr)
library(stringr)

input_dir <- "Data/ICPSR_30261"
output_dir <- "Data/data_outputs/newspapers/icpsr_30261"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

load(file.path(input_dir, "DS0001/30261-0001-Data.rda"))
load(file.path(input_dir, "DS0002/30261-0002-Data.rda"))
load(file.path(input_dir, "DS0006/30261-0006-Data.rda"))

clean_text <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_squish(x)
  na_if(x, "")
}

make_key <- function(x) {
  x %>%
    clean_text() %>%
    str_to_lower() %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish() %>%
    na_if("")
}

strip_code_label <- function(x) {
  x <- clean_text(x)
  x <- str_replace(x, "^\\([^\\)]+\\)\\s*", "")
  na_if(x, "")
}

panel_lookup <- da30261.0001 %>%
  transmute(
    permid = as.integer(PERMID),
    next_permid = as.integer(NEXTPERMID),
    citypermid = as.integer(CITYPERMID),
    paper_name_constant = clean_text(PAPERNAME_CONSTANT),
    paper_name_constant_key = make_key(PAPERNAME_CONSTANT),
    baseline_polaff = clean_text(POLAFF),
    baseline_when = clean_text(WHEN),
    nonpol = clean_text(NONPOL),
    min_year = as.integer(MIN_YEAR),
    max_year = as.integer(MAX_YEAR)
  )

city_lookup <- da30261.0006 %>%
  transmute(
    citypermid = as.integer(CITYPERMID),
    city_name_constant = clean_text(CITYNAME_CONSTANT),
    city_key = make_key(CITYNAME_CONSTANT),
    state = clean_text(STATE),
    state_key = make_key(STATE),
    cnty90 = as.integer(CNTY90)
  )

gentzkow_panel <- da30261.0002 %>%
  transmute(
    recid = as.integer(RECID),
    permid = as.integer(PERMID),
    year = as.integer(YEAR),
    when = clean_text(WHEN),
    paper_name = clean_text(PAPERNAME),
    paper_name_key = make_key(PAPERNAME),
    polaff_raw = clean_text(POLAFF),
    category = clean_text(CATEGORY),
    circ = suppressWarnings(as.numeric(CIRC)),
    circtype = clean_text(CIRCTYPE),
    circsworn = clean_text(CIRCSWORN),
    origpublisher = clean_text(ORIGPUBLISHER),
    subpricetype = clean_text(SUBPRICETYPE),
    subpricetype_label = strip_code_label(SUBPRICETYPE),
    subprice = suppressWarnings(as.numeric(SUBPRICE)),
    adprice = suppressWarnings(as.numeric(ADPRICE)),
    adpricetype = clean_text(ADPRICETYPE),
    addepth = clean_text(ADDEPTH),
    adtype = clean_text(ADTYPE),
    origpages = suppressWarnings(as.numeric(ORIGPAGES)),
    minpage = suppressWarnings(as.numeric(MINPAGE)),
    maxpage = suppressWarnings(as.numeric(MAXPAGE)),
    editors = clean_text(EDITORS),
    publishers = clean_text(PUBLISHERS),
    party_endorsement = clean_text(PARTY_ENDORSEMENT),
    party_endorsement_label = strip_code_label(PARTY_ENDORSEMENT),
    ownerpermid = as.integer(OWNERPERMID)
  ) %>%
  left_join(panel_lookup, by = "permid") %>%
  left_join(city_lookup, by = "citypermid") %>%
  mutate(
    paper_name_final = coalesce(paper_name, paper_name_constant),
    paper_name_final_key = coalesce(paper_name_key, paper_name_constant_key),
    polaff = na_if(str_trim(polaff_raw), ""),
    political_label = case_when(
      polaff == "D" ~ "Democratic",
      polaff == "R" ~ "Republican",
      polaff == "I" ~ "Independent",
      polaff == "ID" ~ "Independent Democrat",
      polaff == "IR" ~ "Independent Republican",
      polaff == "OP" ~ "Other Party",
      polaff == "UNK" ~ "Unknown",
      TRUE ~ NA_character_
    ),
    has_price_data = !is.na(subprice) | !is.na(adprice),
    has_partisan_data = !is.na(polaff) | !is.na(party_endorsement_label)
  ) %>%
  arrange(year, state, city_name_constant, paper_name_final)

gentzkow_1907_1908 <- gentzkow_panel %>%
  filter(year %in% c(1907L, 1908L))

summary_by_year <- gentzkow_panel %>%
  group_by(year) %>%
  summarise(
    newspapers = n(),
    unique_permid = n_distinct(permid, na.rm = TRUE),
    with_subprice = sum(!is.na(subprice)),
    with_adprice = sum(!is.na(adprice)),
    with_polaff = sum(!is.na(polaff)),
    with_party_endorsement = sum(!is.na(party_endorsement_label)),
    .groups = "drop"
  )

write_csv(gentzkow_panel, file.path(output_dir, "gentzkow_newspaper_panel_clean.csv"), na = "")
write_csv(gentzkow_1907_1908, file.path(output_dir, "gentzkow_newspaper_panel_1907_1908.csv"), na = "")
write_csv(panel_lookup, file.path(output_dir, "gentzkow_permid_lookup.csv"), na = "")
write_csv(city_lookup, file.path(output_dir, "gentzkow_city_lookup.csv"), na = "")
write_csv(summary_by_year, file.path(output_dir, "gentzkow_panel_year_summary.csv"), na = "")

cat("Wrote cleaned Gentzkow ICPSR files to", output_dir, "\n")
cat("Full linked panel rows:", nrow(gentzkow_panel), "\n")
cat("1907-1908 linked rows:", nrow(gentzkow_1907_1908), "\n")
cat("Rows with subscription prices:", sum(!is.na(gentzkow_panel$subprice)), "\n")
cat("Rows with political affiliation:", sum(!is.na(gentzkow_panel$polaff)), "\n")
