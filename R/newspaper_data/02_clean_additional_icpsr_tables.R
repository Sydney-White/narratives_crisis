rm(list = ls())

library(readr)
library(dplyr)
library(stringr)

input_dir <- "Data/ICPSR_30261"
output_dir <- "Data/data_outputs/newspapers/icpsr_30261"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

load(file.path(input_dir, "DS0001/30261-0001-Data.rda"))
load(file.path(input_dir, "DS0002/30261-0002-Data.rda"))
load(file.path(input_dir, "DS0003/30261-0003-Data.rda"))
load(file.path(input_dir, "DS0004/30261-0004-Data.rda"))
load(file.path(input_dir, "DS0005/30261-0005-Data.rda"))
load(file.path(input_dir, "DS0006/30261-0006-Data.rda"))
load(file.path(input_dir, "DS0007/30261-0007-Data.rda"))
load(file.path(input_dir, "DS0008/30261-0008-Data.rda"))
load(file.path(input_dir, "DS0009/30261-0009-Data.rda"))

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
    citypermid = as.integer(CITYPERMID),
    paper_name_constant = clean_text(PAPERNAME_CONSTANT),
    paper_name_constant_key = make_key(PAPERNAME_CONSTANT),
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

base_panel <- da30261.0002 %>%
  transmute(
    recid = as.integer(RECID),
    permid = as.integer(PERMID),
    year = as.integer(YEAR),
    paper_name = clean_text(PAPERNAME),
    paper_name_key = make_key(PAPERNAME),
    polaff = na_if(str_trim(as.character(POLAFF)), ""),
    circ = suppressWarnings(as.numeric(CIRC)),
    party_endorsement = clean_text(PARTY_ENDORSEMENT)
  ) %>%
  left_join(panel_lookup, by = "permid") %>%
  left_join(city_lookup, by = "citypermid") %>%
  mutate(
    paper_name_final = coalesce(paper_name, paper_name_constant),
    paper_name_final_key = coalesce(paper_name_key, paper_name_constant_key)
  )

entry_exit_panel <- da30261.0003 %>%
  transmute(
    recid = as.integer(RECID),
    permid = as.integer(PERMID),
    year = as.integer(YEAR),
    cnty90 = as.integer(CNTY90),
    exit_flag = as.integer(EXIT),
    merge_out_flag = as.integer(MERGE_OUT),
    entry_flag = as.integer(ENTRY),
    merge_in_flag = as.integer(MERGE_IN)
  ) %>%
  left_join(base_panel %>%
              select(recid, permid, year, paper_name_final, city_name_constant, state, cnty90_panel = cnty90),
            by = c("recid", "permid", "year")) %>%
  mutate(cnty90 = coalesce(cnty90, cnty90_panel)) %>%
  select(-cnty90_panel)

subscription_price_panel <- da30261.0004 %>%
  transmute(
    recid = as.integer(RECID),
    subpricetype = clean_text(SUBPRICETYPE),
    subpricetype_label = strip_code_label(SUBPRICETYPE),
    subprice = suppressWarnings(as.numeric(SUBPRICE))
  ) %>%
  left_join(base_panel %>%
              select(recid, permid, year, paper_name_final, city_name_constant, state, polaff, circ),
            by = "recid") %>%
  arrange(year, state, city_name_constant, paper_name_final)

advertising_price_panel <- da30261.0005 %>%
  transmute(
    recid = as.integer(RECID),
    adpricetype = clean_text(ADPRICETYPE),
    adpricetype_label = strip_code_label(ADPRICETYPE),
    adprice = suppressWarnings(as.numeric(ADPRICE)),
    addepth = clean_text(ADDEPTH),
    adtype = clean_text(ADTYPE)
  ) %>%
  left_join(base_panel %>%
              select(recid, permid, year, paper_name_final, city_name_constant, state, polaff, circ),
            by = "recid") %>%
  arrange(year, state, city_name_constant, paper_name_final)

city_year_panel <- da30261.0007 %>%
  transmute(
    citypermid = as.integer(CITYPERMID),
    year = as.integer(YEAR),
    city_population_imputed = suppressWarnings(as.numeric(ICITYPOP)),
    num_dailies = suppressWarnings(as.numeric(NUMDAILIES)),
    circulation_total = suppressWarnings(as.numeric(CIRC)),
    circulation_republican = suppressWarnings(as.numeric(CIRC_POLAFF_R)),
    circulation_independent = suppressWarnings(as.numeric(CIRC_POLAFF_I)),
    circulation_democratic = suppressWarnings(as.numeric(CIRC_POLAFF_D)),
    circulation_nonpolitical = suppressWarnings(as.numeric(CIRC_POLAFF_NONE)),
    circulation_ex_last = suppressWarnings(as.numeric(CIRC_EXLAST)),
    circulation_ex_next = suppressWarnings(as.numeric(CIRC_EXNEXT)),
    missing_circulation = suppressWarnings(as.numeric(MISS_CIRC)),
    num_morning = suppressWarnings(as.numeric(NUM_WHEN_M)),
    num_evening = suppressWarnings(as.numeric(NUM_WHEN_E)),
    num_afternoon = suppressWarnings(as.numeric(NUM_WHEN_AD)),
    num_when_missing = suppressWarnings(as.numeric(NUM_WHEN_MISS)),
    num_republican = suppressWarnings(as.numeric(NUM_POLAFF_R)),
    num_democratic = suppressWarnings(as.numeric(NUM_POLAFF_D)),
    num_independent = suppressWarnings(as.numeric(NUM_POLAFF_I)),
    num_nonpolitical = suppressWarnings(as.numeric(NUM_POLAFF_NONE)),
    num_nonpol = suppressWarnings(as.numeric(NUM_NONPOL))
  ) %>%
  left_join(city_lookup, by = "citypermid") %>%
  arrange(year, state, city_name_constant)

city_population_panel <- da30261.0008 %>%
  transmute(
    caseid = as.integer(CASEID),
    citypermid = as.integer(CITYPERMID),
    year = as.integer(YEAR),
    city_population = suppressWarnings(as.numeric(CITYPOP)),
    loc_estimate = clean_text(LOCEST)
  ) %>%
  left_join(city_lookup, by = "citypermid") %>%
  arrange(year, state, city_name_constant)

county_market_structure <- da30261.0009 %>%
  transmute(
    cnty90 = as.integer(CNTY90),
    max_num_dailies = suppressWarnings(as.numeric(MAXNUMDAILIES)),
    num_markets = suppressWarnings(as.numeric(NUMMARKETS)),
    metarea1900 = suppressWarnings(as.numeric(METAREA1900)),
    central_city1900 = suppressWarnings(as.numeric(CENTRAL_CITY1900))
  ) %>%
  arrange(cnty90)

price_summary_1907_1908 <- subscription_price_panel %>%
  filter(year %in% c(1907L, 1908L)) %>%
  group_by(year, subpricetype_label) %>%
  summarise(
    newspapers = n(),
    mean_subprice = mean(subprice, na.rm = TRUE),
    median_subprice = median(subprice, na.rm = TRUE),
    p90_subprice = quantile(subprice, probs = 0.9, na.rm = TRUE),
    .groups = "drop"
  )

adprice_summary_1907_1908 <- advertising_price_panel %>%
  filter(year %in% c(1907L, 1908L)) %>%
  group_by(year, adpricetype_label, adtype) %>%
  summarise(
    newspapers = n(),
    mean_adprice = mean(adprice, na.rm = TRUE),
    median_adprice = median(adprice, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(entry_exit_panel, file.path(output_dir, "gentzkow_entry_exit_panel.csv"), na = "")
write_csv(subscription_price_panel, file.path(output_dir, "gentzkow_subscription_price_panel.csv"), na = "")
write_csv(advertising_price_panel, file.path(output_dir, "gentzkow_advertising_price_panel.csv"), na = "")
write_csv(city_year_panel, file.path(output_dir, "gentzkow_city_year_panel.csv"), na = "")
write_csv(city_population_panel, file.path(output_dir, "gentzkow_city_population_panel.csv"), na = "")
write_csv(county_market_structure, file.path(output_dir, "gentzkow_county_market_structure.csv"), na = "")
write_csv(price_summary_1907_1908, file.path(output_dir, "gentzkow_subscription_price_summary_1907_1908.csv"), na = "")
write_csv(adprice_summary_1907_1908, file.path(output_dir, "gentzkow_adprice_summary_1907_1908.csv"), na = "")

cat("Wrote additional ICPSR tables to", output_dir, "\n")
cat("Subscription-price rows:", nrow(subscription_price_panel), "\n")
cat("Advertising-price rows:", nrow(advertising_price_panel), "\n")
cat("City-year rows:", nrow(city_year_panel), "\n")
cat("County-market rows:", nrow(county_market_structure), "\n")
