rm(list = ls())

library(readr)
library(dplyr)
library(stringr)
library(lubridate)

gentzkow_dir <- "Data/data_outputs/newspapers/icpsr_30261"
chronicling_path <- "Data/data_inputs/chronicling-america.csv"
output_dir <- "Data/data_outputs/newspapers/chronicling_merge"
window_start <- 1900L
window_end <- 1920L

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

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
    str_replace_all("\\bsaint\\b", "st") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish() %>%
    na_if("")
}

make_title_key <- function(x) {
  x %>%
    make_key() %>%
    str_replace_all("\\b(the|daily|weekly|evening|morning|sunday|tri weekly|semi weekly|triweekly|semiweekly|afternoon)\\b", " ") %>%
    str_squish() %>%
    na_if("")
}

make_base_city_key <- function(x) {
  x %>%
    clean_text() %>%
    str_replace_all("\\bsaint\\b", "st") %>%
    str_split("\\s*-\\s*|\\s*/\\s*") %>%
    lapply(function(parts) parts[[1]][1]) %>%
    unlist() %>%
    make_key()
}

strip_city_from_title <- function(title_key, city_key, base_city_key) {
  out <- title_key
  out <- ifelse(
    !is.na(out) & !is.na(city_key) & city_key != "",
    str_squish(str_replace_all(out, paste0("\\b", city_key, "\\b"), " ")),
    out
  )
  out <- ifelse(
    !is.na(out) & !is.na(base_city_key) & base_city_key != "",
    str_squish(str_replace_all(out, paste0("\\b", base_city_key, "\\b"), " ")),
    out
  )
  out <- str_replace_all(out, "\\b(and)\\b", " ")
  out <- str_squish(out)
  na_if(out, "")
}

token_overlap_score <- function(a, b) {
  a_tokens <- unique(str_split(ifelse(is.na(a), "", a), "\\s+")[[1]])
  b_tokens <- unique(str_split(ifelse(is.na(b), "", b), "\\s+")[[1]])
  a_tokens <- a_tokens[a_tokens != ""]
  b_tokens <- b_tokens[b_tokens != ""]
  if (length(a_tokens) == 0 || length(b_tokens) == 0) {
    return(0)
  }
  intersection <- length(intersect(a_tokens, b_tokens))
  union_n <- length(union(a_tokens, b_tokens))
  if (union_n == 0) {
    return(0)
  }
  intersection / union_n
}

first_non_missing <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) {
    return(NA)
  }
  x[[1]]
}

clean_chronicling_title <- function(x) {
  x %>%
    clean_text() %>%
    str_replace("\\s*\\([^\\)]*\\)\\s*\\d{1,4}\\?{0,3}-\\d{1,4}\\??\\s*$", "") %>%
    str_replace("\\s*\\([^\\)]*\\)\\s*\\d{4}-\\d{4}\\s*$", "") %>%
    str_replace("\\s*\\([^\\)]*\\)\\s*$", "") %>%
    str_replace("\\s+\\d{1,4}\\?{0,3}-\\d{1,4}\\??\\s*$", "") %>%
    str_replace("\\s+\\d{4}-\\d{4}\\s*$", "") %>%
    str_squish() %>%
    na_if("")
}

parse_chronicling_date <- function(x) {
  x <- str_squish(as.character(x))
  x[x %in% c("", "NA")] <- NA_character_

  out <- rep(as.Date(NA), length(x))

  iso_idx <- !is.na(x) & str_detect(x, "^\\d{4}-\\d{2}-\\d{2}$")
  out[iso_idx] <- ymd(x[iso_idx], quiet = TRUE)

  slash4_idx <- !is.na(x) & str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{4}$")
  out[slash4_idx] <- mdy(x[slash4_idx], quiet = TRUE)

  slash2_idx <- !is.na(x) & str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{2}$")
  if (any(slash2_idx)) {
    parts <- str_split(x[slash2_idx], "/", simplify = TRUE)
    month <- suppressWarnings(as.integer(parts[, 1]))
    day <- suppressWarnings(as.integer(parts[, 2]))
    year2 <- suppressWarnings(as.integer(parts[, 3]))
    year_full <- ifelse(year2 <= 30, 1900 + year2, 1800 + year2)
    out[slash2_idx] <- make_date(year = year_full, month = month, day = day)
  }

  out
}

extract_issue_year <- function(x) {
  x <- str_squish(as.character(x))
  x[x %in% c("", "NA")] <- NA_character_

  out <- rep(NA_integer_, length(x))

  iso_idx <- !is.na(x) & str_detect(x, "^\\d{4}-\\d{2}-\\d{2}$")
  out[iso_idx] <- suppressWarnings(as.integer(str_sub(x[iso_idx], 1, 4)))

  slash4_idx <- !is.na(x) & str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{4}$")
  out[slash4_idx] <- suppressWarnings(as.integer(str_sub(x[slash4_idx], -4, -1)))

  slash2_idx <- !is.na(x) & str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{2}$")
  out[slash2_idx] <- ifelse(
    suppressWarnings(as.integer(str_sub(x[slash2_idx], -2, -1))) <= 30,
    1900 + suppressWarnings(as.integer(str_sub(x[slash2_idx], -2, -1))),
    1800 + suppressWarnings(as.integer(str_sub(x[slash2_idx], -2, -1)))
  )

  out
}

gentzkow <- read_csv(
  file.path(gentzkow_dir, "gentzkow_newspaper_panel_clean.csv"),
  show_col_types = FALSE
) %>%
  filter(year >= window_start, year <= window_end) %>%
  mutate(
    state_key = make_key(state),
    city_key = make_key(city_name_constant),
    base_city_key = make_base_city_key(city_name_constant),
    paper_key_merge = make_title_key(paper_name_final),
    paper_key_core = strip_city_from_title(paper_key_merge, city_key, base_city_key)
  ) %>%
  arrange(permid, year) %>%
  group_by(permid) %>%
  summarise(
    gentzkow_year_min = min(year, na.rm = TRUE),
    gentzkow_year_max = max(year, na.rm = TRUE),
    gentzkow_n_years = n_distinct(year),
    paper_name_final = first_non_missing(paper_name_final),
    city_name_constant = first_non_missing(city_name_constant),
    state = first_non_missing(state),
    paper_key_merge = first_non_missing(paper_key_merge),
    paper_key_core = first_non_missing(paper_key_core),
    city_key = first_non_missing(city_key),
    base_city_key = first_non_missing(base_city_key),
    state_key = first_non_missing(state_key),
    polaff = first_non_missing(polaff),
    political_label = first_non_missing(political_label),
    subprice = suppressWarnings(mean(subprice, na.rm = TRUE)),
    party_endorsement_label = first_non_missing(party_endorsement_label),
    .groups = "drop"
  ) %>%
  mutate(subprice = if_else(is.nan(subprice), NA_real_, subprice))

chronicling <- read_csv(chronicling_path, show_col_types = FALSE) %>%
  mutate(
    first_issue_date = parse_chronicling_date(`First Issue`),
    last_issue_date = parse_chronicling_date(`Last Issue`),
    first_issue_year = extract_issue_year(`First Issue`),
    last_issue_year = extract_issue_year(`Last Issue`),
    newspaper_title_clean = clean_chronicling_title(Newspapers)
  ) %>%
  mutate(
    active_1900_1920 = case_when(
      !is.na(first_issue_date) & !is.na(last_issue_date) ~
        first_issue_date <= as.Date("1920-12-31") & last_issue_date >= as.Date("1900-01-01"),
      is.na(first_issue_date) & !is.na(last_issue_date) ~ last_issue_date >= as.Date("1900-01-01"),
      !is.na(first_issue_date) & is.na(last_issue_date) ~ first_issue_date <= as.Date("1920-12-31"),
      !is.na(first_issue_year) & !is.na(last_issue_year) ~ first_issue_year <= window_end & last_issue_year >= window_start,
      is.na(first_issue_year) & !is.na(last_issue_year) ~ last_issue_year >= window_start,
      !is.na(first_issue_year) & is.na(last_issue_year) ~ first_issue_year <= window_end,
      TRUE ~ FALSE
    ),
    state_key = make_key(State),
    city_key = make_key(City),
    base_city_key = make_base_city_key(City),
    paper_key_merge = make_title_key(newspaper_title_clean),
    paper_key_core = strip_city_from_title(paper_key_merge, city_key, base_city_key)
  ) %>%
  filter(active_1900_1920) %>%
  distinct(
    LCCN, Newspapers, newspaper_title_clean, State, City, County, `Geo Location`,
    state_key, city_key, base_city_key, paper_key_merge, paper_key_core, .keep_all = TRUE
  )

exact_matches <- chronicling %>%
  inner_join(
    gentzkow,
    by = c("state_key", "city_key", "paper_key_core"),
    relationship = "many-to-many"
  ) %>%
  mutate(match_type = "exact_city_state_title") %>%
  arrange(State, City, newspaper_title_clean, paper_name_final)

exact_unique <- exact_matches %>%
  group_by(LCCN) %>%
  filter(n() == 1) %>%
  ungroup()

matched_lccn <- exact_unique %>%
  distinct(LCCN)

unmatched_chronicling <- chronicling %>%
  anti_join(matched_lccn, by = "LCCN")

candidate_city_state <- unmatched_chronicling %>%
  inner_join(
    gentzkow %>%
      select(permid, gentzkow_year_min, gentzkow_year_max, gentzkow_n_years, paper_name_final, city_name_constant, state, state_key, city_key, base_city_key,
             paper_key_merge, paper_key_core, polaff, political_label, subprice, party_endorsement_label),
    by = c("state_key", "city_key"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    title_distance = stringdist::stringdist(paper_key_core.x, paper_key_core.y, method = "jw"),
    exact_title = paper_key_core.x == paper_key_core.y,
    token_overlap = mapply(token_overlap_score, paper_key_core.x, paper_key_core.y),
    match_score = title_distance - 0.45 * token_overlap
  ) %>%
  arrange(match_score, title_distance, desc(token_overlap), State, City, Newspapers) %>%
  group_by(LCCN) %>%
  slice_head(n = 5) %>%
  ungroup()

candidate_state_fuzzy <- unmatched_chronicling %>%
  inner_join(
    gentzkow %>%
      select(permid, gentzkow_year_min, gentzkow_year_max, gentzkow_n_years, paper_name_final, city_name_constant, state, state_key, city_key, base_city_key,
             paper_key_merge, paper_key_core, polaff, political_label, subprice, party_endorsement_label),
    by = "state_key",
    relationship = "many-to-many"
  ) %>%
  mutate(
    title_distance = stringdist::stringdist(paper_key_core.x, paper_key_core.y, method = "jw"),
    city_distance = stringdist::stringdist(base_city_key.x, base_city_key.y, method = "jw"),
    same_city = city_key.x == city_key.y | base_city_key.x == base_city_key.y,
    token_overlap = mapply(token_overlap_score, paper_key_core.x, paper_key_core.y),
    match_score = title_distance + 0.20 * city_distance - 0.45 * token_overlap - if_else(same_city, 0.25, 0)
  ) %>%
  arrange(match_score, title_distance, city_distance, desc(token_overlap), State, City, Newspapers) %>%
  group_by(LCCN) %>%
  slice_head(n = 8) %>%
  ungroup()

high_confidence_state_matches <- candidate_state_fuzzy %>%
  filter(
    (same_city & (title_distance <= 0.35 | token_overlap >= 0.34)) |
      (!same_city & title_distance <= 0.18 & token_overlap >= 0.55)
  ) %>%
  arrange(match_score, title_distance, city_distance, desc(token_overlap)) %>%
  group_by(LCCN) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(match_type = "state_fuzzy_chronicling_left")

medium_confidence_state_matches <- candidate_state_fuzzy %>%
  filter(
    (same_city & match_score <= 0.45) |
      (!same_city & match_score <= 0.25 & token_overlap > 0)
  ) %>%
  arrange(match_score, title_distance, city_distance, desc(token_overlap)) %>%
  group_by(LCCN) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(match_type = "state_fuzzy_medium")

best_state_match_per_lccn <- candidate_state_fuzzy %>%
  arrange(match_score, title_distance, city_distance, desc(token_overlap)) %>%
  group_by(LCCN) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(
    confidence_tier = case_when(
      LCCN %in% high_confidence_state_matches$LCCN ~ "high",
      LCCN %in% medium_confidence_state_matches$LCCN ~ "medium",
      TRUE ~ "low"
    )
  )

high_confidence_lccn_n <- if (is.data.frame(high_confidence_state_matches) && nrow(high_confidence_state_matches) > 0) {
  dplyr::n_distinct(high_confidence_state_matches[["LCCN"]])
} else {
  0L
}

medium_confidence_lccn_n <- if (is.data.frame(medium_confidence_state_matches) && nrow(medium_confidence_state_matches) > 0) {
  dplyr::n_distinct(medium_confidence_state_matches[["LCCN"]])
} else {
  0L
}

merge_summary <- tibble(
  chronicling_active_1900_1920 = nrow(chronicling),
  gentzkow_identity_1900_1920 = nrow(gentzkow),
  exact_match_rows = nrow(exact_matches),
  exact_unique_lccn = n_distinct(exact_unique$LCCN),
  unmatched_chronicling_rows = nrow(unmatched_chronicling),
  candidate_city_state_rows = nrow(candidate_city_state),
  candidate_state_fuzzy_rows = nrow(candidate_state_fuzzy),
  high_confidence_state_matches = nrow(high_confidence_state_matches),
  high_confidence_lccn = high_confidence_lccn_n,
  medium_confidence_lccn = medium_confidence_lccn_n,
  best_candidate_lccn = n_distinct(best_state_match_per_lccn$LCCN)
)

write_csv(exact_matches, file.path(output_dir, "chronicling_to_gentzkow_exact_matches.csv"), na = "")
write_csv(exact_unique, file.path(output_dir, "chronicling_to_gentzkow_exact_unique_matches.csv"), na = "")
write_csv(unmatched_chronicling, file.path(output_dir, "chronicling_unmatched_1900_1920.csv"), na = "")
write_csv(candidate_city_state, file.path(output_dir, "chronicling_to_gentzkow_city_state_candidates.csv"), na = "")
write_csv(candidate_state_fuzzy, file.path(output_dir, "chronicling_to_gentzkow_state_fuzzy_candidates.csv"), na = "")
write_csv(high_confidence_state_matches, file.path(output_dir, "chronicling_to_gentzkow_state_fuzzy_high_confidence.csv"), na = "")
write_csv(medium_confidence_state_matches, file.path(output_dir, "chronicling_to_gentzkow_state_fuzzy_medium_confidence.csv"), na = "")
write_csv(best_state_match_per_lccn, file.path(output_dir, "chronicling_to_gentzkow_best_state_match_per_lccn.csv"), na = "")
write_csv(merge_summary, file.path(output_dir, "chronicling_to_gentzkow_merge_summary.csv"), na = "")

cat("Wrote Chronicling America merge outputs to", output_dir, "\n")
print(merge_summary)
