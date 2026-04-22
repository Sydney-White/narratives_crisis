rm(list = ls())

library(readr)
library(dplyr)

input_dir <- "Data/data_outputs/newspapers/chronicling_merge"
output_dir <- "Data/data_outputs/newspapers/chronicling_merge"

best_matches <- read_csv(
  file.path(input_dir, "chronicling_to_gentzkow_best_state_match_per_lccn.csv"),
  show_col_types = FALSE
)

exact_matches <- read_csv(
  file.path(input_dir, "chronicling_to_gentzkow_exact_unique_matches.csv"),
  show_col_types = FALSE
)

final_matches <- best_matches %>%
  filter(confidence_tier %in% c("high", "medium")) %>%
  mutate(match_source = paste0("best_", confidence_tier))

exact_matches_clean <- exact_matches %>%
  mutate(
    confidence_tier = "exact",
    match_source = "exact_unique",
    final_match_type = "exact",
    in_exact = TRUE
  )

final_matches <- final_matches %>%
  left_join(
    exact_matches_clean %>%
      distinct(LCCN) %>%
      mutate(in_exact = TRUE),
    by = "LCCN"
  ) %>%
  mutate(
    in_exact = if_else(is.na(in_exact), FALSE, in_exact),
    final_match_type = case_when(
      in_exact ~ "exact",
      confidence_tier == "high" ~ "fuzzy_high",
      confidence_tier == "medium" ~ "fuzzy_medium",
      TRUE ~ "other"
    )
  ) %>%
  filter(!in_exact) %>%
  bind_rows(exact_matches_clean) %>%
  arrange(desc(in_exact), confidence_tier, State, City, Newspapers)

summary_tbl <- final_matches %>%
  count(final_match_type, name = "n_matches")

write_csv(
  final_matches,
  file.path(output_dir, "chronicling_to_gentzkow_final_matches.csv"),
  na = ""
)

write_csv(
  summary_tbl,
  file.path(output_dir, "chronicling_to_gentzkow_final_match_summary.csv"),
  na = ""
)

cat("Wrote final match file to", file.path(output_dir, "chronicling_to_gentzkow_final_matches.csv"), "\n")
cat("Total final matches:", nrow(final_matches), "\n")
print(summary_tbl)
