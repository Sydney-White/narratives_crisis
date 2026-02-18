################################################################################
# Run Descriptive Statistics on All Datasets
# 1907 Panic Paper Project
################################################################################

rm(list = ls())

# Load utilities
source("R/warp/descriptive_stats_utils.R")

# Output directory for descriptive reports
output_dir <- "Data/data_outputs/descriptive_reports"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# LOAD AND ANALYZE ALL CSV DATASETS
# =============================================================================

# --- Data Inputs ---

# Census data
if (file.exists("Data/data_inputs/all_census_normal.csv")) {
  census <- read.csv("Data/data_inputs/all_census_normal.csv")
  run_full_descriptives(census, "census", output_dir)
}

# Chronicling America (newspaper) data
if (file.exists("Data/data_inputs/chronicling-america.csv")) {
  chronicling <- read.csv("Data/data_inputs/chronicling-america.csv")
  run_full_descriptives(chronicling, "chronicling_america", output_dir)
}

# Digitized bank failures
if (file.exists("Data/data_inputs/digitized_bank_failures.csv")) {
  bank_failures <- read.csv("Data/data_inputs/digitized_bank_failures.csv")
  run_full_descriptives(bank_failures, "bank_failures", output_dir)
}

# Classified stage 2 output
if (file.exists("Data/data_inputs/classified_stage2_output.csv")) {
  classified <- read.csv("Data/data_inputs/classified_stage2_output.csv")
  run_full_descriptives(classified, "classified_stage2", output_dir)
}

# --- Data Outputs ---

# Clean election data
if (file.exists("Data/data_outputs/clean_election_data.csv")) {
  elections <- read.csv("Data/data_outputs/clean_election_data.csv")
  run_full_descriptives(elections, "elections", output_dir)
}

# =============================================================================
# QUICK SUMMARY OF ALL DATASETS
# =============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("QUICK COMPARISON ACROSS DATASETS\n")
cat(strrep("=", 60), "\n\n")

# List all CSVs
csv_files <- c(
  list.files("Data/data_inputs", pattern = "\\.csv$", full.names = TRUE),
  list.files("Data/data_outputs", pattern = "\\.csv$", full.names = TRUE)
)

for (f in csv_files) {
  tryCatch({
    df <- read.csv(f)
    cat(sprintf("%-50s | %6d obs | %3d vars\n", 
                basename(f), nrow(df), ncol(df)))
  }, error = function(e) {
    cat(sprintf("%-50s | ERROR: %s\n", basename(f), e$message))
  })
}

cat("\n")
cat("Descriptive reports saved to:", output_dir, "\n")

# =============================================================================
# EXAMPLE USAGE FOR INDIVIDUAL ANALYSIS
# =============================================================================

# To analyze a specific dataset interactively:
#
# source("R/descriptive_stats_utils.R")
# 
# df <- read.csv("Data/data_outputs/clean_election_data.csv")
#
# # Quick summary
# quick_summary(df)
#
# # Full summary stats
# get_summary_stats(df)
#
# # Missing data
# print_missing_summary(df)
#
# # Correlations (select specific vars)
# get_correlations(df, vars = c("rep_swing_1904_1908", "anti_rep_swing_1904_1908"))
# plot_correlations(df)
#
# # Outliers
# detect_outliers_iqr(df)
# detect_outliers_zscore(df)
#
# # Distributions
# plot_distributions(df)
# plot_boxplots(df)
#
# # Export to LaTeX
# export_summary_latex(df, out_path = "output/summary_table.tex")
