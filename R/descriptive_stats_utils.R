################################################################################
# Descriptive Statistics Utilities
# Comprehensive functions for exploring datasets
################################################################################

library(tidyverse)
library(stargazer)
library(moments)
library(corrplot)

# =============================================================================
# 1. SUMMARY STATISTICS
# =============================================================================

#' Generate comprehensive summary statistics for numeric variables
#' @param df A data frame
#' @param vars Optional character vector of variable names (defaults to all numeric)
#' @return A tibble with summary statistics
get_summary_stats <- function(df, vars = NULL) {
  
  if (is.null(vars)) {
    vars <- names(df)[sapply(df, is.numeric)]
  }
  
  df %>%
    select(all_of(vars)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    group_by(variable) %>%
    summarise(
      n = sum(!is.na(value)),
      missing = sum(is.na(value)),
      missing_pct = round(100 * mean(is.na(value)), 2),
      mean = round(mean(value, na.rm = TRUE), 3),
      sd = round(sd(value, na.rm = TRUE), 3),
      min = round(min(value, na.rm = TRUE), 3),
      p25 = round(quantile(value, 0.25, na.rm = TRUE), 3),
      median = round(median(value, na.rm = TRUE), 3),
      p75 = round(quantile(value, 0.75, na.rm = TRUE), 3),
      max = round(max(value, na.rm = TRUE), 3),
      skewness = round(moments::skewness(value, na.rm = TRUE), 3),
      kurtosis = round(moments::kurtosis(value, na.rm = TRUE), 3)
    ) %>%
    arrange(variable)
}

#' Quick summary table (fewer stats, cleaner output)
#' @param df A data frame
#' @param vars Optional character vector of variable names
quick_summary <- function(df, vars = NULL) {
  
  if (is.null(vars)) {
    vars <- names(df)[sapply(df, is.numeric)]
  }
  
  df %>%
    select(all_of(vars)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    group_by(variable) %>%
    summarise(
      n = sum(!is.na(value)),
      mean = round(mean(value, na.rm = TRUE), 2),
      sd = round(sd(value, na.rm = TRUE), 2),
      min = round(min(value, na.rm = TRUE), 2),
      max = round(max(value, na.rm = TRUE), 2)
    )
}

# =============================================================================
# 2. MISSING DATA ANALYSIS
# =============================================================================

#' Analyze missing data patterns
#' @param df A data frame
#' @return A list with missing data statistics and patterns
analyze_missing <- function(df) {
  
  # Per-variable missing
  var_missing <- df %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
    mutate(
      pct_missing = round(100 * n_missing / nrow(df), 2),
      n_total = nrow(df)
    ) %>%
    arrange(desc(pct_missing))
  
  # Per-row missing
  row_missing <- tibble(
    n_missing_vars = rowSums(is.na(df))
  ) %>%
    count(n_missing_vars, name = "n_rows") %>%
    mutate(pct_rows = round(100 * n_rows / nrow(df), 2))
  
  # Complete cases
  n_complete <- sum(complete.cases(df))
  
  list(
    by_variable = var_missing,
    by_row = row_missing,
    n_complete = n_complete,
    pct_complete = round(100 * n_complete / nrow(df), 2),
    n_total = nrow(df)
  )
}

#' Print missing data summary
#' @param df A data frame
print_missing_summary <- function(df) {
  missing <- analyze_missing(df)
  
  cat("\n========== MISSING DATA SUMMARY ==========\n")
  cat(sprintf("Total observations: %d\n", missing$n_total))
  cat(sprintf("Complete cases: %d (%.1f%%)\n", missing$n_complete, missing$pct_complete))
  
  cat("\n--- Variables with missing data ---\n")
  vars_with_missing <- missing$by_variable %>% filter(n_missing > 0)
  if (nrow(vars_with_missing) > 0) {
    print(vars_with_missing, n = 50)
  } else {
    cat("No missing data!\n")
  }
  
  invisible(missing)
}

# =============================================================================
# 3. CORRELATION ANALYSIS
# =============================================================================

#' Compute correlation matrix with p-values
#' @param df A data frame
#' @param vars Optional character vector of variable names
#' @param method Correlation method: "pearson", "spearman", or "kendall"
#' @return A list with correlation matrix and p-values
get_correlations <- function(df, vars = NULL, method = "pearson") {
  
  if (is.null(vars)) {
    vars <- names(df)[sapply(df, is.numeric)]
  }
  
  df_numeric <- df %>% select(all_of(vars))
  
  # Correlation matrix
  cor_mat <- cor(df_numeric, use = "pairwise.complete.obs", method = method)
  
  # P-values
  n <- nrow(df_numeric)
  p_mat <- matrix(NA, ncol(df_numeric), ncol(df_numeric))
  rownames(p_mat) <- colnames(p_mat) <- vars
  
  for (i in 1:(length(vars) - 1)) {
    for (j in (i + 1):length(vars)) {
      test <- cor.test(df_numeric[[vars[i]]], df_numeric[[vars[j]]], method = method)
      p_mat[i, j] <- p_mat[j, i] <- test$p.value
    }
  }
  
  list(
    correlation = round(cor_mat, 3),
    p_values = round(p_mat, 4)
  )
}

#' Plot correlation matrix
#' @param df A data frame
#' @param vars Optional character vector of variable names
#' @param method Correlation method
#' @param save_path Optional path to save the plot
plot_correlations <- function(df, vars = NULL, method = "pearson", save_path = NULL) {
  
  if (is.null(vars)) {
    vars <- names(df)[sapply(df, is.numeric)]
  }
  
  cor_result <- get_correlations(df, vars, method)
  
  if (!is.null(save_path)) {
    pdf(save_path, width = 10, height = 10)
  }
  
  corrplot::corrplot(
    cor_result$correlation,
    method = "color",
    type = "upper",
    addCoef.col = "black",
    tl.col = "black",
    tl.srt = 45,
    diag = FALSE,
    number.cex = 0.7,
    title = paste0(str_to_title(method), " Correlations"),
    mar = c(0, 0, 2, 0)
  )
  
  if (!is.null(save_path)) {
    dev.off()
    message("Correlation plot saved to: ", save_path)
  }
  
  invisible(cor_result)
}

# =============================================================================
# 4. OUTLIER DETECTION
# =============================================================================

#' Detect outliers using IQR method
#' @param df A data frame
#' @param vars Optional character vector of variable names
#' @param multiplier IQR multiplier (default 1.5)
#' @return A tibble with outlier counts per variable
detect_outliers_iqr <- function(df, vars = NULL, multiplier = 1.5) {
  
  if (is.null(vars)) {
    vars <- names(df)[sapply(df, is.numeric)]
  }
  
  outlier_counts <- map_dfr(vars, function(v) {
    x <- df[[v]]
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower <- q1 - multiplier * iqr
    upper <- q3 + multiplier * iqr
    
    tibble(
      variable = v,
      n_outliers = sum(x < lower | x > upper, na.rm = TRUE),
      n_low = sum(x < lower, na.rm = TRUE),
      n_high = sum(x > upper, na.rm = TRUE),
      pct_outliers = round(100 * n_outliers / sum(!is.na(x)), 2),
      lower_bound = round(lower, 3),
      upper_bound = round(upper, 3)
    )
  })
  
  outlier_counts %>% arrange(desc(n_outliers))
}

#' Detect outliers using Z-score method
#' @param df A data frame
#' @param vars Optional character vector of variable names
#' @param threshold Z-score threshold (default 3)
detect_outliers_zscore <- function(df, vars = NULL, threshold = 3) {
  
  if (is.null(vars)) {
    vars <- names(df)[sapply(df, is.numeric)]
  }
  
  map_dfr(vars, function(v) {
    x <- df[[v]]
    z <- scale(x)
    
    tibble(
      variable = v,
      n_outliers = sum(abs(z) > threshold, na.rm = TRUE),
      pct_outliers = round(100 * n_outliers / sum(!is.na(x)), 2),
      max_zscore = round(max(abs(z), na.rm = TRUE), 2)
    )
  }) %>% arrange(desc(n_outliers))
}

# =============================================================================
# 5. DISTRIBUTION ANALYSIS
# =============================================================================

#' Plot histograms for all numeric variables
#' @param df A data frame
#' @param vars Optional character vector of variable names
#' @param ncol Number of columns in facet grid
#' @param save_path Optional path to save the plot
plot_distributions <- function(df, vars = NULL, ncol = 3, save_path = NULL) {
  
  if (is.null(vars)) {
    vars <- names(df)[sapply(df, is.numeric)]
  }
  
  p <- df %>%
    select(all_of(vars)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    ggplot(aes(x = value)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
    facet_wrap(~variable, scales = "free", ncol = ncol) +
    theme_bw() +
    labs(x = NULL, y = "Count", title = "Distributions of Numeric Variables")
  
  if (!is.null(save_path)) {
    ggsave(save_path, p, width = 12, height = 8)
    message("Distribution plot saved to: ", save_path)
  }
  
  p
}

#' Plot boxplots for all numeric variables
#' @param df A data frame
#' @param vars Optional character vector of variable names
#' @param save_path Optional path to save the plot
plot_boxplots <- function(df, vars = NULL, save_path = NULL) {
  
  if (is.null(vars)) {
    vars <- names(df)[sapply(df, is.numeric)]
  }
  
  p <- df %>%
    select(all_of(vars)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    ggplot(aes(x = variable, y = value)) +
    geom_boxplot(fill = "steelblue", alpha = 0.7) +
    coord_flip() +
    theme_bw() +
    labs(x = NULL, y = "Value", title = "Boxplots of Numeric Variables")
  
  if (!is.null(save_path)) {
    ggsave(save_path, p, width = 10, height = max(4, length(vars) * 0.4))
    message("Boxplot saved to: ", save_path)
  }
  
  p
}

# =============================================================================
# 6. TABLE OUTPUT (LaTeX/HTML)
# =============================================================================

#' Export summary statistics to LaTeX using stargazer
#' @param df A data frame
#' @param vars Optional character vector of variable names
#' @param out_path Path for output file (optional)
#' @param title Table title
export_summary_latex <- function(df, vars = NULL, out_path = NULL, title = "Summary Statistics") {
  
  if (is.null(vars)) {
    vars <- names(df)[sapply(df, is.numeric)]
  }
  
  stargazer(
    as.data.frame(df[vars]),
    type = if (is.null(out_path)) "text" else "latex",
    out = out_path,
    title = title,
    summary = TRUE,
    header = FALSE,
    digits = 2
  )
}

# =============================================================================
# 7. FULL DESCRIPTIVE REPORT
# =============================================================================

#' Run full descriptive analysis on a dataset
#' @param df A data frame
#' @param name Name for the dataset (used in output)
#' @param output_dir Directory to save outputs (optional)
run_full_descriptives <- function(df, name = "dataset", output_dir = NULL) {
  
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("DESCRIPTIVE ANALYSIS:", name, "\n")
  cat(strrep("=", 60), "\n")
  
  # Basic info
  cat("\n--- Dataset Overview ---\n")
  cat(sprintf("Observations: %d\n", nrow(df)))
  cat(sprintf("Variables: %d\n", ncol(df)))
  cat(sprintf("Numeric variables: %d\n", sum(sapply(df, is.numeric))))
  cat(sprintf("Character/factor variables: %d\n", sum(sapply(df, function(x) is.character(x) | is.factor(x)))))
  
  # Summary stats
  cat("\n--- Summary Statistics ---\n")
  stats <- get_summary_stats(df)
  print(stats, n = 50)
  
  # Missing data
  print_missing_summary(df)
  
  # Outliers
  cat("\n--- Outlier Detection (IQR method) ---\n")
  outliers <- detect_outliers_iqr(df)
  outliers_present <- outliers %>% filter(n_outliers > 0)
  if (nrow(outliers_present) > 0) {
    print(outliers_present, n = 30)
  } else {
    cat("No outliers detected.\n")
  }
  
  # Save outputs if directory provided
  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Save stats CSV
    write.csv(stats, file.path(output_dir, paste0(name, "_summary_stats.csv")), row.names = FALSE)
    
    # Save distribution plots
    tryCatch({
      plot_distributions(df, save_path = file.path(output_dir, paste0(name, "_distributions.pdf")))
    }, error = function(e) message("Could not save distribution plot: ", e$message))
    
    # Save correlation plot
    tryCatch({
      plot_correlations(df, save_path = file.path(output_dir, paste0(name, "_correlations.pdf")))
    }, error = function(e) message("Could not save correlation plot: ", e$message))
    
    message("\nOutputs saved to: ", output_dir)
  }
  
  invisible(list(
    summary = stats,
    outliers = outliers
  ))
}
