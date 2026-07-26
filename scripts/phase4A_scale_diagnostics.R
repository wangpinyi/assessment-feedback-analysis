# ==============================================================================
# PHASE 4A: POLYCHORIC CORRELATIONS AND ORDINAL RELIABILITY
# Learner Assessment and Feedback Survey
# ==============================================================================
# Purpose
#   1. Read and validate the three processed files.
#   2. Treat the four feedback items as ordered categorical variables.
#   3. Estimate a polychoric correlation matrix.
#   4. estimate ordinal alpha and one-factor omega.
#   5. evaluate dimensionality and item diagnostics.
#   6. create the four-item feedback-experience composite.
#   7. document the ceiling effect and a three-item sensitivity composite.
#
# IMPORTANT
#   - Run this script from the assessment-feedback-analysis RStudio project.
#   - Edit only the three path definitions in Section 1 if your files are stored
#     somewhere else.
#   - The four feedback items contain no missing values in the uploaded data.
#   - Duration variables are deliberately excluded from this phase.
# ==============================================================================

# -----------------------------------------------------------------------------
# 0. PACKAGES
# -----------------------------------------------------------------------------
# Run this installation command once if needed:
# install.packages(c("tidyverse", "here", "psych", "lavaan", "semTools"))

library(tidyverse)
library(here)
library(psych)
library(lavaan)
library(semTools)

# The psych documentation recommends one core when sparse ordinal tables or
# resampling can create unstable/non-positive-definite matrices.
options(mc.cores = 1)

# -----------------------------------------------------------------------------
# 1. FILE PATHS
# -----------------------------------------------------------------------------

library(here)

phase1_path <- here(
  "data_processed",
  "learner_analysis_phase1.csv"
)

phase2_path <- here(
  "data_processed",
  "learner_analysis_phase2_recoded.csv"
)

dictionary_path <- here(
  "data_processed",
  "learner_question_dictionary.csv"
)

# Verify that all required files exist
required_files <- c(
  phase1_path,
  phase2_path,
  dictionary_path
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "The following required files were not found:\n\n",
      paste(missing_files, collapse = "\n")
    )
  )
}

message("All required data files were located successfully.")

cat(
  "\nPhase 1 file:\n", phase1_path,
  "\n\nPhase 2 file:\n", phase2_path,
  "\n\nQuestion dictionary:\n", dictionary_path,
  "\n"
)

# -----------------------------------------------------------------------------
# 2. IMPORT THE THREE PROCESSED FILES
# -----------------------------------------------------------------------------
library(tidyverse)

phase1 <- read_csv(
  phase1_path,
  show_col_types = FALSE
)

phase2 <- read_csv(
  phase2_path,
  show_col_types = FALSE
)

question_dictionary <- read_csv(
  dictionary_path,
  show_col_types = FALSE
)

cat("Phase 1 dimensions:", nrow(phase1), "rows x", ncol(phase1), "columns\n")
cat("Phase 2 dimensions:", nrow(phase2), "rows x", ncol(phase2), "columns\n")
cat("Dictionary dimensions:", nrow(question_dictionary), "rows x",
    ncol(question_dictionary), "columns\n")

# Phase 2 should contain all Phase 1 variables plus duration_minutes and
# duration_flag. The common columns should otherwise be identical.
phase2_common <- phase2 |> select(all_of(names(phase1)))
comparison <- all.equal(phase1, phase2_common, check.attributes = FALSE)

if (!isTRUE(comparison)) {
  stop(
    "Phase 1 and Phase 2 do not match on their common columns.\n",
    paste(comparison, collapse = "\n")
  )
} else {
  message("Data-integrity check passed: Phase 1 and Phase 2 common columns match.")
}

# Use Phase 2 as the canonical (正经的；教规的) analysis file because it is the latest processed
# version. Duration variables will not be selected or analyzed in Phase 4A.
learner <- phase2

# -----------------------------------------------------------------------------
# 3. DEFINE THE FOUR FEEDBACK ITEMS AND CONNECT THEM TO THE DICTIONARY
# -----------------------------------------------------------------------------
feedback_items <- c(
  "rubric_clear_aligned",
  "feedback_specific_relevant",
  "feedback_helped_improve",
  "feedback_timely"
)

required_columns <- c("case_id", feedback_items)
missing_columns <- setdiff(required_columns, names(learner))

if (length(missing_columns) > 0) {
  stop("Required columns are missing: ", paste(missing_columns, collapse = ", "))
}

# The question dictionary retains the cleaned Qualtrics names q14, q8, q9,
# and q10. This crosswalk connects those raw survey items to the final analysis
# variables used in the processed learner file.
feedback_crosswalk <- tibble(
  raw_variable = c("q14", "q8", "q9", "q10"),
  analysis_variable = feedback_items
) |>
  left_join(
    question_dictionary |> select(variable, question_text),
    by = c("raw_variable" = "variable")
  )

if (anyNA(feedback_crosswalk$question_text)) {
  stop("At least one feedback item could not be matched to the question dictionary.")
}

print(feedback_crosswalk, n = Inf)
write_csv(
  feedback_crosswalk,
  file.path(output_dir, "phase4A_feedback_item_crosswalk.csv")
)

# -----------------------------------------------------------------------------
# 4. VALIDATE AND RECODE THE LIKERT RESPONSES
# -----------------------------------------------------------------------------
likert_levels <- c(
  "Strongly disagree",
  "Somewhat disagree",
  "Neither agree nor disagree",
  "Somewhat agree",
  "Strongly agree"
)

observed_labels <- learner |>
  select(all_of(feedback_items)) |>
  unlist(use.names = FALSE) |>
  unique() |>
  na.omit() |>
  as.character()

unexpected_labels <- setdiff(observed_labels, likert_levels)
if (length(unexpected_labels) > 0) {
  stop(
    "Unexpected Likert labels detected: ",
    paste(unexpected_labels, collapse = ", ")
  )
}

# Ordered-factor version: appropriate for lavaan's categorical CFA.
feedback_ordered <- learner |>
  select(all_of(feedback_items)) |>
  mutate(
    across(
      everything(),
      ~ factor(.x, levels = likert_levels, ordered = TRUE)
    )
  )

# Numeric 1-5 version: convenient for psych::polychoric() and for computing the
# observed composite. Because the factor levels were explicitly defined above,
# as.integer() maps Strongly disagree = 1 through Strongly agree = 5.
feedback_numeric <- feedback_ordered |>
  mutate(across(everything(), as.integer))

# Missing-data and category checks.
item_quality_checks <- tibble(
  item = feedback_items,
  valid_n = map_int(feedback_ordered, ~ sum(!is.na(.x))),
  missing_n = map_int(feedback_ordered, ~ sum(is.na(.x))),
  observed_categories = map_int(feedback_ordered, ~ n_distinct(.x, na.rm = TRUE)),
  minimum_score = map_int(feedback_numeric, ~ min(.x, na.rm = TRUE)),
  maximum_score = map_int(feedback_numeric, ~ max(.x, na.rm = TRUE))
)

print(item_quality_checks)

if (any(item_quality_checks$observed_categories < 2)) {
  stop("At least one item has fewer than two observed categories.")
}

# Full item response distributions, including zero-count categories.
item_distributions <- learner |>
  select(case_id, all_of(feedback_items)) |>
  pivot_longer(
    cols = all_of(feedback_items),
    names_to = "item",
    values_to = "response"
  ) |>
  mutate(response = factor(response, levels = likert_levels, ordered = TRUE)) |>
  count(item, response, .drop = FALSE, name = "n") |>
  group_by(item) |>
  mutate(percent = n / sum(n)) |>
  ungroup()

print(item_distributions, n = Inf)
write_csv(
  item_distributions,
  file.path(output_dir, "phase4A_item_distributions.csv")
)

### Long-format table
item_distributions <- feedback_ordered |>
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "response"
  ) |>
  count(
    item,
    response,
    .drop = FALSE
  ) |>
  group_by(item) |>
  mutate(
    proportion = n / sum(n),
    percent = 100 * proportion
  ) |>
  ungroup()

print(
  item_distributions,
  n = Inf,
  width = Inf
)

# -----------------------------------------------------------------------------
# 5. ESTIMATE THE POLYCHORIC CORRELATION MATRIX
# -----------------------------------------------------------------------------
# Why these options?
#   global = FALSE : estimates thresholds locally for each item pair. This is
#                    appropriate when items have different observed numbers of
#                    response categories, as they do here.
#   correct = 0     : does not add an arbitrary continuity correction to empty
#                    contingency-table cells.
#   smooth = FALSE  : preserves the originally estimated matrix so that any
#                    non-positive-definite result is detected rather than hidden.
#   ML = FALSE      : uses psych's faster two-step estimator.

poly_result <- psych::polychoric(
  x = as.data.frame(feedback_numeric),
  global = FALSE,
  correct = 0,
  smooth = FALSE,
  ML = FALSE,
  progress = FALSE,
  na.rm = TRUE
)

rho_raw <- poly_result$rho
dimnames(rho_raw) <- list(feedback_items, feedback_items)

raw_eigenvalues <- eigen(rho_raw, symmetric = TRUE)$values
cat("Raw polychoric eigenvalues:\n")
print(raw_eigenvalues)

# A valid correlation matrix should be positive definite. Smooth only if needed,
# and retain both matrices so the adjustment is transparent.
if (min(raw_eigenvalues) <= 1e-8) {
  warning(
    "The raw polychoric matrix is not positive definite. ",
    "psych::cor.smooth() will be used for reliability calculations."
  )
  rho <- psych::cor.smooth(rho_raw)
} else {
  rho <- rho_raw
}

rho_export <- as.data.frame(rho) |>
  rownames_to_column("item")

write_csv(
  rho_export,
  file.path(output_dir, "phase4A_polychoric_correlation_matrix.csv")
)

cat("Polychoric correlation matrix used for diagnostics:\n")
print(round(rho, 3))

# Pearson matrix is retained only as a comparison. It treats 1-5 as if the
# category intervals were equal, which is not the primary assumption here.
pearson_r <- cor(feedback_numeric, use = "pairwise.complete.obs")


### Cleaner table
polychoric_pairs <- as.data.frame(
  as.table(rho)
) |>
  as_tibble() |>
  rename(
    item_1 = Var1,
    item_2 = Var2,
    correlation = Freq
  ) |>
  filter(
    as.integer(item_1) <
      as.integer(item_2)
  )

print(
  polychoric_pairs,
  n = Inf
)

### The four feedback items exhibit a coherent correlation structure consistent with a dominant common dimension. 
### Polychoric correlations range from .524 to .901, with an average correlation of approximately .701. 
### All eigenvalues are positive, so the matrix is positive definite and requires no smoothing. 
### The first eigenvalue is 3.118 and accounts for approximately 78.0% of the total standardized variance, substantially exceeding the remaining eigenvalues.
### These results support proceeding with ordinal reliability and one-factor analyses.


### Continuity-correction sensitivity check
poly_corrected <- psych::polychoric(
  x = as.data.frame(feedback_numeric),
  global = FALSE,
  correct = 0.5,
  smooth = FALSE,
  ML = FALSE,
  progress = FALSE,
  na.rm = TRUE
)

rho_corrected <- poly_corrected$rho

print(
  round(
    rho_corrected,
    3
  )
)

rho_difference <-
  rho_corrected -
  rho_raw

print(
  round(
    rho_difference,
    3
  )
)

max_absolute_difference <- max(
  abs(
    rho_difference[
      upper.tri(rho_difference)
    ]
  )
)

max_absolute_difference

### Continuity-correction sensitivity check: passed, with a modest sparse-cell effect
# ------------------------------------------------------------

### Pairwise sensitivity table
correlation_pairs <- combn(
  colnames(rho_raw),
  2,
  simplify = FALSE
)

polychoric_sensitivity_table <- purrr::map_dfr(
  correlation_pairs,
  function(pair) {
    
    item_1 <- pair[1]
    item_2 <- pair[2]
    
    tibble(
      item_1 = item_1,
      item_2 = item_2,
      correlation_no_correction =
        rho_raw[item_1, item_2],
      correlation_correction_0_5 =
        rho_corrected[item_1, item_2],
      difference =
        rho_corrected[item_1, item_2] -
        rho_raw[item_1, item_2],
      absolute_difference =
        abs(difference)
    )
  }
) |>
  arrange(
    desc(absolute_difference)
  )

print(
  polychoric_sensitivity_table,
  n = Inf,
  width = Inf
)

polychoric_sensitivity_table <- polychoric_sensitivity_table |>
  mutate(
    sensitivity_level = case_when(
      absolute_difference < 0.02 ~ "Minimal",
      absolute_difference < 0.05 ~ "Modest",
      absolute_difference < 0.10 ~ "Noticeable",
      TRUE ~ "Substantial"
    )
  )

print(
  polychoric_sensitivity_table,
  n = Inf,
  width = Inf
)

write_csv(
  polychoric_sensitivity_table,
  file.path(
    output_dir,
    "phase4A_polychoric_sensitivity_comparison.csv"
  )
)

file.exists(
  file.path(
    output_dir,
    "phase4A_polychoric_sensitivity_comparison.csv"
  )
)
### Modest numerical sensitivity but strong substantive robustness.


# -----------------------------------------------------------------------------
# 6. ORDINAL ALPHA AND ITEM DIAGNOSTICS
# -----------------------------------------------------------------------------
# For a k-item standardized correlation matrix:
# alpha = k * average_r / [1 + (k - 1) * average_r]
alpha_from_correlation <- function(R) {
  k <- ncol(R)
  if (k < 2) return(NA_real_)
  average_r <- mean(R[upper.tri(R)])
  k * average_r / (1 + (k - 1) * average_r)
}

# Correlation between one item and the unit-weighted sum of the remaining items,
# calculated directly from the polychoric correlation matrix.
item_rest_from_correlation <- function(R, item_index) {
  other_indices <- setdiff(seq_len(ncol(R)), item_index)
  covariance_item_with_rest <- sum(R[item_index, other_indices])
  variance_rest_sum <- sum(R[other_indices, other_indices])

  covariance_item_with_rest /
    sqrt(R[item_index, item_index] * variance_rest_sum)
}

ordinal_alpha <- alpha_from_correlation(rho)
pearson_alpha_comparison <- alpha_from_correlation(pearson_r)
average_polychoric_r <- mean(rho[upper.tri(rho)])

ordinal_item_rest <- map_dbl(
  seq_along(feedback_items),
  ~ item_rest_from_correlation(rho, .x)
)

ordinal_alpha_if_deleted <- map_dbl(
  seq_along(feedback_items),
  ~ alpha_from_correlation(rho[-.x, -.x, drop = FALSE])
)

# -----------------------------------------------------------------------------
# 7. ONE-FACTOR ORDINAL CFA AND OMEGA
# -----------------------------------------------------------------------------
# Omega is model based. Unlike alpha, it does not require all four items to have
# equal factor loadings. The ordered= argument tells lavaan that these are
# ordinal outcomes; lavaan then uses its WLSMV categorical-data machinery.

cfa_model <- paste(
  "feedback_experience =~",
  paste(feedback_items, collapse = " + ")
)

cfa_fit <- lavaan::cfa(
  model = cfa_model,
  data = feedback_ordered,
  ordered = feedback_items,
  estimator = "WLSMV",
  std.lv = TRUE
)

if (!lavaan::lavInspect(cfa_fit, "converged")) {
  stop("The one-factor ordinal CFA did not converge.")
}

cat("\nOne-factor ordinal CFA summary:\n")
print(summary(cfa_fit, standardized = TRUE, fit.measures = TRUE))

standardized_loadings <- lavaan::standardizedSolution(cfa_fit) |>
  as_tibble() |>
  filter(op == "=~") |>
  transmute(
    item = rhs,
    standardized_loading = est.std,
    standard_error = se,
    z_value = z,
    p_value = pvalue
  )

### Although the one-factor ordinal CFA converged and produced near-perfect
### global fit indices, the solution was inadmissible because the standardized
### loading for feedback specificity exceeded 1.00 and its residual variance was
### negative. CFA-based fit and reliability estimates were therefore not interpreted.


# -----------------------------------------------------------------------------
# 7A. CHECK CFA ADMISSIBILITY
# -----------------------------------------------------------------------------

cfa_parameter_table <- lavaan::standardizedSolution(cfa_fit) |>
  as_tibble()

cfa_loadings <- cfa_parameter_table |>
  filter(op == "=~") |>
  transmute(
    item = rhs,
    standardized_loading = est.std,
    loading_above_one =
      standardized_loading > 1
  )

cfa_residual_variances <- cfa_parameter_table |>
  filter(
    op == "~~",
    lhs == rhs,
    lhs %in% feedback_items
  ) |>
  transmute(
    item = lhs,
    standardized_residual_variance = est.std,
    negative_residual_variance =
      standardized_residual_variance < 0
  )

cfa_admissibility_table <- cfa_loadings |>
  left_join(
    cfa_residual_variances,
    by = "item"
  )

print(
  cfa_admissibility_table,
  n = Inf,
  width = Inf
)


### Formal flag
four_item_cfa_admissible <-
  all(
    cfa_admissibility_table$standardized_loading <= 1
  ) &&
  all(
    cfa_admissibility_table$
      standardized_residual_variance >= 0
  )

four_item_cfa_admissible


### Export the evidence
write_csv(
  cfa_admissibility_table,
  file.path(
    output_dir,
    "phase4A_four_item_cfa_admissibility.csv"
  )
)


### Conditional block
if (!four_item_cfa_admissible) {
  
  warning(
    paste0(
      "Four-item CFA is inadmissible because at least one ",
      "standardized loading exceeds 1 or at least one ",
      "residual variance is negative. Omega will not be ",
      "calculated from this model."
    )
  )
  
  omega_latent_response <- NA_real_
  omega_observed_ordinal <- NA_real_
  
} else {
  
  omega_latent_response <-
    semTools::compRelSEM(
      cfa_fit,
      ord.scale = FALSE,
      simplify = TRUE
    )
  
  omega_observed_ordinal <-
    semTools::compRelSEM(
      cfa_fit,
      ord.scale = TRUE,
      simplify = TRUE
    )
}

### The item creating the Heywood case is
### feedback_specific_relevant,
### not feedback_timely.

# -----------------------------------------------------------------------------
# 7B. THREE-ITEM DIAGNOSTIC MODEL EXCLUDING SPECIFICITY
# -----------------------------------------------------------------------------

feedback_items_3item <- c(
  "rubric_clear_aligned",
  "feedback_helped_improve",
  "feedback_timely"
)

cfa_model_3item <- paste(
  "feedback_experience_3item =~",
  paste(
    feedback_items_3item,
    collapse = " + "
  )
)

cfa_fit_3item <- lavaan::cfa(
  model = cfa_model_3item,
  data = feedback_ordered,
  ordered = feedback_items_3item,
  estimator = "WLSMV",
  std.lv = TRUE
)

cat(
  "\nThree-item CFA excluding specificity:\n"
)

print(
  summary(
    cfa_fit_3item,
    standardized = TRUE,
    fit.measures = FALSE
  )
)

# Keep whichever fit statistics are available in the installed lavaan version.
all_fit_measures <- lavaan::fitMeasures(cfa_fit)
requested_fit_measures <- c(
  "chisq.scaled", "df.scaled", "pvalue.scaled",
  "cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr"
)
available_fit_measures <- intersect(requested_fit_measures, names(all_fit_measures))

cfa_fit_table <- tibble(
  measure = available_fit_measures,
  estimate = unname(all_fit_measures[available_fit_measures])
)

### Three-item diagonostics
three_item_parameters <-
  lavaan::standardizedSolution(
    cfa_fit_3item
  ) |>
  as_tibble()

three_item_loadings <-
  three_item_parameters |>
  filter(op == "=~") |>
  transmute(
    item = rhs,
    standardized_loading = est.std
  )

three_item_residuals <-
  three_item_parameters |>
  filter(
    op == "~~",
    lhs == rhs,
    lhs %in% feedback_items_3item
  ) |>
  transmute(
    item = lhs,
    standardized_residual_variance =
      est.std
  )

three_item_admissibility <-
  three_item_loadings |>
  left_join(
    three_item_residuals,
    by = "item"
  ) |>
  mutate(
    loading_above_one =
      standardized_loading > 1,
    
    negative_residual =
      standardized_residual_variance < 0
  )

print(
  three_item_admissibility,
  n = Inf,
  width = Inf
)

### Three-item Omega
three_item_cfa_admissible <-
  all(
    three_item_admissibility$
      standardized_loading <= 1
  ) &&
  all(
    three_item_admissibility$
      standardized_residual_variance >= 0
  )

three_item_cfa_admissible

### it is TRUE, then it is admissable
if (three_item_cfa_admissible) {
  
  omega_3item_latent_response <-
    semTools::compRelSEM(
      cfa_fit_3item,
      ord.scale = FALSE,
      simplify = TRUE
    )
  
  omega_3item_observed_ordinal <-
    semTools::compRelSEM(
      cfa_fit_3item,
      ord.scale = TRUE,
      simplify = TRUE
    )
  
  print(
    omega_3item_latent_response
  )
  
  print(
    omega_3item_observed_ordinal
  )
}

### Print the ordinal alpha results
reliability_summary <- tibble(
  statistic = c(
    "Average polychoric correlation",
    "Ordinal alpha",
    "Pearson alpha for comparison"
  ),
  estimate = c(
    average_polychoric_r,
    ordinal_alpha,
    pearson_alpha_comparison
  )
)

print(
  reliability_summary,
  n = Inf,
  width = Inf
)


### Create the item table
ordinal_item_diagnostics <- tibble(
  item = feedback_items,
  ordinal_item_rest_correlation =
    ordinal_item_rest,
  ordinal_alpha_if_deleted =
    ordinal_alpha_if_deleted
)

print(
  ordinal_item_diagnostics,
  n = Inf,
  width = Inf
)


# -----------------------------------------------------------------------------
# 8. OBSERVED-SCORE ALPHA
# -----------------------------------------------------------------------------

four_item_alpha_object <- psych::alpha(
  as.data.frame(feedback_numeric),
  check.keys = FALSE,
  warnings = FALSE
)

three_item_numeric <- feedback_numeric |>
  select(all_of(feedback_items_3item))

three_item_alpha_object <- psych::alpha(
  as.data.frame(three_item_numeric),
  check.keys = FALSE,
  warnings = FALSE
)

observed_alpha_summary <- tibble(
  composite = c(
    "Four-item feedback-experience composite",
    "Three-item composite excluding specificity"
  ),
  
  number_of_items = c(4, 3),
  
  raw_alpha = c(
    four_item_alpha_object$total$raw_alpha,
    three_item_alpha_object$total$raw_alpha
  ),
  
  standardized_alpha = c(
    four_item_alpha_object$total$std.alpha,
    three_item_alpha_object$total$std.alpha
  ),
  
  average_pearson_correlation = c(
    four_item_alpha_object$total$average_r,
    three_item_alpha_object$total$average_r
  )
)

print(
  observed_alpha_summary,
  n = Inf,
  width = Inf
)

# -----------------------------------------------------------------------------
# 9. CREATE COMPOSITE VARIABLES
# -----------------------------------------------------------------------------

numeric_item_names <- paste0(
  feedback_items,
  "_num"
)

three_item_numeric_names <- paste0(
  feedback_items_3item,
  "_num"
)

phase4A_data <- bind_cols(
  learner,
  feedback_numeric |>
    set_names(numeric_item_names)
)

phase4A_data <- phase4A_data |>
  mutate(
    # Primary four-item feedback-experience index
    feedback_experience = rowMeans(
      pick(all_of(numeric_item_names)),
      na.rm = FALSE
    ),
    
    # Sensitivity index excluding the specificity item
    feedback_experience_3item = rowMeans(
      pick(all_of(three_item_numeric_names)),
      na.rm = FALSE
    ),
    
    # Ceiling indicators
    feedback_experience_maximum =
      feedback_experience == 5,
    
    feedback_experience_3item_maximum =
      feedback_experience_3item == 5
  )


### Summarize the composite distributions
composite_summary <- bind_rows(
  
  phase4A_data |>
    summarise(
      composite =
        "Four-item feedback-experience composite",
      
      valid_n =
        sum(!is.na(feedback_experience)),
      
      mean =
        mean(feedback_experience, na.rm = TRUE),
      
      standard_deviation =
        sd(feedback_experience, na.rm = TRUE),
      
      minimum =
        min(feedback_experience, na.rm = TRUE),
      
      maximum =
        max(feedback_experience, na.rm = TRUE),
      
      maximum_score_n =
        sum(feedback_experience == 5, na.rm = TRUE),
      
      maximum_score_percent =
        100 * mean(
          feedback_experience == 5,
          na.rm = TRUE
        )
    ),
  
  phase4A_data |>
    summarise(
      composite =
        "Three-item composite excluding specificity",
      
      valid_n =
        sum(!is.na(feedback_experience_3item)),
      
      mean =
        mean(
          feedback_experience_3item,
          na.rm = TRUE
        ),
      
      standard_deviation =
        sd(
          feedback_experience_3item,
          na.rm = TRUE
        ),
      
      minimum =
        min(
          feedback_experience_3item,
          na.rm = TRUE
        ),
      
      maximum =
        max(
          feedback_experience_3item,
          na.rm = TRUE
        ),
      
      maximum_score_n =
        sum(
          feedback_experience_3item == 5,
          na.rm = TRUE
        ),
      
      maximum_score_percent =
        100 * mean(
          feedback_experience_3item == 5,
          na.rm = TRUE
        )
    )
)

print(
  composite_summary,
  n = Inf,
  width = Inf
)

### Compare the two composites
composite_correlation <- cor(
  phase4A_data$feedback_experience,
  phase4A_data$feedback_experience_3item,
  use = "complete.obs"
)

composite_correlation


# -----------------------------------------------------------------------------
# 10. FINAL PHASE 4A VALIDATION AND EXPORT
# -----------------------------------------------------------------------------

# Confirm the composite calculations
stopifnot(
  nrow(phase4A_data) == 170,
  sum(is.na(phase4A_data$feedback_experience)) == 0,
  sum(is.na(phase4A_data$feedback_experience_3item)) == 0,
  all(
    phase4A_data$feedback_experience >= 1 &
      phase4A_data$feedback_experience <= 5
  ),
  all(
    phase4A_data$feedback_experience_3item >= 1 &
      phase4A_data$feedback_experience_3item <= 5
  )
)

message("Phase 4A composite validation passed.")


# Final scale-decision summary
phase4A_measurement_decision <- tibble(
  feature = c(
    "Primary variable",
    "Primary variable type",
    "Primary number of items",
    "Primary raw alpha",
    "Primary standardized alpha",
    "Primary ordinal alpha",
    "Four-item CFA admissible",
    "Primary composite mean",
    "Primary composite SD",
    "Primary maximum-score percentage",
    "Sensitivity variable",
    "Sensitivity number of items",
    "Sensitivity raw alpha",
    "Sensitivity observed omega",
    "Correlation between primary and sensitivity scores"
  ),
  
  result = c(
    "feedback_experience",
    "Unit-weighted four-item mean/index",
    "4",
    sprintf("%.3f", four_item_alpha_object$total$raw_alpha),
    sprintf("%.3f", four_item_alpha_object$total$std.alpha),
    sprintf("%.3f", ordinal_alpha),
    "No",
    sprintf(
      "%.3f",
      mean(
        phase4A_data$feedback_experience
      )
    ),
    sprintf(
      "%.3f",
      sd(
        phase4A_data$feedback_experience
      )
    ),
    sprintf(
      "%.1f%%",
      100 * mean(
        phase4A_data$feedback_experience == 5
      )
    ),
    "feedback_experience_3item",
    "3",
    sprintf("%.3f", three_item_alpha_object$total$raw_alpha),
    sprintf("%.3f", omega_3item_observed_ordinal),
    sprintf("%.3f", composite_correlation)
  )
)

print(
  phase4A_measurement_decision,
  n = Inf,
  width = Inf
)


# Export the analysis-ready dataset
write_csv(
  phase4A_data,
  file.path(
    output_dir,
    "learner_analysis_phase4A_with_composites.csv"
  )
)

saveRDS(
  phase4A_data,
  file.path(
    output_dir,
    "learner_analysis_phase4A_with_composites.rds"
  )
)


# Export diagnostic summaries
write_csv(
  observed_alpha_summary,
  file.path(
    output_dir,
    "phase4A_observed_alpha_summary.csv"
  )
)

write_csv(
  composite_summary,
  file.path(
    output_dir,
    "phase4A_composite_summary.csv"
  )
)

write_csv(
  ordinal_item_diagnostics,
  file.path(
    output_dir,
    "phase4A_ordinal_item_diagnostics.csv"
  )
)

write_csv(
  three_item_admissibility,
  file.path(
    output_dir,
    "phase4A_three_item_cfa_admissibility.csv"
  )
)

write_csv(
  phase4A_measurement_decision,
  file.path(
    output_dir,
    "phase4A_measurement_decision.csv"
  )
)


# Save core R objects for reproducibility
saveRDS(
  list(
    polychoric_matrix = rho,
    polychoric_matrix_corrected = rho_corrected,
    raw_eigenvalues = raw_eigenvalues,
    ordinal_alpha = ordinal_alpha,
    ordinal_item_diagnostics =
      ordinal_item_diagnostics,
    four_item_cfa = cfa_fit,
    three_item_cfa = cfa_fit_3item,
    observed_alpha_summary =
      observed_alpha_summary,
    composite_summary =
      composite_summary,
    measurement_decision =
      phase4A_measurement_decision
  ),
  file.path(
    output_dir,
    "phase4A_analysis_objects.rds"
  )
)


# Save the computing environment
writeLines(
  capture.output(sessionInfo()),
  file.path(
    output_dir,
    "phase4A_session_info.txt"
  )
)


# Confirm exported files
phase4A_export_check <- tibble(
  file = c(
    "Analysis-ready CSV",
    "Analysis-ready RDS",
    "Observed alpha summary",
    "Composite summary",
    "Ordinal item diagnostics",
    "Three-item CFA diagnostics",
    "Measurement decision",
    "Analysis objects",
    "Session information"
  ),
  
  exists = file.exists(
    c(
      file.path(
        output_dir,
        "learner_analysis_phase4A_with_composites.csv"
      ),
      file.path(
        output_dir,
        "learner_analysis_phase4A_with_composites.rds"
      ),
      file.path(
        output_dir,
        "phase4A_observed_alpha_summary.csv"
      ),
      file.path(
        output_dir,
        "phase4A_composite_summary.csv"
      ),
      file.path(
        output_dir,
        "phase4A_ordinal_item_diagnostics.csv"
      ),
      file.path(
        output_dir,
        "phase4A_three_item_cfa_admissibility.csv"
      ),
      file.path(
        output_dir,
        "phase4A_measurement_decision.csv"
      ),
      file.path(
        output_dir,
        "phase4A_analysis_objects.rds"
      ),
      file.path(
        output_dir,
        "phase4A_session_info.txt"
      )
    )
  )
)

print(
  phase4A_export_check,
  n = Inf,
  width = Inf
)

stopifnot(
  all(phase4A_export_check$exists)
)

message("Phase 4A files exported successfully.")














 