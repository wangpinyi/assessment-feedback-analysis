# ==============================================================================
# PHASE 4C, STEP 1: ORDINAL REGRESSION FOR PERCEIVED AI USEFULNESS
# Learner Assessment and Feedback Survey
# ==============================================================================
# Outcome
#   ai_as_useful_as_human:
#   "I believe AI-generated feedback can be as useful as feedback from a human
#   instructor for identifying areas of improvement."
#
# Purpose of Step 1
#   1. Prepare and validate the Phase 4C analysis dataset.
#   2. Describe the ordered outcome and inspect sparse predictor-by-outcome cells.
#   3. Fit a theoretically ordered sequence of cumulative-link models.
#   4. Compare the contribution of AI comfort and feedback experience.
#   5. evaluate convergence, proportional odds, latent-scale effects, coding
#      assumptions, teaching-experience adjustment, and link-function sensitivity.
#   6. save a checkpoint for final-model selection in Phase 4C, Step 2.
#
# MODEL SEQUENCE
#   Background model:
#     usefulness ~ prior AI experience + AI awareness
#
#   Comfort-added model:
#     usefulness ~ prior AI experience + AI awareness + AI comfort
#
#   Full candidate model:
#     usefulness ~ prior AI experience + AI awareness + AI comfort
#                  + feedback experience
#
# RATIONALE
#   - The usefulness item has five ordered response categories; a cumulative-link
#     model is preferable to ordinary least squares.
#   - AI comfort is included as a central Phase 4C predictor because comfort with
#     AI-supported feedback and perceived usefulness are conceptually related but
#     not identical constructs.
#   - Prior AI experience remains categorical because its association need not be
#     linear across the four experience categories.
#   - AI awareness is represented by a 0-1-2 score in the main models; a factor-
#     coded sensitivity model checks the equal-step assumption.
#   - AI comfort is represented by a 0-1-2-3-4 score in the main models; a factor-
#     coded sensitivity model checks whether a one-category linear association is
#     adequate.
#   - The feedback composite is centered and divided by 0.5, so its coefficient
#     is interpreted for a 0.5-point increase on the original 1-to-5 scale.
#   - Teaching experience is retained as a sensitivity adjustment to preserve
#     parsimony with n = 170.
#
# IMPORTANT
#   - Save this file as scripts/04C_ordinal_regression_ai_usefulness.R before use.
#   - Run it from the assessment-feedback-analysis RStudio project.
#   - Step 1 deliberately does not freeze a final model. Review the printed model
#     comparisons and diagnostics before proceeding to predicted probabilities,
#     interpretation, figures, and final exports in Step 2.
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. START CLEAN AND LOAD PACKAGES
# ------------------------------------------------------------------------------

rm(list = ls())
graphics.off()
options(scipen = 999)

# Run once if needed:
# install.packages(c("tidyverse", "here", "ordinal"))

library(tidyverse)
library(here)
library(ordinal)

here::i_am(
  "scripts/04C_ordinal_regression_ai_usefulness.R"
)

cat("Project root:", here(), "\n")


# ------------------------------------------------------------------------------
# 1. DEFINE INPUT AND OUTPUT PATHS
# ------------------------------------------------------------------------------

phase4A_rds_path <- here(
  "output",
  "learner_scale_diagnostics",
  "learner_analysis_phase4A_with_composites.rds"
)

phase3_rds_path <- here(
  "data_processed",
  "learner_analysis_phase3_descriptive_ready.rds"
)

phase2_csv_path <- here(
  "data_processed",
  "learner_analysis_phase2_recoded.csv"
)

output_dir <- here(
  "output",
  "learner_ai_usefulness_ordinal"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!dir.exists(output_dir)) {
  stop("The Phase 4C output directory could not be created.")
}


# ------------------------------------------------------------------------------
# 2. IMPORT THE MOST ADVANCED AVAILABLE DATASET
# ------------------------------------------------------------------------------

feedback_items_4item <- c(
  "rubric_clear_aligned",
  "feedback_specific_relevant",
  "feedback_helped_improve",
  "feedback_timely"
)

# Phase 4A sensitivity composite: specificity excluded.
feedback_items_3item <- c(
  "rubric_clear_aligned",
  "feedback_helped_improve",
  "feedback_timely"
)

likert_levels <- c(
  "Strongly disagree",
  "Somewhat disagree",
  "Neither agree nor disagree",
  "Somewhat agree",
  "Strongly agree"
)

if (file.exists(phase4A_rds_path)) {
  learner <- readRDS(phase4A_rds_path)
  input_source <- "Phase 4A analysis-ready RDS"

} else if (file.exists(phase3_rds_path)) {
  learner <- readRDS(phase3_rds_path)
  input_source <- "Phase 3 RDS"

} else if (file.exists(phase2_csv_path)) {
  learner <- readr::read_csv(
    phase2_csv_path,
    show_col_types = FALSE
  )
  input_source <- "Phase 2 recoded CSV"

} else {
  stop(
    paste0(
      "No valid Phase 4C input file was found.\n\n",
      "Expected one of the following:\n",
      phase4A_rds_path, "\n",
      phase3_rds_path, "\n",
      phase2_csv_path
    )
  )
}

cat("Input source:", input_source, "\n")
cat("Rows:", nrow(learner), "\n")
cat("Columns:", ncol(learner), "\n")


# ------------------------------------------------------------------------------
# 3. ENSURE THE PHASE 4A COMPOSITES ARE AVAILABLE
# ------------------------------------------------------------------------------

missing_feedback_items <- setdiff(
  feedback_items_4item,
  names(learner)
)

if (length(missing_feedback_items) > 0) {
  stop(
    "Required feedback items are missing: ",
    paste(missing_feedback_items, collapse = ", ")
  )
}

if (
  !all(
    c(
      "feedback_experience",
      "feedback_experience_3item"
    ) %in% names(learner)
  )
) {
  feedback_numeric <- learner |>
    transmute(
      across(
        all_of(feedback_items_4item),
        ~ as.integer(
          factor(
            as.character(.x),
            levels = likert_levels,
            ordered = TRUE
          )
        )
      )
    )

  numeric_item_names <- paste0(
    feedback_items_4item,
    "_num"
  )

  three_item_numeric_names <- paste0(
    feedback_items_3item,
    "_num"
  )

  feedback_numeric <- feedback_numeric |>
    set_names(numeric_item_names)

  learner <- learner |>
    bind_cols(feedback_numeric) |>
    mutate(
      feedback_experience = rowMeans(
        pick(all_of(numeric_item_names)),
        na.rm = FALSE
      ),

      feedback_experience_3item = rowMeans(
        pick(all_of(three_item_numeric_names)),
        na.rm = FALSE
      )
    )
}


# ------------------------------------------------------------------------------
# 4. VALIDATE REQUIRED VARIABLES AND LABELS
# ------------------------------------------------------------------------------

required_variables <- c(
  "case_id",
  "ai_as_useful_as_human",
  "ai_comfort",
  "prior_ai_experience",
  "ai_awareness",
  "teaching_experience",
  "feedback_experience",
  "feedback_experience_3item"
)

missing_variables <- setdiff(
  required_variables,
  names(learner)
)

if (length(missing_variables) > 0) {
  stop(
    "Required Phase 4C variables are missing: ",
    paste(missing_variables, collapse = ", ")
  )
}

if (nrow(learner) != 170) {
  warning(
    "The dataset contains ",
    nrow(learner),
    " rows rather than the expected 170."
  )
}

if (anyDuplicated(learner$case_id) > 0) {
  stop("Duplicate case IDs were detected.")
}

prior_ai_experience_levels <- c(
  "No experience",
  "Minimal experience",
  "Some experience",
  "Extensive experience"
)

ai_awareness_levels <- c(
  "Not aware at all",
  "Somewhat aware",
  "Yes, fully aware"
)

teaching_experience_levels <- c(
  "0-3 years",
  "4-10 years",
  "11-20 years",
  "20+ years"
)

check_labels <- function(variable, expected_levels, variable_name) {
  unexpected_values <- setdiff(
    unique(
      na.omit(
        as.character(variable)
      )
    ),
    expected_levels
  )

  if (length(unexpected_values) > 0) {
    stop(
      "Unexpected labels in ",
      variable_name,
      ": ",
      paste(unexpected_values, collapse = ", ")
    )
  }
}

check_labels(
  learner$ai_as_useful_as_human,
  likert_levels,
  "ai_as_useful_as_human"
)

check_labels(
  learner$ai_comfort,
  likert_levels,
  "ai_comfort"
)

check_labels(
  learner$prior_ai_experience,
  prior_ai_experience_levels,
  "prior_ai_experience"
)

check_labels(
  learner$ai_awareness,
  ai_awareness_levels,
  "ai_awareness"
)

check_labels(
  learner$teaching_experience,
  teaching_experience_levels,
  "teaching_experience"
)


# ------------------------------------------------------------------------------
# 5. RECODE THE OUTCOME AND PREDICTORS
# ------------------------------------------------------------------------------

analysis_data <- learner |>
  mutate(
    # Ordered outcome from lower to higher perceived usefulness.
    ai_usefulness = factor(
      as.character(ai_as_useful_as_human),
      levels = likert_levels,
      ordered = TRUE
    ),

    prior_ai_experience = factor(
      as.character(prior_ai_experience),
      levels = prior_ai_experience_levels
    ),

    ai_awareness_factor = factor(
      as.character(ai_awareness),
      levels = ai_awareness_levels
    ),

    ai_awareness_score = case_when(
      as.character(ai_awareness) == "Not aware at all" ~ 0,
      as.character(ai_awareness) == "Somewhat aware" ~ 1,
      as.character(ai_awareness) == "Yes, fully aware" ~ 2,
      TRUE ~ NA_real_
    ),

    # Unordered factor for the nonlinear-coding sensitivity model.
    ai_comfort_factor = factor(
      as.character(ai_comfort),
      levels = likert_levels
    ),

    # Main coding: odds ratio per one-category increase in AI comfort.
    ai_comfort_score = case_when(
      as.character(ai_comfort) == "Strongly disagree" ~ 0,
      as.character(ai_comfort) == "Somewhat disagree" ~ 1,
      as.character(ai_comfort) == "Neither agree nor disagree" ~ 2,
      as.character(ai_comfort) == "Somewhat agree" ~ 3,
      as.character(ai_comfort) == "Strongly agree" ~ 4,
      TRUE ~ NA_real_
    ),

    teaching_experience = factor(
      as.character(teaching_experience),
      levels = teaching_experience_levels
    )
  )

feedback_experience_mean <- mean(
  analysis_data$feedback_experience,
  na.rm = TRUE
)

feedback_experience_3item_mean <- mean(
  analysis_data$feedback_experience_3item,
  na.rm = TRUE
)

analysis_data <- analysis_data |>
  mutate(
    feedback_experience_half_point =
      (feedback_experience - feedback_experience_mean) / 0.5,

    feedback_experience_3item_half_point =
      (feedback_experience_3item - feedback_experience_3item_mean) / 0.5
  )


# ------------------------------------------------------------------------------
# 6. CHECK MISSINGNESS, DISTRIBUTIONS, AND SPARSE CELLS
# ------------------------------------------------------------------------------

model_variables <- c(
  "ai_usefulness",
  "prior_ai_experience",
  "ai_awareness_score",
  "ai_comfort_score",
  "feedback_experience_half_point",
  "feedback_experience_3item_half_point",
  "teaching_experience"
)

missingness_summary <- analysis_data |>
  summarise(
    across(
      all_of(model_variables),
      ~ sum(is.na(.x))
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_n"
  ) |>
  mutate(
    total_n = nrow(analysis_data),
    missing_percent = 100 * missing_n / total_n
  )

print(
  missingness_summary,
  n = Inf
)

analysis_complete <- analysis_data |>
  drop_na(
    ai_usefulness,
    prior_ai_experience,
    ai_awareness_score,
    ai_comfort_score,
    feedback_experience_half_point,
    feedback_experience_3item_half_point,
    teaching_experience
  )

cat(
  "Complete cases available for all Phase 4C models:",
  nrow(analysis_complete),
  "\n"
)

if (nrow(analysis_complete) < 150) {
  warning(
    "Fewer than 150 complete cases remain. Review missingness before modeling."
  )
}

outcome_distribution <- analysis_complete |>
  count(
    ai_usefulness,
    .drop = FALSE,
    name = "n"
  ) |>
  mutate(
    percent = 100 * n / sum(n)
  )

print(
  outcome_distribution,
  n = Inf
)

comfort_usefulness_crosstab <- analysis_complete |>
  count(
    ai_comfort_factor,
    ai_usefulness,
    .drop = FALSE,
    name = "n"
  ) |>
  group_by(ai_comfort_factor) |>
  mutate(
    row_percent = 100 * n / sum(n)
  ) |>
  ungroup()

prior_experience_crosstab <- analysis_complete |>
  count(
    prior_ai_experience,
    ai_usefulness,
    .drop = FALSE,
    name = "n"
  ) |>
  group_by(prior_ai_experience) |>
  mutate(
    row_percent = 100 * n / sum(n)
  ) |>
  ungroup()

awareness_crosstab <- analysis_complete |>
  count(
    ai_awareness_factor,
    ai_usefulness,
    .drop = FALSE,
    name = "n"
  ) |>
  group_by(ai_awareness_factor) |>
  mutate(
    row_percent = 100 * n / sum(n)
  ) |>
  ungroup()

teaching_experience_crosstab <- analysis_complete |>
  count(
    teaching_experience,
    ai_usefulness,
    .drop = FALSE,
    name = "n"
  ) |>
  group_by(teaching_experience) |>
  mutate(
    row_percent = 100 * n / sum(n)
  ) |>
  ungroup()

sparse_cell_summary <- bind_rows(
  comfort_usefulness_crosstab |>
    transmute(
      predictor = "AI comfort",
      predictor_level = as.character(ai_comfort_factor),
      ai_usefulness = as.character(ai_usefulness),
      n
    ),

  prior_experience_crosstab |>
    transmute(
      predictor = "Prior AI experience",
      predictor_level = as.character(prior_ai_experience),
      ai_usefulness = as.character(ai_usefulness),
      n
    ),

  awareness_crosstab |>
    transmute(
      predictor = "AI awareness",
      predictor_level = as.character(ai_awareness_factor),
      ai_usefulness = as.character(ai_usefulness),
      n
    ),

  teaching_experience_crosstab |>
    transmute(
      predictor = "Teaching experience",
      predictor_level = as.character(teaching_experience),
      ai_usefulness = as.character(ai_usefulness),
      n
    )
) |>
  mutate(
    sparse_cell = n < 5
  )

cat(
  "Number of descriptive predictor-by-outcome cells with n < 5:",
  sum(sparse_cell_summary$sparse_cell),
  "\n"
)

# Descriptive association only; the ordinal models below provide the adjusted
# inferential results.
comfort_usefulness_spearman <- cor.test(
  analysis_complete$ai_comfort_score,
  as.integer(analysis_complete$ai_usefulness),
  method = "spearman",
  exact = FALSE
)

cat("\nSpearman association between AI comfort and perceived usefulness:\n")
print(comfort_usefulness_spearman)


# ------------------------------------------------------------------------------
# 7. FIT THE CUMULATIVE-LINK MODEL SEQUENCE
# ------------------------------------------------------------------------------

clm_control_settings <- ordinal::clm.control(
  maxIter = 100,
  gradTol = 0.000001
)

model_null <- ordinal::clm(
  ai_usefulness ~ 1,
  data = analysis_complete,
  link = "logit",
  threshold = "flexible",
  control = clm_control_settings
)

model_background <- ordinal::clm(
  ai_usefulness ~
    prior_ai_experience +
    ai_awareness_score,
  data = analysis_complete,
  link = "logit",
  threshold = "flexible",
  control = clm_control_settings
)

model_comfort_added <- ordinal::clm(
  ai_usefulness ~
    prior_ai_experience +
    ai_awareness_score +
    ai_comfort_score,
  data = analysis_complete,
  link = "logit",
  threshold = "flexible",
  control = clm_control_settings
)

# Full Phase 4C candidate model.
model_primary <- ordinal::clm(
  ai_usefulness ~
    prior_ai_experience +
    ai_awareness_score +
    ai_comfort_score +
    feedback_experience_half_point,
  data = analysis_complete,
  link = "logit",
  threshold = "flexible",
  control = clm_control_settings
)

# Sensitivity 1: teaching-experience adjustment.
model_teaching_adjusted <- ordinal::clm(
  ai_usefulness ~
    prior_ai_experience +
    ai_awareness_score +
    ai_comfort_score +
    feedback_experience_half_point +
    teaching_experience,
  data = analysis_complete,
  link = "logit",
  threshold = "flexible",
  control = clm_control_settings
)

# Sensitivity 2: awareness as a categorical factor.
model_awareness_factor <- ordinal::clm(
  ai_usefulness ~
    prior_ai_experience +
    ai_awareness_factor +
    ai_comfort_score +
    feedback_experience_half_point,
  data = analysis_complete,
  link = "logit",
  threshold = "flexible",
  control = clm_control_settings
)

# Sensitivity 3: AI comfort as a categorical factor rather than a linear score.
model_comfort_factor <- ordinal::clm(
  ai_usefulness ~
    prior_ai_experience +
    ai_awareness_score +
    ai_comfort_factor +
    feedback_experience_half_point,
  data = analysis_complete,
  link = "logit",
  threshold = "flexible",
  control = clm_control_settings
)

# Sensitivity 4: replace the four-item composite with the three-item composite.
model_three_item_composite <- ordinal::clm(
  ai_usefulness ~
    prior_ai_experience +
    ai_awareness_score +
    ai_comfort_score +
    feedback_experience_3item_half_point,
  data = analysis_complete,
  link = "logit",
  threshold = "flexible",
  control = clm_control_settings
)

# Sensitivity 5: probit rather than logit link.
model_probit <- ordinal::clm(
  ai_usefulness ~
    prior_ai_experience +
    ai_awareness_score +
    ai_comfort_score +
    feedback_experience_half_point,
  data = analysis_complete,
  link = "probit",
  threshold = "flexible",
  control = clm_control_settings
)


# ------------------------------------------------------------------------------
# 8. VERIFY CONVERGENCE AND NUMERICAL STABILITY
# ------------------------------------------------------------------------------

extract_clm_diagnostics <- function(model) {
  convergence_raw <- model[["convergence"]]
  convergence_flat <- unlist(
    convergence_raw,
    recursive = TRUE,
    use.names = TRUE
  )

  convergence_code <- NA_integer_

  if (!is.null(names(convergence_flat))) {
    normalized_names <- tolower(
      sub(".*\\.", "", names(convergence_flat))
    )

    code_position <- which(
      normalized_names %in% c(
        "code",
        "convergence",
        "conv"
      )
    )

    if (length(code_position) > 0) {
      convergence_code <- suppressWarnings(
        as.integer(convergence_flat[code_position[1]])
      )
    }
  }

  if (is.na(convergence_code)) {
    numeric_candidates <- suppressWarnings(
      as.numeric(convergence_flat)
    )

    numeric_candidates <- numeric_candidates[
      is.finite(numeric_candidates)
    ]

    if (length(numeric_candidates) > 0) {
      convergence_code <- as.integer(numeric_candidates[1])
    }
  }

  maximum_gradient <- suppressWarnings(
    as.numeric(model[["maxGradient"]])
  )

  maximum_gradient <- maximum_gradient[
    is.finite(maximum_gradient)
  ]

  if (length(maximum_gradient) == 0) {
    gradient_values <- suppressWarnings(
      as.numeric(model[["gradient"]])
    )

    gradient_values <- gradient_values[
      is.finite(gradient_values)
    ]

    maximum_gradient <- if (length(gradient_values) > 0) {
      max(abs(gradient_values))
    } else {
      NA_real_
    }
  } else {
    maximum_gradient <- maximum_gradient[1]
  }

  hessian_condition <- suppressWarnings(
    as.numeric(model[["cond.H"]])
  )

  hessian_condition <- hessian_condition[
    is.finite(hessian_condition)
  ]

  if (length(hessian_condition) == 0) {
    hessian_matrix <- model[["Hessian"]]

    if (
      is.matrix(hessian_matrix) &&
      all(is.finite(hessian_matrix))
    ) {
      hessian_eigenvalues <- eigen(
        hessian_matrix,
        symmetric = TRUE,
        only.values = TRUE
      )$values

      if (
        length(hessian_eigenvalues) > 0 &&
        all(is.finite(hessian_eigenvalues)) &&
        min(abs(hessian_eigenvalues)) > 0
      ) {
        hessian_condition <- abs(
          max(hessian_eigenvalues) /
            min(hessian_eigenvalues)
        )
      } else {
        hessian_condition <- NA_real_
      }
    } else {
      hessian_condition <- NA_real_
    }
  } else {
    hessian_condition <- hessian_condition[1]
  }

  convergence_message <- model[["message"]]

  if (is.null(convergence_message)) {
    convergence_message <- NA_character_
  } else {
    convergence_message <- paste(
      convergence_message,
      collapse = "; "
    )
  }

  list(
    convergence_code = convergence_code,
    convergence_message = convergence_message,
    maximum_gradient = maximum_gradient,
    hessian_condition = hessian_condition
  )
}

check_clm_model <- function(model, model_name) {
  diagnostics <- extract_clm_diagnostics(model)

  if (is.na(diagnostics$convergence_code)) {
    warning(
      model_name,
      ": the convergence code could not be extracted."
    )
  } else if (diagnostics$convergence_code == 1L) {
    warning(
      model_name,
      " reached a qualified or non-unique optimum (code 1). ",
      diagnostics$convergence_message
    )
  } else if (diagnostics$convergence_code != 0L) {
    stop(
      model_name,
      " did not converge successfully. Convergence code: ",
      diagnostics$convergence_code,
      ". ",
      diagnostics$convergence_message
    )
  }

  if (
    is.finite(diagnostics$maximum_gradient) &&
    diagnostics$maximum_gradient > 0.001
  ) {
    warning(
      model_name,
      " has a maximum absolute gradient above 0.001: ",
      signif(diagnostics$maximum_gradient, 4)
    )
  }

  if (
    is.finite(diagnostics$hessian_condition) &&
    diagnostics$hessian_condition > 1000000
  ) {
    warning(
      model_name,
      " has a large Hessian condition number: ",
      signif(diagnostics$hessian_condition, 4)
    )
  }

  invisible(diagnostics)
}

models_to_check <- list(
  "Null model" = model_null,
  "Background model" = model_background,
  "Comfort-added model" = model_comfort_added,
  "Full candidate model" = model_primary,
  "Teaching-adjusted model" = model_teaching_adjusted,
  "Awareness-factor model" = model_awareness_factor,
  "Comfort-factor model" = model_comfort_factor,
  "Three-item-composite model" = model_three_item_composite,
  "Probit model" = model_probit
)

purrr::iwalk(
  models_to_check,
  ~ check_clm_model(.x, .y)
)

cat("\nFull candidate model summary:\n")
print(summary(model_primary))


# ------------------------------------------------------------------------------
# 9. SUMMARIZE MODEL FIT AND COMPARE NESTED MODELS
# ------------------------------------------------------------------------------

extract_model_fit <- function(fitted_model, model_name) {
  model_log_likelihood <- logLik(fitted_model)
  diagnostics <- extract_clm_diagnostics(fitted_model)

  tibble(
    model = model_name,
    n = as.integer(fitted_model$nobs),
    number_of_parameters = attr(model_log_likelihood, "df"),
    log_likelihood = as.numeric(model_log_likelihood),
    AIC = AIC(fitted_model),
    BIC = BIC(fitted_model),
    convergence_code = diagnostics$convergence_code,
    convergence_message = diagnostics$convergence_message,
    maximum_absolute_gradient = diagnostics$maximum_gradient,
    hessian_condition_number = diagnostics$hessian_condition,
    link = fitted_model$link
  )
}

model_fit_summary <- purrr::imap_dfr(
  models_to_check,
  ~ extract_model_fit(
    fitted_model = .x,
    model_name = .y
  )
)

print(
  model_fit_summary,
  n = Inf,
  width = Inf
)

comparison_null_background <- anova(
  model_null,
  model_background
)

comparison_background_comfort <- anova(
  model_background,
  model_comfort_added
)

comparison_comfort_feedback <- anova(
  model_comfort_added,
  model_primary
)

comparison_primary_teaching <- anova(
  model_primary,
  model_teaching_adjusted
)

comparison_awareness_coding <- anova(
  model_primary,
  model_awareness_factor
)

comparison_comfort_coding <- anova(
  model_primary,
  model_comfort_factor
)

primary_omnibus_tests <- anova(
  model_primary,
  type = "II"
)

cat("\nNull versus background model:\n")
print(comparison_null_background)

cat("\nBackground versus comfort-added model:\n")
print(comparison_background_comfort)

cat("\nComfort-added versus full candidate model:\n")
print(comparison_comfort_feedback)

cat("\nFull candidate versus teaching-adjusted model:\n")
print(comparison_primary_teaching)

cat("\nScore-coded versus factor-coded awareness:\n")
print(comparison_awareness_coding)

cat("\nScore-coded versus factor-coded AI comfort:\n")
print(comparison_comfort_coding)

cat("\nFull-candidate-model omnibus tests:\n")
print(primary_omnibus_tests)


# ------------------------------------------------------------------------------
# 10. TEST PROPORTIONAL-ODDS AND LATENT-SCALE ASSUMPTIONS
# ------------------------------------------------------------------------------

run_diagnostic_safely <- function(test_expression) {
  tryCatch(
    test_expression,
    error = function(e) {
      structure(
        list(
          message = conditionMessage(e)
        ),
        class = "phase4C_diagnostic_error"
      )
    }
  )
}

nominal_test_result <- run_diagnostic_safely(
  ordinal::nominal_test(model_primary)
)

scale_test_result <- run_diagnostic_safely(
  ordinal::scale_test(model_primary)
)

diagnostic_to_tibble <- function(result, test_name) {
  if (inherits(result, "phase4C_diagnostic_error")) {
    return(
      tibble(
        test = test_name,
        term = "TEST COULD NOT BE COMPLETED",
        note = result$message
      )
    )
  }

  as.data.frame(result) |>
    rownames_to_column(
      var = "term"
    ) |>
    as_tibble() |>
    mutate(
      test = test_name,
      .before = 1
    )
}

nominal_test_table <- diagnostic_to_tibble(
  nominal_test_result,
  "Nominal-effects test"
)

scale_test_table <- diagnostic_to_tibble(
  scale_test_result,
  "Scale-effects test"
)

cat("\nNominal-effects test of proportional odds:\n")
print(nominal_test_result)

cat("\nScale-effects diagnostic:\n")
print(scale_test_result)

# Interpretation rules for Step 2:
#   - A small nominal-test p-value suggests that a predictor's location effect
#     differs across cumulative cut points.
#   - A small scale-test p-value suggests predictor-related heterogeneity in the
#     latent response scale; it is not identical to a proportional-odds failure.
#   - Do not automatically select a more complex model from one borderline test.
#     Review sparse cells, convergence, AIC/BIC, effect stability, and substantive
#     interpretability together.


# ------------------------------------------------------------------------------
# 11. SAVE THE STEP-1 CHECKPOINT AND DIAGNOSTIC TABLES
# ------------------------------------------------------------------------------

readr::write_csv(
  missingness_summary,
  file.path(
    output_dir,
    "phase4C_step1_missingness_summary.csv"
  )
)

readr::write_csv(
  outcome_distribution,
  file.path(
    output_dir,
    "phase4C_step1_outcome_distribution.csv"
  )
)

readr::write_csv(
  sparse_cell_summary,
  file.path(
    output_dir,
    "phase4C_step1_sparse_cell_summary.csv"
  )
)

readr::write_csv(
  model_fit_summary,
  file.path(
    output_dir,
    "phase4C_step1_model_fit_summary.csv"
  )
)

readr::write_csv(
  nominal_test_table,
  file.path(
    output_dir,
    "phase4C_step1_nominal_test.csv"
  )
)

readr::write_csv(
  scale_test_table,
  file.path(
    output_dir,
    "phase4C_step1_scale_test.csv"
  )
)

saveRDS(
  analysis_complete,
  file.path(
    output_dir,
    "learner_analysis_phase4C_complete_cases.rds"
  )
)

saveRDS(
  list(
    model_null = model_null,
    model_background = model_background,
    model_comfort_added = model_comfort_added,
    model_primary = model_primary,
    model_teaching_adjusted = model_teaching_adjusted,
    model_awareness_factor = model_awareness_factor,
    model_comfort_factor = model_comfort_factor,
    model_three_item_composite = model_three_item_composite,
    model_probit = model_probit,
    model_fit_summary = model_fit_summary,
    nominal_test_result = nominal_test_result,
    scale_test_result = scale_test_result,
    feedback_experience_mean = feedback_experience_mean,
    feedback_experience_3item_mean = feedback_experience_3item_mean,
    likert_levels = likert_levels
  ),
  file.path(
    output_dir,
    "phase4C_step1_model_checkpoint.rds"
  )
)

cat(
  "\nPhase 4C Step 1 completed.\n",
  "Review these printed outputs before freezing the final model:\n",
  "  1. Full candidate model summary\n",
  "  2. Model fit summary\n",
  "  3. Background versus comfort-added comparison\n",
  "  4. Comfort-added versus full candidate comparison\n",
  "  5. Awareness- and comfort-coding comparisons\n",
  "  6. Nominal-effects and scale-effects tests\n",
  "  7. Convergence and Hessian diagnostics\n",
  sep = ""
)


# ==============================================================================
# PHASE 4C FINALIZATION
# Targeted location-scale model, adjusted probabilities, figure, and exports
# ==============================================================================


# ------------------------------------------------------------------------------
# 12. FIT AND SELECT THE FINAL AI-COMFORT LOCATION-SCALE MODEL
# ------------------------------------------------------------------------------

# Targeted model: AI comfort enters both the location and latent-scale parts.
model_comfort_scale <- ordinal::clm(
  ai_usefulness ~
    prior_ai_experience +
    ai_awareness_score +
    ai_comfort_score,

  scale = ~ ai_comfort_score,

  data = analysis_complete,
  link = "logit",
  threshold = "flexible",
  control = clm_control_settings
)

# Sensitivity model: retain feedback experience in the location component.
model_primary_comfort_scale <- ordinal::clm(
  ai_usefulness ~
    prior_ai_experience +
    ai_awareness_score +
    ai_comfort_score +
    feedback_experience_half_point,

  scale = ~ ai_comfort_score,

  data = analysis_complete,
  link = "logit",
  threshold = "flexible",
  control = clm_control_settings
)

# Compare the targeted scale model with the corresponding proportional-odds
# model. A significant test supports allowing latent dispersion to vary with
# AI comfort.
comparison_comfort_scale <- anova(
  model_comfort_added,
  model_comfort_scale
)

# Test whether the feedback-experience composite improves the location-scale
# model. The final analysis found no improvement, so the parsimonious model is
# retained.
comparison_scale_feedback <- anova(
  model_comfort_scale,
  model_primary_comfort_scale
)

check_clm_model(
  model_comfort_scale,
  "AI-comfort location-scale model"
)

check_clm_model(
  model_primary_comfort_scale,
  "Full AI-comfort location-scale model"
)

cat("\nProportional-odds versus AI-comfort location-scale model:\n")
print(comparison_comfort_scale)

cat("\nAI-comfort scale model versus feedback-added scale model:\n")
print(comparison_scale_feedback)

cat("\nSelected Phase 4C model summary:\n")
print(summary(model_comfort_scale))

# Freeze the final Phase 4C model.
final_phase4C_model <- model_comfort_scale


# ------------------------------------------------------------------------------
# 13. CREATE FINAL MODEL-FIT TABLES
# ------------------------------------------------------------------------------

phase4C_scale_fit_comparison <- bind_rows(
  extract_model_fit(
    model_comfort_added,
    "Comfort-added proportional-odds model"
  ),

  extract_model_fit(
    model_comfort_scale,
    "Comfort-added AI-comfort scale model"
  ),

  extract_model_fit(
    model_primary,
    "Full proportional-odds model"
  ),

  extract_model_fit(
    model_primary_comfort_scale,
    "Full AI-comfort scale model"
  )
)

print(
  phase4C_scale_fit_comparison,
  n = Inf,
  width = Inf
)

anova_to_tibble <- function(anova_object, comparison_name) {
  as.data.frame(anova_object) |>
    rownames_to_column(
      var = "model"
    ) |>
    as_tibble() |>
    mutate(
      comparison = comparison_name,
      .before = 1
    )
}

comparison_comfort_scale_table <- anova_to_tibble(
  comparison_comfort_scale,
  "Proportional odds versus AI-comfort location-scale"
)

comparison_scale_feedback_table <- anova_to_tibble(
  comparison_scale_feedback,
  "Final scale model versus feedback-added scale model"
)

# Export the full printed coefficient matrix. The selected model contains both
# location and log-scale coefficients, so the plain-text model summary is the
# authoritative labeled output.
final_model_coefficient_matrix <- coef(
  summary(final_phase4C_model)
) |>
  as.data.frame() |>
  rownames_to_column(
    var = "parameter"
  ) |>
  as_tibble()


# ------------------------------------------------------------------------------
# 14. ADJUSTED PREDICTED PROBABILITIES
# ------------------------------------------------------------------------------

ai_usefulness_levels <- levels(
  analysis_complete$ai_usefulness
)

stopifnot(
  length(ai_usefulness_levels) == 5,
  setequal(
    unique(analysis_complete$ai_comfort_score),
    0:4
  )
)

# Exceedance above the neutral category equals the probability of selecting
# either Somewhat agree or Strongly agree.
agreement_cut <- paste(
  ai_usefulness_levels[3],
  ai_usefulness_levels[4],
  sep = "|"
)

mean_awareness_score <- mean(
  analysis_complete$ai_awareness_score,
  na.rm = TRUE
)

usefulness_probability_by_comfort <- emmeans::emmeans(
  final_phase4C_model,
  specs = ~ ai_comfort_score,
  mode = "exc.prob",
  at = list(
    cut = agreement_cut,
    ai_comfort_score = 0:4,
    ai_awareness_score = mean_awareness_score
  ),
  weights = "proportional"
) |>
  as.data.frame() |>
  as_tibble() |>
  transmute(
    ai_comfort_score,

    ai_comfort = factor(
      ai_comfort_score,
      levels = 0:4,
      labels = likert_levels,
      ordered = TRUE
    ),

    predicted_probability = exc.prob,
    standard_error = SE,

    # Preserve the original asymptotic confidence limits.
    confidence_low_raw = asymp.LCL,
    confidence_high_raw = asymp.UCL,

    # Bound the displayed probability-scale limits at zero and one.
    confidence_low = pmax(
      asymp.LCL,
      0
    ),

    confidence_high = pmin(
      asymp.UCL,
      1
    )
  )

print(
  usefulness_probability_by_comfort,
  n = Inf,
  width = Inf
)

stopifnot(
  nrow(usefulness_probability_by_comfort) == 5,
  all(usefulness_probability_by_comfort$predicted_probability >= 0),
  all(usefulness_probability_by_comfort$predicted_probability <= 1),
  all(usefulness_probability_by_comfort$confidence_low >= 0),
  all(usefulness_probability_by_comfort$confidence_high <= 1),
  all(
    usefulness_probability_by_comfort$confidence_low <=
      usefulness_probability_by_comfort$predicted_probability
  ),
  all(
    usefulness_probability_by_comfort$confidence_high >=
      usefulness_probability_by_comfort$predicted_probability
  )
)




# ------------------------------------------------------------------------------
# 15. CREATE THE FINAL UF-THEMED FIGURE
# ------------------------------------------------------------------------------

### Add a lable position column
usefulness_probability_by_comfort <- usefulness_probability_by_comfort |>
  mutate(
    label_y = pmin(confidence_high + 0.04, 1.03),
    percent_label = scales::percent(
      predicted_probability,
      accuracy = 1
    )
  )

uf_blue <- "#0021A5"
uf_orange <- "#FA4616"

figure_caption <- paste0(
  "Adjusted predictions from the final cumulative-link location-scale model. ",
  "AI awareness is held at its sample mean, and prior AI experience is ",
  "averaged over its observed distribution. Error bars represent 95% confidence intervals."
)

usefulness_probability_plot <- ggplot(
  usefulness_probability_by_comfort,
  aes(
    x = ai_comfort,
    y = predicted_probability
  )
) +
  geom_errorbar(
    aes(
      ymin = confidence_low,
      ymax = confidence_high
    ),
    width = 0.10,
    linewidth = 0.8,
    color = uf_blue
  ) +
  geom_line(
    aes(group = 1),
    linewidth = 1.1,
    color = uf_blue
  ) +
  geom_point(
    size = 4,
    color = uf_orange
  ) +
  geom_text(
    aes(
      y = label_y,
      label = percent_label
    ),
    size = 4,
    fontface = "bold"
  ) +
  scale_x_discrete(
    labels = function(x) {
      stringr::str_wrap(x, width = 18)
    }
  ) +
  scale_y_continuous(
    limits = c(0, 1.08),
    breaks = seq(0, 1, 0.2),
    labels = scales::label_percent(accuracy = 1)
  ) +
  labs(
    title = "Adjusted Probability of Perceiving AI Feedback as Useful",
    subtitle = paste0(
      "Probability of selecting Somewhat agree or Strongly agree across levels ",
      "of comfort with AI-generated feedback"
    ),
    x = "Comfort With AI-Generated Feedback",
    y = "Predicted probability",
    caption = stringr::str_wrap(figure_caption, width = 125)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 11.5, margin = margin(b = 18)),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 18, b = 12)),
    plot.caption = element_text(
      hjust = 0,
      size = 9,
      lineheight = 1.15,
      margin = margin(t = 20)
    ),
    plot.margin = margin(t = 20, r = 25, b = 25, l = 20)
  )

print(usefulness_probability_plot)




# ------------------------------------------------------------------------------
# 16. EXPORT FINAL PHASE 4C RESULTS
# ------------------------------------------------------------------------------

readr::write_csv(
  usefulness_probability_by_comfort,
  file.path(
    output_dir,
    "phase4C_final_predicted_probabilities_by_ai_comfort.csv"
  )
)

readr::write_csv(
  phase4C_scale_fit_comparison,
  file.path(
    output_dir,
    "phase4C_final_model_fit_comparison.csv"
  )
)

readr::write_csv(
  comparison_comfort_scale_table,
  file.path(
    output_dir,
    "phase4C_comparison_proportional_vs_location_scale.csv"
  )
)

readr::write_csv(
  comparison_scale_feedback_table,
  file.path(
    output_dir,
    "phase4C_comparison_feedback_added_to_final_model.csv"
  )
)

readr::write_csv(
  final_model_coefficient_matrix,
  file.path(
    output_dir,
    "phase4C_final_model_coefficient_matrix.csv"
  )
)

# Preserve the original descriptive and diagnostic outputs under final names.
readr::write_csv(
  outcome_distribution,
  file.path(
    output_dir,
    "phase4C_final_outcome_distribution.csv"
  )
)

readr::write_csv(
  sparse_cell_summary,
  file.path(
    output_dir,
    "phase4C_final_sparse_cell_diagnostics.csv"
  )
)

readr::write_csv(
  nominal_test_table,
  file.path(
    output_dir,
    "phase4C_final_nominal_effects_diagnostic.csv"
  )
)

readr::write_csv(
  scale_test_table,
  file.path(
    output_dir,
    "phase4C_final_scale_effects_diagnostic.csv"
  )
)

saveRDS(
  final_phase4C_model,
  file.path(
    output_dir,
    "final_phase4C_ai_usefulness_model.rds"
  )
)

saveRDS(
  list(
    final_model = final_phase4C_model,
    proportional_odds_model = model_comfort_added,
    feedback_added_scale_model = model_primary_comfort_scale,
    final_model_fit_comparison = phase4C_scale_fit_comparison,
    proportional_vs_scale_comparison = comparison_comfort_scale,
    feedback_addition_comparison = comparison_scale_feedback,
    predicted_probabilities = usefulness_probability_by_comfort,
    outcome_distribution = outcome_distribution,
    sparse_cell_summary = sparse_cell_summary,
    nominal_test_result = nominal_test_result,
    scale_test_result = scale_test_result,
    mean_awareness_score = mean_awareness_score,
    agreement_cut = agreement_cut,
    likert_levels = likert_levels
  ),
  file.path(
    output_dir,
    "phase4C_final_analysis_checkpoint.rds"
  )
)

capture.output(
  summary(final_phase4C_model),
  file = file.path(
    output_dir,
    "phase4C_final_model_summary.txt"
  )
)

capture.output(
  sessionInfo(),
  file = file.path(
    output_dir,
    "phase4C_session_info.txt"
  )
)

ggsave(
  filename = file.path(
    output_dir,
    "phase4C_adjusted_usefulness_probability.png"
  ),
  plot = usefulness_probability_plot,
  width = 12,
  height = 8.5,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(
    output_dir,
    "phase4C_adjusted_usefulness_probability.pdf"
  ),
  plot = usefulness_probability_plot,
  width = 12,
  height = 8.5,
  device = "pdf"
)

# Create a compact manifest so the output folder is self-documenting.
phase4C_output_manifest <- tibble(
  file = c(
    "final_phase4C_ai_usefulness_model.rds",
    "phase4C_final_analysis_checkpoint.rds",
    "phase4C_final_model_summary.txt",
    "phase4C_session_info.txt",
    "phase4C_final_predicted_probabilities_by_ai_comfort.csv",
    "phase4C_final_model_fit_comparison.csv",
    "phase4C_comparison_proportional_vs_location_scale.csv",
    "phase4C_comparison_feedback_added_to_final_model.csv",
    "phase4C_final_model_coefficient_matrix.csv",
    "phase4C_final_outcome_distribution.csv",
    "phase4C_final_sparse_cell_diagnostics.csv",
    "phase4C_final_nominal_effects_diagnostic.csv",
    "phase4C_final_scale_effects_diagnostic.csv",
    "phase4C_adjusted_usefulness_probability.png",
    "phase4C_adjusted_usefulness_probability.pdf"
  ),
  description = c(
    "Final fitted cumulative-link location-scale model",
    "Complete Phase 4C model and output checkpoint",
    "Printed summary of the final model",
    "R and package version information",
    "Adjusted agreement probabilities across AI-comfort levels",
    "Fit comparison among proportional-odds and location-scale models",
    "Likelihood-ratio comparison of proportional-odds and scale models",
    "Likelihood-ratio test of adding feedback experience",
    "Coefficient matrix from the final model summary",
    "Distribution of the perceived-usefulness outcome",
    "Predictor-by-outcome sparse-cell diagnostics",
    "Nominal-effects diagnostic output",
    "Scale-effects diagnostic output",
    "Report-ready PNG figure",
    "Vector-format PDF figure"
  )
)

readr::write_csv(
  phase4C_output_manifest,
  file.path(
    output_dir,
    "phase4C_output_manifest.csv"
  )
)

cat(
  "\n",
  paste0(
    "PHASE 4C COMPLETE\n",
    "Final model: cumulative-logit location-scale model\n",
    "Location formula: perceived usefulness ~ prior AI experience + ",
    "AI awareness + AI comfort\n",
    "Scale formula: ~ AI comfort\n",
    "All final outputs saved to: ",
    output_dir,
    "\n"
  ),
  sep = ""
)
