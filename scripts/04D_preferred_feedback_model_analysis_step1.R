# ==============================================================================
# PHASE 4D, STEP 1: PREFERRED-FEEDBACK-MODEL ANALYSIS
# Learner Assessment and Feedback Survey
# ==============================================================================
# Outcome
#   preferred_feedback_model:
#   "Which of the following would best describe your preferred feedback model?"
#
# Purpose of Step 1
#   1. Prepare and validate the four-category preferred-model outcome.
#   2. Describe the distribution of preferences and the degree of human review.
#   3. Inspect predictor-by-outcome sparsity before inferential modeling.
#   4. Fit a theoretically ordered sequence of baseline-category multinomial
#      logistic regression models.
#   5. evaluate convergence, numerical stability, model fit, predictor-level
#      omnibus tests, coding assumptions, and teaching-experience adjustment.
#   6. Fit a mean-bias-reduced multinomial sensitivity model because sparse and
#      zero cells may destabilize ordinary maximum-likelihood estimates.
#   7. Save a checkpoint for final-model selection, adjusted probabilities,
#      figures, interpretation, and final exports in Phase 4D, Step 2.
#
# MAIN OUTCOME CATEGORIES
#   Reference:
#     Human-led grading; AI support
#
#   Alternatives:
#     AI initial feedback; universal human review
#     AI-led feedback; disputed-only human review
#     No preference
#
# MODEL SEQUENCE
#   Background model:
#     preferred model ~ prior AI experience + AI awareness
#
#   AI-attitude model:
#     preferred model ~ prior AI experience + AI awareness
#                       + AI comfort + perceived AI usefulness
#
#   Full candidate model:
#     preferred model ~ prior AI experience + AI awareness
#                       + AI comfort + perceived AI usefulness
#                       + feedback experience
#
# RATIONALE
#   - The response categories are substantively distinct but do not form one
#     defensible four-level order because "No preference" is not an ordered point
#     on the human-versus-AI control continuum. A nominal multinomial model is
#     therefore preferable to an ordinal regression.
#   - "Human-led grading; AI support" is the reference because it is the largest
#     category and the most human-controlled model.
#   - Prior AI experience remains categorical because its association need not be
#     linear across the four experience categories.
#   - AI awareness is represented by a 0-1-2 score in the main models; a factor-
#     coded sensitivity model checks the equal-step assumption.
#   - AI comfort and perceived AI usefulness are represented by 0-1-2-3-4 scores
#     in the main models. Factor-coded sensitivity models check whether their
#     one-category linear associations are adequate.
#   - The feedback composite is centered and divided by 0.5, so its coefficient
#     is interpreted for a 0.5-point increase on the original 1-to-5 scale.
#   - Teaching experience is retained as a sensitivity adjustment to preserve
#     parsimony with approximately 169 valid outcome responses.
#   - Maximum-likelihood multinomial models are used for nested model comparison.
#     A brglm2 mean-bias-reduced model is fitted as a stability sensitivity check.
#
# IMPORTANT
#   - Save this file as scripts/04D_preferred_feedback_model_analysis.R.
#   - Run it from the assessment-feedback-analysis RStudio project.
#   - Step 1 deliberately does not freeze a final model. Review the printed model
#     comparisons and diagnostics before proceeding to Step 2.
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. START CLEAN AND LOAD PACKAGES
# ------------------------------------------------------------------------------

rm(list = ls())
graphics.off()
options(scipen = 999)

# Run once if needed:
# install.packages(c("tidyverse", "here", "nnet", "brglm2"))

library(tidyverse)
library(here)
library(nnet)
library(brglm2)

here::i_am(
  "scripts/04D_preferred_feedback_model_analysis_step1.R"
)

cat("Project root:", here(), "\n")


# ------------------------------------------------------------------------------
# 1. DEFINE INPUT AND OUTPUT PATHS
# ------------------------------------------------------------------------------

phase4C_complete_path <- here(
  "output",
  "learner_ai_usefulness_ordinal",
  "learner_analysis_phase4C_complete_cases.rds"
)

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
  "learner_preferred_model_multinomial"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!dir.exists(output_dir)) {
  stop("The Phase 4D output directory could not be created.")
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

# Match the Phase 4A/4C sensitivity composite: specificity excluded.
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

if (file.exists(phase4C_complete_path)) {
  learner <- readRDS(phase4C_complete_path)
  input_source <- "Phase 4C complete-case RDS"

} else if (file.exists(phase4A_rds_path)) {
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
      "No valid Phase 4D input file was found.\n\n",
      "Expected one of the following:\n",
      phase4C_complete_path, "\n",
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

  learner$feedback_experience <- rowMeans(
    feedback_numeric,
    na.rm = FALSE
  )

  learner$feedback_experience_3item <- rowMeans(
    feedback_numeric |>
      select(
        all_of(feedback_items_3item)
      ),
    na.rm = FALSE
  )
}


# ------------------------------------------------------------------------------
# 4. VALIDATE REQUIRED VARIABLES AND RESPONSE LABELS
# ------------------------------------------------------------------------------

required_variables <- c(
  "case_id",
  "preferred_feedback_model",
  "prior_ai_experience",
  "ai_awareness",
  "ai_comfort",
  "ai_as_useful_as_human",
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
    "Required Phase 4D variables are missing: ",
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

preferred_model_original_levels <- c(
  "Human instructor grades everything; AI is only used as a support tool for the instructor.",
  "AI provides initial feedback; human instructor reviews every submission before grades are final.",
  "AI provides all feedback; human instructor reviews only disputed grades.",
  "I have no preference."
)

preferred_model_short_levels <- c(
  "Human-led grading; AI support",
  "AI initial feedback; universal human review",
  "AI-led feedback; disputed-only human review",
  "No preference"
)

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
  learner$preferred_feedback_model,
  preferred_model_original_levels,
  "preferred_feedback_model"
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
  learner$ai_comfort,
  likert_levels,
  "ai_comfort"
)

check_labels(
  learner$ai_as_useful_as_human,
  likert_levels,
  "ai_as_useful_as_human"
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
    # Unordered nominal outcome. The first level is the reference category.
    preferred_model = factor(
      as.character(preferred_feedback_model),
      levels = preferred_model_original_levels,
      labels = preferred_model_short_levels
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

    ai_comfort_factor = factor(
      as.character(ai_comfort),
      levels = likert_levels
    ),

    ai_comfort_score = case_when(
      as.character(ai_comfort) == "Strongly disagree" ~ 0,
      as.character(ai_comfort) == "Somewhat disagree" ~ 1,
      as.character(ai_comfort) == "Neither agree nor disagree" ~ 2,
      as.character(ai_comfort) == "Somewhat agree" ~ 3,
      as.character(ai_comfort) == "Strongly agree" ~ 4,
      TRUE ~ NA_real_
    ),

    ai_usefulness_factor = factor(
      as.character(ai_as_useful_as_human),
      levels = likert_levels
    ),

    ai_usefulness_score = case_when(
      as.character(ai_as_useful_as_human) == "Strongly disagree" ~ 0,
      as.character(ai_as_useful_as_human) == "Somewhat disagree" ~ 1,
      as.character(ai_as_useful_as_human) == "Neither agree nor disagree" ~ 2,
      as.character(ai_as_useful_as_human) == "Somewhat agree" ~ 3,
      as.character(ai_as_useful_as_human) == "Strongly agree" ~ 4,
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
      (feedback_experience_3item - feedback_experience_3item_mean) / 0.5,

    expressed_preference = case_when(
      is.na(preferred_model) ~ NA,
      preferred_model == "No preference" ~ FALSE,
      TRUE ~ TRUE
    ),

    universal_human_review = case_when(
      preferred_model %in% c(
        "Human-led grading; AI support",
        "AI initial feedback; universal human review"
      ) ~ 1,
      preferred_model == "AI-led feedback; disputed-only human review" ~ 0,
      TRUE ~ NA_real_
    ),

    human_led_within_universal_review = case_when(
      preferred_model == "Human-led grading; AI support" ~ 1,
      preferred_model == "AI initial feedback; universal human review" ~ 0,
      TRUE ~ NA_real_
    )
  )

stopifnot(
  levels(analysis_data$preferred_model)[1] ==
    "Human-led grading; AI support"
)


# ------------------------------------------------------------------------------
# 6. CHECK MISSINGNESS AND CREATE THE COMMON ANALYTIC SAMPLE
# ------------------------------------------------------------------------------

model_variables <- c(
  "preferred_model",
  "prior_ai_experience",
  "ai_awareness_score",
  "ai_comfort_score",
  "ai_usefulness_score",
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
    preferred_model,
    prior_ai_experience,
    ai_awareness_score,
    ai_comfort_score,
    ai_usefulness_score,
    feedback_experience_half_point,
    feedback_experience_3item_half_point,
    teaching_experience
  )

cat(
  "Complete cases available for all Phase 4D models:",
  nrow(analysis_complete),
  "\n"
)

if (nrow(analysis_complete) != 169) {
  warning(
    "The common Phase 4D analytic sample contains ",
    nrow(analysis_complete),
    " cases rather than the expected 169."
  )
}

outcome_counts <- table(
  analysis_complete$preferred_model
)

if (any(outcome_counts < 20)) {
  warning(
    "At least one preferred-model category contains fewer than 20 cases. ",
    "Use parsimonious models and interpret large coefficients cautiously."
  )
}


# ------------------------------------------------------------------------------
# 7. DESCRIBE PREFERRED-MODEL RESPONSES AND HUMAN-REVIEW PATTERNS
# ------------------------------------------------------------------------------

preferred_model_distribution <- analysis_complete |>
  count(
    preferred_model,
    .drop = FALSE,
    name = "n"
  ) |>
  mutate(
    percent = 100 * n / sum(n),
    valid_n = sum(n)
  )

print(
  preferred_model_distribution,
  n = Inf
)

valid_preference_n <- nrow(analysis_complete)

directional_preference_n <- sum(
  analysis_complete$expressed_preference,
  na.rm = TRUE
)

universal_review_n <- sum(
  analysis_complete$preferred_model %in% c(
    "Human-led grading; AI support",
    "AI initial feedback; universal human review"
  )
)

selective_review_n <- sum(
  analysis_complete$preferred_model ==
    "AI-led feedback; disputed-only human review"
)

human_led_n <- sum(
  analysis_complete$preferred_model ==
    "Human-led grading; AI support"
)

ai_initial_universal_n <- sum(
  analysis_complete$preferred_model ==
    "AI initial feedback; universal human review"
)

no_preference_n <- sum(
  analysis_complete$preferred_model ==
    "No preference"
)

human_review_summary <- tibble(
  metric = c(
    "Valid preferred-model responses",
    "Expressed a directional model preference",
    "No preference",
    "Preferred universal human review",
    "Preferred disputed-only human review",
    "Preferred human-led grading with AI support",
    "Preferred AI initial feedback with universal human review",
    "Universal review among directional preferences",
    "Human-led grading among universal-review preferences"
  ),

  n = c(
    valid_preference_n,
    directional_preference_n,
    no_preference_n,
    universal_review_n,
    selective_review_n,
    human_led_n,
    ai_initial_universal_n,
    universal_review_n,
    human_led_n
  ),

  denominator = c(
    valid_preference_n,
    valid_preference_n,
    valid_preference_n,
    valid_preference_n,
    valid_preference_n,
    valid_preference_n,
    valid_preference_n,
    directional_preference_n,
    universal_review_n
  )
) |>
  mutate(
    percent = 100 * n / denominator
  )

print(
  human_review_summary,
  n = Inf
)


# ------------------------------------------------------------------------------
# 8. INSPECT PREDICTOR-BY-OUTCOME SPARSITY
# ------------------------------------------------------------------------------

make_preference_crosstab <- function(
  data,
  predictor_variable,
  predictor_label
) {
  data |>
    count(
      .data[[predictor_variable]],
      preferred_model,
      .drop = FALSE,
      name = "n"
    ) |>
    group_by(
      .data[[predictor_variable]]
    ) |>
    mutate(
      row_percent = 100 * n / sum(n)
    ) |>
    ungroup() |>
    transmute(
      predictor = predictor_label,
      predictor_level = as.character(
        .data[[predictor_variable]]
      ),
      preferred_model = as.character(preferred_model),
      n,
      row_percent,
      sparse_cell = n < 5,
      zero_cell = n == 0
    )
}

sparse_cell_summary <- bind_rows(
  make_preference_crosstab(
    analysis_complete,
    "prior_ai_experience",
    "Prior AI experience"
  ),

  make_preference_crosstab(
    analysis_complete,
    "ai_awareness_factor",
    "AI awareness"
  ),

  make_preference_crosstab(
    analysis_complete,
    "ai_comfort_factor",
    "AI comfort"
  ),

  make_preference_crosstab(
    analysis_complete,
    "ai_usefulness_factor",
    "Perceived AI usefulness"
  ),

  make_preference_crosstab(
    analysis_complete,
    "teaching_experience",
    "Teaching experience"
  )
)

cat(
  "Predictor-by-outcome cells with n < 5:",
  sum(sparse_cell_summary$sparse_cell),
  "\n"
)

cat(
  "Predictor-by-outcome cells with n = 0:",
  sum(sparse_cell_summary$zero_cell),
  "\n"
)

print(
  sparse_cell_summary |>
    filter(sparse_cell),
  n = Inf
)

attitude_correlation <- cor.test(
  analysis_complete$ai_comfort_score,
  analysis_complete$ai_usefulness_score,
  method = "spearman",
  exact = FALSE
)

cat(
  "\nSpearman association between AI comfort and perceived AI usefulness:\n"
)

print(attitude_correlation)


# ------------------------------------------------------------------------------
# 9. FIT THE MAXIMUM-LIKELIHOOD MULTINOMIAL MODEL SEQUENCE
# ------------------------------------------------------------------------------

fit_multinom <- function(formula, data) {
  nnet::multinom(
    formula = formula,
    data = data,
    Hess = TRUE,
    model = TRUE,
    trace = FALSE,
    maxit = 1000,
    MaxNWts = 10000
  )
}

model_null <- fit_multinom(
  preferred_model ~ 1,
  data = analysis_complete
)

model_background <- fit_multinom(
  preferred_model ~
    prior_ai_experience +
    ai_awareness_score,
  data = analysis_complete
)

# AI comfort added without perceived AI usefulness.
model_comfort_only <- fit_multinom(
  preferred_model ~
    prior_ai_experience +
    ai_awareness_score +
    ai_comfort_score,
  data = analysis_complete
)

# Perceived AI usefulness added without AI comfort.
model_usefulness_only <- fit_multinom(
  preferred_model ~
    prior_ai_experience +
    ai_awareness_score +
    ai_usefulness_score,
  data = analysis_complete
)

model_attitudes <- fit_multinom(
  preferred_model ~
    prior_ai_experience +
    ai_awareness_score +
    ai_comfort_score +
    ai_usefulness_score,
  data = analysis_complete
)

# Full Phase 4D candidate model.
model_primary <- fit_multinom(
  preferred_model ~
    prior_ai_experience +
    ai_awareness_score +
    ai_comfort_score +
    ai_usefulness_score +
    feedback_experience_half_point,
  data = analysis_complete
)

# Sensitivity 1: teaching-experience adjustment.
model_teaching_adjusted <- fit_multinom(
  preferred_model ~
    prior_ai_experience +
    ai_awareness_score +
    ai_comfort_score +
    ai_usefulness_score +
    feedback_experience_half_point +
    teaching_experience,
  data = analysis_complete
)

# Sensitivity 2: awareness as a categorical factor.
model_awareness_factor <- fit_multinom(
  preferred_model ~
    prior_ai_experience +
    ai_awareness_factor +
    ai_comfort_score +
    ai_usefulness_score +
    feedback_experience_half_point,
  data = analysis_complete
)

# Sensitivity 3: AI comfort as a categorical factor.
model_comfort_factor <- fit_multinom(
  preferred_model ~
    prior_ai_experience +
    ai_awareness_score +
    ai_comfort_factor +
    ai_usefulness_score +
    feedback_experience_half_point,
  data = analysis_complete
)

# Sensitivity 4: perceived AI usefulness as a categorical factor.
model_usefulness_factor <- fit_multinom(
  preferred_model ~
    prior_ai_experience +
    ai_awareness_score +
    ai_comfort_score +
    ai_usefulness_factor +
    feedback_experience_half_point,
  data = analysis_complete
)

# Sensitivity 5: replace the four-item composite with the three-item composite.
model_three_item_composite <- fit_multinom(
  preferred_model ~
    prior_ai_experience +
    ai_awareness_score +
    ai_comfort_score +
    ai_usefulness_score +
    feedback_experience_3item_half_point,
  data = analysis_complete
)


# ------------------------------------------------------------------------------
# 10. VERIFY CONVERGENCE AND NUMERICAL STABILITY
# ------------------------------------------------------------------------------

extract_multinom_diagnostics <- function(model) {
  model_summary <- summary(model)

  coefficient_values <- as.numeric(
    model_summary$coefficients
  )

  standard_error_values <- as.numeric(
    model_summary$standard.errors
  )

  hessian_condition <- NA_real_

  if (
    is.matrix(model$Hessian) &&
    all(is.finite(model$Hessian))
  ) {
    hessian_condition <- tryCatch(
      kappa(
        model$Hessian,
        exact = TRUE
      ),
      error = function(e) NA_real_
    )
  }

  list(
    convergence_code = as.integer(model$convergence),
    all_coefficients_finite = all(is.finite(coefficient_values)),
    all_standard_errors_finite = all(is.finite(standard_error_values)),
    maximum_absolute_coefficient = max(
      abs(coefficient_values),
      na.rm = TRUE
    ),
    maximum_standard_error = max(
      standard_error_values,
      na.rm = TRUE
    ),
    hessian_condition_number = hessian_condition
  )
}

check_multinom_model <- function(model, model_name) {
  diagnostics <- extract_multinom_diagnostics(model)

  if (diagnostics$convergence_code != 0L) {
    stop(
      model_name,
      " did not converge successfully. Convergence code: ",
      diagnostics$convergence_code
    )
  }

  if (!diagnostics$all_coefficients_finite) {
    stop(
      model_name,
      " contains a non-finite coefficient."
    )
  }

  if (!diagnostics$all_standard_errors_finite) {
    stop(
      model_name,
      " contains a non-finite standard error."
    )
  }

  if (diagnostics$maximum_absolute_coefficient > 10) {
    warning(
      model_name,
      " contains a coefficient with absolute value above 10: ",
      signif(
        diagnostics$maximum_absolute_coefficient,
        4
      ),
      ". This may indicate sparse-data instability or separation."
    )
  }

  if (diagnostics$maximum_standard_error > 5) {
    warning(
      model_name,
      " contains a standard error above 5: ",
      signif(
        diagnostics$maximum_standard_error,
        4
      ),
      ". Interpret individual contrasts cautiously."
    )
  }

  if (
    is.finite(diagnostics$hessian_condition_number) &&
    diagnostics$hessian_condition_number > 1000000
  ) {
    warning(
      model_name,
      " has a large Hessian condition number: ",
      signif(
        diagnostics$hessian_condition_number,
        4
      )
    )
  }

  invisible(diagnostics)
}

models_to_check <- list(
  "Null model" = model_null,
  "Background model" = model_background,
  "Comfort-only model" = model_comfort_only,
  "Usefulness-only model" = model_usefulness_only,
  "AI-attitude model" = model_attitudes,
  "Full candidate model" = model_primary,
  "Teaching-adjusted model" = model_teaching_adjusted,
  "Awareness-factor model" = model_awareness_factor,
  "Comfort-factor model" = model_comfort_factor,
  "Usefulness-factor model" = model_usefulness_factor,
  "Three-item-composite model" = model_three_item_composite
)

purrr::iwalk(
  models_to_check,
  ~ check_multinom_model(.x, .y)
)

cat("\nFull candidate multinomial model summary:\n")
print(summary(model_primary))


# ------------------------------------------------------------------------------
# 11. SUMMARIZE MODEL FIT AND COMPARE NESTED MODELS
# ------------------------------------------------------------------------------

### helper function
extract_multinom_n <- function(model) {
  if (
    !is.null(model$model) &&
    is.data.frame(model$model)
  ) {
    return(
      nrow(model$model)
    )
  }
  
  if (!is.null(model$fitted.values)) {
    fitted_matrix <- as.matrix(
      model$fitted.values
    )
    
    return(
      nrow(fitted_matrix)
    )
  }
  
  stop(
    "The multinomial model does not contain enough information ",
    "to determine its analytic sample size."
  )
}

### Extract model fit
extract_model_fit <- function(
    fitted_model,
    model_name
) {
  model_log_likelihood <- logLik(
    fitted_model
  )
  
  diagnostics <- extract_multinom_diagnostics(
    fitted_model
  )
  
  tibble(
    model = model_name,
    
    n = nrow(
      fitted_model$model
    ),
    
    number_of_parameters = attr(
      model_log_likelihood,
      "df"
    ),
    
    log_likelihood = as.numeric(
      model_log_likelihood
    ),
    
    AIC = AIC(
      fitted_model
    ),
    
    BIC = BIC(
      fitted_model
    ),
    
    convergence_code =
      diagnostics$convergence_code,
    
    maximum_absolute_coefficient =
      diagnostics$maximum_absolute_coefficient,
    
    maximum_standard_error =
      diagnostics$maximum_standard_error,
    
    hessian_condition_number =
      diagnostics$hessian_condition_number
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


# ------------------------------------------------------------------------------
# 12. CONDUCT TERM-LEVEL OMNIBUS LIKELIHOOD-RATIO TESTS
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# HELPER: EXTRACT MULTINOMIAL MODEL SAMPLE SIZE
# ------------------------------------------------------------------------------

extract_multinom_n <- function(model) {
  if (
    !is.null(model$model) &&
    is.data.frame(model$model)
  ) {
    return(
      nrow(model$model)
    )
  }
  
  if (!is.null(model$fitted.values)) {
    return(
      nrow(
        as.matrix(model$fitted.values)
      )
    )
  }
  
  stop(
    "The multinomial model does not contain enough information ",
    "to determine its analytic sample size."
  )
}


# ------------------------------------------------------------------------------
# HELPER: COMPARE NESTED MULTINOMIAL MODELS
# ------------------------------------------------------------------------------

compare_nested_multinom <- function(
    smaller_model,
    larger_model,
    comparison_name
) {
  smaller_n <- extract_multinom_n(
    smaller_model
  )
  
  larger_n <- extract_multinom_n(
    larger_model
  )
  
  if (smaller_n != larger_n) {
    stop(
      comparison_name,
      ": models were fitted to different sample sizes. ",
      "Smaller model n = ",
      smaller_n,
      "; larger model n = ",
      larger_n,
      "."
    )
  }
  
  smaller_log_likelihood <- logLik(
    smaller_model
  )
  
  larger_log_likelihood <- logLik(
    larger_model
  )
  
  likelihood_ratio <- 2 * (
    as.numeric(larger_log_likelihood) -
      as.numeric(smaller_log_likelihood)
  )
  
  df_difference <- attr(
    larger_log_likelihood,
    "df"
  ) - attr(
    smaller_log_likelihood,
    "df"
  )
  
  if (df_difference <= 0) {
    stop(
      comparison_name,
      ": the larger model must contain more parameters ",
      "than the smaller model."
    )
  }
  
  tibble(
    comparison = as.character(
      comparison_name
    ),
    
    smaller_model_n = smaller_n,
    
    larger_model_n = larger_n,
    
    smaller_model_log_likelihood =
      as.numeric(smaller_log_likelihood),
    
    larger_model_log_likelihood =
      as.numeric(larger_log_likelihood),
    
    likelihood_ratio_chisquare =
      likelihood_ratio,
    
    df = df_difference,
    
    p_value = pchisq(
      likelihood_ratio,
      df = df_difference,
      lower.tail = FALSE
    )
  )
}

#-------------------------------------------
### Omnibus test

primary_omnibus_tests <- purrr::map_dfr(
  primary_predictor_terms,
  function(term_to_remove) {
    reduced_formula <- reformulate(
      termlabels = setdiff(
        primary_predictor_terms,
        term_to_remove
      ),
      response = "preferred_model"
    )
    
    reduced_model <- fit_multinom(
      reduced_formula,
      data = analysis_complete
    )
    
    compare_nested_multinom(
      smaller_model = reduced_model,
      larger_model = model_primary,
      comparison_name = unname(
        primary_term_labels[
          term_to_remove
        ]
      )
    ) |>
      rename(
        term = comparison
      )
  }
)

print(
  primary_omnibus_tests,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 12A. FIT AND COMPARE PARSIMONIOUS FINAL-MODEL CANDIDATES
# ------------------------------------------------------------------------------

# Retain the two predictors that made significant unique contributions
# in the full candidate model.
model_usefulness_feedback <- fit_multinom(
  preferred_model ~
    ai_usefulness_score +
    feedback_experience_half_point,
  data = analysis_complete
)

# Check whether AI comfort improves the reduced model after the
# nonsignificant background predictors are removed.
model_comfort_usefulness_feedback <- fit_multinom(
  preferred_model ~
    ai_comfort_score +
    ai_usefulness_score +
    feedback_experience_half_point,
  data = analysis_complete
)

check_multinom_model(
  model_usefulness_feedback,
  "Usefulness-and-feedback model"
)

check_multinom_model(
  model_comfort_usefulness_feedback,
  "Comfort-usefulness-feedback model"
)

final_candidate_comparisons <- bind_rows(
  compare_nested_multinom(
    model_usefulness_feedback,
    model_comfort_usefulness_feedback,
    "Usefulness-feedback versus comfort-added model"
  ),
  
  compare_nested_multinom(
    model_comfort_usefulness_feedback,
    model_primary,
    "Reduced attitude model versus full background-adjusted model"
  )
)

print(
  final_candidate_comparisons,
  n = Inf,
  width = Inf
)

final_candidate_fit <- bind_rows(
  extract_model_fit(
    model_usefulness_feedback,
    "Usefulness-and-feedback model"
  ),
  
  extract_model_fit(
    model_comfort_usefulness_feedback,
    "Comfort-usefulness-feedback model"
  ),
  
  extract_model_fit(
    model_primary,
    "Full background-adjusted model"
  )
)

print(
  final_candidate_fit,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------------------------
# 12B. SELECT THE FINAL PHASE 4D MODEL
# ------------------------------------------------------------------------------

# The usefulness-and-feedback model is retained because it has the lowest
# AIC and BIC, requires the fewest parameters, converges successfully, and
# provides better numerical stability than the more complex alternatives.
final_phase4D_model <- model_usefulness_feedback

cat(
  "\nSelected Phase 4D model:\n",
  "Preferred feedback model ~ perceived AI usefulness + feedback experience\n"
)

print(
  summary(final_phase4D_model)
)


# ------------------------------------------------------------------------------
# 12C. TEST WHETHER FEEDBACK EXPERIENCE IMPROVES THE REDUCED MODEL
# ------------------------------------------------------------------------------

model_reduced_usefulness_only <- fit_multinom(
  preferred_model ~
    ai_usefulness_score,
  data = analysis_complete
)

model_reduced_feedback_only <- fit_multinom(
  preferred_model ~
    feedback_experience_half_point,
  data = analysis_complete
)

check_multinom_model(
  model_reduced_usefulness_only,
  "Reduced usefulness-only model"
)

check_multinom_model(
  model_reduced_feedback_only,
  "Reduced feedback-only model"
)

reduced_model_comparisons <- bind_rows(
  compare_nested_multinom(
    model_reduced_usefulness_only,
    model_usefulness_feedback,
    "Usefulness only versus usefulness plus feedback"
  ),
  
  compare_nested_multinom(
    model_reduced_feedback_only,
    model_usefulness_feedback,
    "Feedback only versus feedback plus usefulness"
  )
)

print(
  reduced_model_comparisons,
  n = Inf,
  width = Inf
)

reduced_model_fit <- bind_rows(
  extract_model_fit(
    model_reduced_usefulness_only,
    "Reduced usefulness-only model"
  ),
  
  extract_model_fit(
    model_reduced_feedback_only,
    "Reduced feedback-only model"
  ),
  
  extract_model_fit(
    model_usefulness_feedback,
    "Usefulness-and-feedback model"
  )
)

print(
  reduced_model_fit,
  n = Inf,
  width = Inf
)



# ------------------------------------------------------------------------------
# 12D. FREEZE THE FINAL PHASE 4D MODEL
# ------------------------------------------------------------------------------

final_phase4D_model <- model_usefulness_feedback

final_phase4D_model_selection <- tibble(
  selected_model =
    "Usefulness-and-feedback multinomial model",
  
  formula =
    "preferred_model ~ ai_usefulness_score + feedback_experience_half_point",
  
  analytic_n =
    nrow(final_phase4D_model$model),
  
  number_of_parameters =
    attr(
      logLik(final_phase4D_model),
      "df"
    ),
  
  log_likelihood =
    as.numeric(
      logLik(final_phase4D_model)
    ),
  
  AIC =
    AIC(final_phase4D_model),
  
  BIC =
    BIC(final_phase4D_model),
  
  feedback_addition_likelihood_ratio =
    reduced_model_comparisons |>
    filter(
      comparison ==
        "Usefulness only versus usefulness plus feedback"
    ) |>
    pull(
      likelihood_ratio_chisquare
    ),
  
  feedback_addition_df =
    reduced_model_comparisons |>
    filter(
      comparison ==
        "Usefulness only versus usefulness plus feedback"
    ) |>
    pull(
      df
    ),
  
  feedback_addition_p_value =
    reduced_model_comparisons |>
    filter(
      comparison ==
        "Usefulness only versus usefulness plus feedback"
    ) |>
    pull(
      p_value
    )
)

print(
  final_phase4D_model_selection,
  n = Inf,
  width = Inf
)



# ------------------------------------------------------------------------------
# HELPER: TIDY MULTINOMIAL COEFFICIENTS
# ------------------------------------------------------------------------------

tidy_multinom_coefficients <- function(
    model,
    reference_category
) {
  model_summary <- summary(model)
  
  coefficient_matrix <- as.matrix(
    model_summary$coefficients
  )
  
  standard_error_matrix <- as.matrix(
    model_summary$standard.errors
  )
  
  if (
    !identical(
      dim(coefficient_matrix),
      dim(standard_error_matrix)
    )
  ) {
    stop(
      "Coefficient and standard-error matrices have incompatible dimensions."
    )
  }
  
  purrr::map_dfr(
    seq_len(
      nrow(coefficient_matrix)
    ),
    function(row_index) {
      estimate <- coefficient_matrix[
        row_index,
      ]
      
      standard_error <- standard_error_matrix[
        row_index,
      ]
      
      z_value <- estimate / standard_error
      
      tibble(
        outcome_category = rownames(
          coefficient_matrix
        )[row_index],
        
        reference_category =
          reference_category,
        
        term = colnames(
          coefficient_matrix
        ),
        
        estimate = as.numeric(
          estimate
        ),
        
        standard_error = as.numeric(
          standard_error
        ),
        
        z_value = as.numeric(
          z_value
        ),
        
        p_value = 2 * pnorm(
          abs(z_value),
          lower.tail = FALSE
        ),
        
        relative_risk_ratio = exp(
          estimate
        ),
        
        confidence_low = exp(
          estimate -
            qnorm(0.975) * standard_error
        ),
        
        confidence_high = exp(
          estimate +
            qnorm(0.975) * standard_error
        )
      )
    }
  )
}


# ------------------------------------------------------------------------------
# 13. CREATE THE FINAL-MODEL COEFFICIENT TABLE
# ------------------------------------------------------------------------------
final_coefficient_table <- tidy_multinom_coefficients(
  final_phase4D_model,
  reference_category =
    "Human-led grading; AI support"
)

print(
  final_coefficient_table |>
    filter(term != "(Intercept)"),
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------------------------
# 14. FIT THE FINAL MEAN-BIAS-REDUCED SENSITIVITY MODEL
# ------------------------------------------------------------------------------

final_phase4D_bias_reduced_model <- brglm2::brmultinom(
  preferred_model ~
    ai_usefulness_score +
    feedback_experience_half_point,
  data = analysis_complete,
  type = "AS_mean",
  ref = 1
)

cat(
  "\nFinal mean-bias-reduced multinomial sensitivity model:\n"
)

print(
  summary(final_phase4D_bias_reduced_model)
)

bias_reduced_coefficients <- unlist(
  coef(final_phase4D_bias_reduced_model),
  use.names = TRUE
)

bias_reduced_diagnostics <- tibble(
  estimator =
    "Mean-bias-reduced multinomial logit",
  
  all_coefficients_finite =
    all(
      is.finite(
        bias_reduced_coefficients
      )
    ),
  
  maximum_absolute_coefficient =
    max(
      abs(
        bias_reduced_coefficients
      ),
      na.rm = TRUE
    ),
  
  number_of_coefficients =
    length(
      bias_reduced_coefficients
    )
)

print(
  bias_reduced_diagnostics,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 14A. COMPARE MAXIMUM-LIKELIHOOD AND BIAS-REDUCED ESTIMATES
# ------------------------------------------------------------------------------

extract_multinom_estimates <- function(
    model,
    estimator_label
) {
  coefficient_matrix <- as.matrix(
    coef(model)
  )
  
  purrr::map_dfr(
    seq_len(
      nrow(coefficient_matrix)
    ),
    function(row_index) {
      tibble(
        outcome_category =
          rownames(coefficient_matrix)[row_index],
        
        term =
          colnames(coefficient_matrix),
        
        estimator =
          estimator_label,
        
        estimate =
          as.numeric(
            coefficient_matrix[row_index, ]
          ),
        
        relative_risk_ratio =
          exp(
            as.numeric(
              coefficient_matrix[row_index, ]
            )
          )
      )
    }
  )
}

maximum_likelihood_estimates <- extract_multinom_estimates(
  final_phase4D_model,
  "Maximum likelihood"
)

bias_reduced_estimates <- extract_multinom_estimates(
  final_phase4D_bias_reduced_model,
  "Mean bias reduced"
)

phase4D_estimator_comparison <- bind_rows(
  maximum_likelihood_estimates,
  bias_reduced_estimates
) |>
  filter(
    term != "(Intercept)"
  ) |>
  mutate(
    estimator = case_when(
      estimator == "Maximum likelihood" ~
        "maximum_likelihood",
      
      estimator == "Mean bias reduced" ~
        "mean_bias_reduced",
      
      TRUE ~ estimator
    )
  ) |>
  pivot_wider(
    names_from = estimator,
    values_from = c(
      estimate,
      relative_risk_ratio
    )
  ) |>
  mutate(
    absolute_estimate_difference =
      abs(
        estimate_maximum_likelihood -
          estimate_mean_bias_reduced
      ),
    
    percent_RRR_difference =
      100 *
      abs(
        relative_risk_ratio_maximum_likelihood -
          relative_risk_ratio_mean_bias_reduced
      ) /
      relative_risk_ratio_maximum_likelihood
  )

print(
  phase4D_estimator_comparison,
  n = Inf,
  width = Inf
)


### Export the comparison table
readr::write_csv(
  phase4D_estimator_comparison,
  file.path(
    output_dir,
    "phase4D_final_ml_vs_bias_reduced_comparison.csv"
  )
)


# ------------------------------------------------------------------------------
# 14B. REFIT FINAL MODEL WITH AN EXPLICIT STORED CALL
# ------------------------------------------------------------------------------

final_phase4D_model <- nnet::multinom(
  preferred_model ~
    ai_usefulness_score +
    feedback_experience_half_point,
  
  data = analysis_complete,
  
  Hess = TRUE,
  model = TRUE,
  trace = FALSE,
  maxit = 1000,
  MaxNWts = 10000
)

check_multinom_model(
  final_phase4D_model,
  "Explicitly refitted final Phase 4D model"
)

print(
  final_phase4D_model$call
)

# ------------------------------------------------------------------------------
# 15. CALCULATE ADJUSTED PREDICTED PROBABILITIES
# ------------------------------------------------------------------------------

# Perceived AI usefulness was coded from 0 to 4:
#   0 = Strongly disagree
#   1 = Somewhat disagree
#   2 = Neither agree nor disagree
#   3 = Somewhat agree
#   4 = Strongly agree
#
# feedback_experience_half_point is centered at the sample mean.
# Therefore, setting it to zero holds feedback experience at its sample mean.

ai_usefulness_levels <- c(
  "Strongly disagree",
  "Somewhat disagree",
  "Neither agree nor disagree",
  "Somewhat agree",
  "Strongly agree"
)

stopifnot(
  setequal(
    unique(
      analysis_complete$ai_usefulness_score
    ),
    0:4
  )
)

feedback_experience_reference <- mean(
  analysis_complete$feedback_experience,
  na.rm = TRUE
)

cat(
  "\nFeedback experience held at sample mean:",
  round(
    feedback_experience_reference,
    3
  ),
  "\n"
)


# ------------------------------------------------------------------------------
# 15A. OBTAIN MODEL-ADJUSTED CATEGORY PROBABILITIES
# ------------------------------------------------------------------------------

phase4D_probability_grid <- emmeans::ref_grid(
  final_phase4D_model,
  
  mode = "prob",
  
  at = list(
    ai_usefulness_score = 0:4,
    feedback_experience_half_point = 0
  )
)

print(
  phase4D_probability_grid
)

phase4D_probability_emmeans <- emmeans::emmeans(
  phase4D_probability_grid,
  
  specs =
    ~ preferred_model |
    ai_usefulness_score
)

phase4D_probability_raw <- summary(
  phase4D_probability_emmeans,
  infer = c(TRUE, FALSE),
  level = 0.95,
  df = Inf
) |>
  as.data.frame() |>
  as_tibble()

print(
  phase4D_probability_raw,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 15B. CREATE A CLEAN, LABELED PROBABILITY TABLE
# ------------------------------------------------------------------------------

ai_usefulness_levels <- c(
  "Strongly disagree",
  "Somewhat disagree",
  "Neither agree nor disagree",
  "Somewhat agree",
  "Strongly agree"
)

confidence_low_variable <- intersect(
  c(
    "asymp.LCL",
    "lower.CL"
  ),
  names(
    phase4D_probability_raw
  )
)

confidence_high_variable <- intersect(
  c(
    "asymp.UCL",
    "upper.CL"
  ),
  names(
    phase4D_probability_raw
  )
)

if (
  length(confidence_low_variable) != 1 ||
  length(confidence_high_variable) != 1
) {
  stop(
    "The probability confidence-limit columns could not be identified."
  )
}

phase4D_predicted_probabilities <- phase4D_probability_raw |>
  transmute(
    ai_usefulness_score,
    
    ai_usefulness = factor(
      ai_usefulness_score,
      levels = 0:4,
      labels = ai_usefulness_levels,
      ordered = TRUE
    ),
    
    preferred_model = factor(
      as.character(
        preferred_model
      ),
      levels = levels(
        analysis_complete$preferred_model
      )
    ),
    
    predicted_probability = prob,
    
    standard_error = SE,
    
    confidence_low_raw =
      .data[[confidence_low_variable]],
    
    confidence_high_raw =
      .data[[confidence_high_variable]],
    
    confidence_low = pmax(
      .data[[confidence_low_variable]],
      0
    ),
    
    confidence_high = pmin(
      .data[[confidence_high_variable]],
      1
    )
  )

print(
  phase4D_predicted_probabilities,
  n = Inf,
  width = Inf
)



# ------------------------------------------------------------------------------
# 15C. VALIDATE THE PREDICTED PROBABILITIES
# ------------------------------------------------------------------------------

phase4D_probability_checks <- phase4D_predicted_probabilities |>
  group_by(
    ai_usefulness_score,
    ai_usefulness
  ) |>
  summarise(
    number_of_outcomes = n(),
    
    probability_sum = sum(
      predicted_probability
    ),
    
    minimum_probability = min(
      predicted_probability
    ),
    
    maximum_probability = max(
      predicted_probability
    ),
    
    .groups = "drop"
  )

print(
  phase4D_probability_checks,
  n = Inf
)

stopifnot(
  nrow(
    phase4D_predicted_probabilities
  ) == 20,
  
  all(
    phase4D_predicted_probabilities$
      predicted_probability >= 0
  ),
  
  all(
    phase4D_predicted_probabilities$
      predicted_probability <= 1
  ),
  
  all(
    phase4D_predicted_probabilities$
      confidence_low >= 0
  ),
  
  all(
    phase4D_predicted_probabilities$
      confidence_high <= 1
  ),
  
  all(
    phase4D_probability_checks$
      number_of_outcomes == 4
  ),
  
  all(
    abs(
      phase4D_probability_checks$
        probability_sum - 1
    ) < 0.000001
  )
)


# ------------------------------------------------------------------------------
# 15D. ADD OBSERVED SAMPLE SIZES BY AI-USEFULNESS LEVEL
# ------------------------------------------------------------------------------

ai_usefulness_observed_n <- analysis_complete |>
  count(
    ai_usefulness_score,
    name = "observed_n"
  )

phase4D_predicted_probabilities <- phase4D_predicted_probabilities |>
  left_join(
    ai_usefulness_observed_n,
    by = "ai_usefulness_score"
  ) |>
  relocate(
    observed_n,
    .after = ai_usefulness
  )

print(
  phase4D_predicted_probabilities,
  n = Inf,
  width = Inf
)



# ------------------------------------------------------------------------------
# 15E. CREATE A WIDE-FORMAT PROBABILITY SUMMARY
# ------------------------------------------------------------------------------

phase4D_probability_wide <- phase4D_predicted_probabilities |>
  select(
    ai_usefulness_score,
    ai_usefulness,
    observed_n,
    preferred_model,
    predicted_probability
  ) |>
  mutate(
    predicted_percent = scales::percent(
      predicted_probability,
      accuracy = 0.1
    )
  ) |>
  select(
    -predicted_probability
  ) |>
  pivot_wider(
    names_from = preferred_model,
    values_from = predicted_percent
  )

print(
  phase4D_probability_wide,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------------------------
# 15F. SUMMARIZE AI-FORWARD PREFERENCE PROBABILITIES
# ------------------------------------------------------------------------------

phase4D_probability_grouped <- phase4D_predicted_probabilities |>
  mutate(
    preference_group = case_when(
      preferred_model ==
        "Human-led grading; AI support" ~
        "Human-led grading",
      
      preferred_model %in% c(
        "AI initial feedback; universal human review",
        "AI-led feedback; disputed-only human review"
      ) ~
        "AI-forward feedback model",
      
      preferred_model ==
        "No preference" ~
        "No preference",
      
      TRUE ~ NA_character_
    )
  ) |>
  group_by(
    ai_usefulness_score,
    ai_usefulness,
    observed_n,
    preference_group
  ) |>
  summarise(
    predicted_probability = sum(
      predicted_probability
    ),
    .groups = "drop"
  ) |>
  mutate(
    preference_group = factor(
      preference_group,
      levels = c(
        "Human-led grading",
        "AI-forward feedback model",
        "No preference"
      )
    ),
    
    predicted_percent = scales::percent(
      predicted_probability,
      accuracy = 0.1
    )
  ) |>
  arrange(
    ai_usefulness_score,
    preference_group
  )

print(
  phase4D_probability_grouped,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------------------------
# 16. CREATE THE FINAL ADJUSTED-PROBABILITY FIGURE
# ------------------------------------------------------------------------------

uf_blue <- "#0021A5"
uf_orange <- "#FA4616"

usefulness_axis_labels <- c(
  "Strongly\ndisagree\n(n = 7)",
  "Somewhat\ndisagree\n(n = 20)",
  "Neither agree\nnor disagree\n(n = 26)",
  "Somewhat\nagree\n(n = 55)",
  "Strongly\nagree\n(n = 61)"
)

preferred_model_plot_labels <- c(
  "Human-led grading; AI support" =
    "Human-Led Grading\nAI Used as Support",
  
  "AI initial feedback; universal human review" =
    "AI Initial Feedback\nUniversal Human Review",
  
  "AI-led feedback; disputed-only human review" =
    "AI-Led Feedback\nDisputed-Only Human Review",
  
  "No preference" =
    "No Preference"
)

phase4D_plot_data <- phase4D_predicted_probabilities |>
  mutate(
    preferred_model_plot = factor(
      as.character(preferred_model),
      levels = names(
        preferred_model_plot_labels
      ),
      labels = unname(
        preferred_model_plot_labels
      )
    ),
    
    percent_label = scales::percent(
      predicted_probability,
      accuracy = 0.1
    ),
    
    label_y = pmin(
      confidence_high + 0.055,
      1.035
    )
  )

figure_caption <- paste0(
  "Adjusted probabilities from the final multinomial logistic regression model, ",
  "with feedback experience held at its sample mean of 4.80. ",
  "Shaded bands represent 95% confidence intervals; observed sample sizes ",
  "are shown on the x-axis."
)

phase4D_probability_plot <- ggplot(
  phase4D_plot_data,
  aes(
    x = ai_usefulness_score,
    y = predicted_probability,
    group = 1
  )
) +
  geom_ribbon(
    aes(
      ymin = confidence_low,
      ymax = confidence_high
    ),
    fill = uf_blue,
    alpha = 0.15
  ) +
  geom_line(
    linewidth = 1.15,
    color = uf_blue
  ) +
  geom_point(
    size = 3.6,
    color = uf_orange
  ) +
  geom_text(
    aes(
      y = label_y,
      label = percent_label
    ),
    size = 3.3,
    fontface = "bold"
  ) +
  facet_wrap(
    vars(preferred_model_plot),
    ncol = 2
  ) +
  scale_x_continuous(
    breaks = 0:4,
    labels = usefulness_axis_labels
  ) +
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = seq(
      0,
      1,
      by = 0.2
    ),
    labels = scales::label_percent(
      accuracy = 1
    ),
    expand = expansion(
      mult = c(0, 0.01)
    )
  ) +
  labs(
    title = "Adjusted Probabilities of Preferred Feedback Models",
    subtitle = paste0(
      "Predicted preferences across levels of perceived usefulness ",
      "of AI-generated feedback"
    ),
    x = "Perceived Usefulness of AI-Generated Feedback",
    y = "Adjusted predicted probability",
    caption = stringr::str_wrap(
      figure_caption,
      width = 125
    )
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_blank(),
    
    strip.text = element_text(
      face = "bold",
      size = 11,
      lineheight = 1.05,
      margin = margin(
        t = 8,
        r = 5,
        b = 8,
        l = 5
      )
    ),
    
    strip.background = element_rect(
      fill = "grey95",
      color = NA
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 17
    ),
    
    plot.subtitle = element_text(
      size = 11.5,
      margin = margin(
        b = 18
      )
    ),
    
    axis.text.x = element_text(
      size = 8.5,
      lineheight = 0.95
    ),
    
    axis.title.x = element_text(
      margin = margin(
        t = 16
      )
    ),
    
    axis.title.y = element_text(
      margin = margin(
        r = 12
      )
    ),
    
    plot.caption = element_text(
      hjust = 0,
      size = 9,
      lineheight = 1.15,
      margin = margin(
        t = 18
      )
    ),
    
    panel.spacing = unit(
      1.3,
      "lines"
    ),
    
    plot.margin = margin(
      t = 20,
      r = 25,
      b = 25,
      l = 20
    )
  )

print(
  phase4D_probability_plot
)


# ------------------------------------------------------------------------------
# 17. EXPORT AND SAVE FINAL PHASE 4D RESULTS
# ------------------------------------------------------------------------------

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 17A. EXPORT DESCRIPTIVE AND DIAGNOSTIC TABLES
# ------------------------------------------------------------------------------

readr::write_csv(
  preferred_model_distribution,
  file.path(
    output_dir,
    "phase4D_final_preferred_model_distribution.csv"
  )
)

readr::write_csv(
  human_review_summary,
  file.path(
    output_dir,
    "phase4D_final_human_review_summary.csv"
  )
)

readr::write_csv(
  sparse_cell_summary,
  file.path(
    output_dir,
    "phase4D_final_sparse_cell_diagnostics.csv"
  )
)

readr::write_csv(
  model_fit_summary,
  file.path(
    output_dir,
    "phase4D_final_model_fit_summary.csv"
  )
)

readr::write_csv(
  primary_omnibus_tests,
  file.path(
    output_dir,
    "phase4D_final_primary_omnibus_tests.csv"
  )
)

readr::write_csv(
  reduced_model_comparisons,
  file.path(
    output_dir,
    "phase4D_final_reduced_model_comparisons.csv"
  )
)

readr::write_csv(
  reduced_model_fit,
  file.path(
    output_dir,
    "phase4D_final_reduced_model_fit.csv"
  )
)


# ------------------------------------------------------------------------------
# 17B. EXPORT FINAL-MODEL RESULTS
# ------------------------------------------------------------------------------

readr::write_csv(
  final_phase4D_model_selection,
  file.path(
    output_dir,
    "phase4D_final_model_selection.csv"
  )
)

readr::write_csv(
  final_coefficient_table,
  file.path(
    output_dir,
    "phase4D_final_coefficient_table.csv"
  )
)

readr::write_csv(
  final_coefficient_table |>
    filter(
      term != "(Intercept)"
    ),
  file.path(
    output_dir,
    "phase4D_final_report_ready_coefficients.csv"
  )
)

readr::write_csv(
  bias_reduced_diagnostics,
  file.path(
    output_dir,
    "phase4D_final_bias_reduced_diagnostics.csv"
  )
)

readr::write_csv(
  phase4D_estimator_comparison,
  file.path(
    output_dir,
    "phase4D_final_ml_vs_bias_reduced_comparison.csv"
  )
)


# ------------------------------------------------------------------------------
# 17C. EXPORT ADJUSTED-PROBABILITY TABLES
# ------------------------------------------------------------------------------

readr::write_csv(
  phase4D_predicted_probabilities,
  file.path(
    output_dir,
    "phase4D_final_adjusted_probabilities_long.csv"
  )
)

readr::write_csv(
  phase4D_probability_wide,
  file.path(
    output_dir,
    "phase4D_final_adjusted_probabilities_wide.csv"
  )
)

readr::write_csv(
  phase4D_probability_grouped,
  file.path(
    output_dir,
    "phase4D_final_grouped_ai_forward_probabilities.csv"
  )
)

if (exists("phase4D_probability_checks")) {
  readr::write_csv(
    phase4D_probability_checks,
    file.path(
      output_dir,
      "phase4D_final_probability_validation.csv"
    )
  )
}


# ------------------------------------------------------------------------------
# 17D. SAVE FINAL MODELS AND ANALYSIS CHECKPOINT
# ------------------------------------------------------------------------------

saveRDS(
  final_phase4D_model,
  file.path(
    output_dir,
    "final_phase4D_multinomial_model.rds"
  )
)

saveRDS(
  final_phase4D_bias_reduced_model,
  file.path(
    output_dir,
    "final_phase4D_bias_reduced_model.rds"
  )
)

saveRDS(
  analysis_complete,
  file.path(
    output_dir,
    "learner_analysis_phase4D_complete_cases.rds"
  )
)

saveRDS(
  list(
    final_model =
      final_phase4D_model,
    
    bias_reduced_model =
      final_phase4D_bias_reduced_model,
    
    final_model_selection =
      final_phase4D_model_selection,
    
    final_coefficient_table =
      final_coefficient_table,
    
    primary_omnibus_tests =
      primary_omnibus_tests,
    
    reduced_model_comparisons =
      reduced_model_comparisons,
    
    reduced_model_fit =
      reduced_model_fit,
    
    estimator_comparison =
      phase4D_estimator_comparison,
    
    predicted_probabilities =
      phase4D_predicted_probabilities,
    
    probability_wide =
      phase4D_probability_wide,
    
    grouped_probabilities =
      phase4D_probability_grouped,
    
    probability_checks =
      get0(
        "phase4D_probability_checks"
      ),
    
    preferred_model_distribution =
      preferred_model_distribution,
    
    human_review_summary =
      human_review_summary,
    
    sparse_cell_summary =
      sparse_cell_summary,
    
    model_fit_summary =
      model_fit_summary,
    
    feedback_experience_mean =
      feedback_experience_mean,
    
    ai_usefulness_levels =
      ai_usefulness_levels
  ),
  file.path(
    output_dir,
    "phase4D_final_analysis_checkpoint.rds"
  )
)


# ------------------------------------------------------------------------------
# 17E. EXPORT TEXT SUMMARIES
# ------------------------------------------------------------------------------

capture.output(
  summary(
    final_phase4D_model
  ),
  file = file.path(
    output_dir,
    "phase4D_final_model_summary.txt"
  )
)

capture.output(
  summary(
    final_phase4D_bias_reduced_model
  ),
  file = file.path(
    output_dir,
    "phase4D_final_bias_reduced_model_summary.txt"
  )
)

capture.output(
  sessionInfo(),
  file = file.path(
    output_dir,
    "phase4D_session_info.txt"
  )
)


# ------------------------------------------------------------------------------
# 17F. EXPORT FINAL FIGURE
# ------------------------------------------------------------------------------

ggsave(
  filename = file.path(
    output_dir,
    "phase4D_adjusted_preferred_model_probabilities.png"
  ),
  plot = phase4D_probability_plot,
  width = 13,
  height = 9.5,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(
    output_dir,
    "phase4D_adjusted_preferred_model_probabilities.pdf"
  ),
  plot = phase4D_probability_plot,
  width = 13,
  height = 9.5,
  device = cairo_pdf,
  bg = "white"
)


# ------------------------------------------------------------------------------
# 17G. CREATE OUTPUT MANIFEST
# ------------------------------------------------------------------------------

phase4D_output_manifest <- tibble(
  file = c(
    "final_phase4D_multinomial_model.rds",
    "final_phase4D_bias_reduced_model.rds",
    "learner_analysis_phase4D_complete_cases.rds",
    "phase4D_final_analysis_checkpoint.rds",
    "phase4D_final_model_summary.txt",
    "phase4D_final_bias_reduced_model_summary.txt",
    "phase4D_session_info.txt",
    "phase4D_final_preferred_model_distribution.csv",
    "phase4D_final_human_review_summary.csv",
    "phase4D_final_sparse_cell_diagnostics.csv",
    "phase4D_final_model_fit_summary.csv",
    "phase4D_final_primary_omnibus_tests.csv",
    "phase4D_final_reduced_model_comparisons.csv",
    "phase4D_final_reduced_model_fit.csv",
    "phase4D_final_model_selection.csv",
    "phase4D_final_coefficient_table.csv",
    "phase4D_final_report_ready_coefficients.csv",
    "phase4D_final_bias_reduced_diagnostics.csv",
    "phase4D_final_ml_vs_bias_reduced_comparison.csv",
    "phase4D_final_adjusted_probabilities_long.csv",
    "phase4D_final_adjusted_probabilities_wide.csv",
    "phase4D_final_grouped_ai_forward_probabilities.csv",
    "phase4D_adjusted_preferred_model_probabilities.png",
    "phase4D_adjusted_preferred_model_probabilities.pdf"
  ),
  
  description = c(
    "Final maximum-likelihood multinomial model",
    "Final mean-bias-reduced sensitivity model",
    "Complete-case Phase 4D analysis dataset",
    "Complete Phase 4D analysis checkpoint",
    "Printed summary of the final multinomial model",
    "Printed summary of the bias-reduced model",
    "R and package version information",
    "Observed preferred-model distribution",
    "Summary of human-review preferences",
    "Predictor-by-outcome sparse-cell diagnostics",
    "Fit statistics for candidate models",
    "Term-level likelihood-ratio tests",
    "Likelihood-ratio comparisons for reduced models",
    "Fit statistics for reduced models",
    "Final model-selection summary",
    "Complete final coefficient table",
    "Report-ready coefficient and relative-risk-ratio table",
    "Bias-reduced model numerical diagnostics",
    "Comparison of maximum-likelihood and bias-reduced estimates",
    "Long-format adjusted probability table",
    "Wide-format adjusted probability table",
    "Grouped AI-forward probability summary",
    "Report-ready PNG figure",
    "Vector-format PDF figure"
  )
)

readr::write_csv(
  phase4D_output_manifest,
  file.path(
    output_dir,
    "phase4D_output_manifest.csv"
  )
)


cat(
  "\n",
  "PHASE 4D COMPLETE\n",
  "Final model: preferred feedback model ~ perceived AI usefulness + ",
  "feedback experience\n",
  "Analytic sample: n = ",
  nrow(analysis_complete),
  "\n",
  "All final outputs saved to:\n",
  output_dir,
  "\n",
  sep = ""
)






























# ------------------------------------------------------------------------------
# 15. SAVE THE STEP-1 CHECKPOINT AND DIAGNOSTIC TABLES
# ------------------------------------------------------------------------------

readr::write_csv(
  missingness_summary,
  file.path(
    output_dir,
    "phase4D_step1_missingness_summary.csv"
  )
)

readr::write_csv(
  preferred_model_distribution,
  file.path(
    output_dir,
    "phase4D_step1_preferred_model_distribution.csv"
  )
)

readr::write_csv(
  human_review_summary,
  file.path(
    output_dir,
    "phase4D_step1_human_review_summary.csv"
  )
)

readr::write_csv(
  sparse_cell_summary,
  file.path(
    output_dir,
    "phase4D_step1_sparse_cell_summary.csv"
  )
)

readr::write_csv(
  model_fit_summary,
  file.path(
    output_dir,
    "phase4D_step1_model_fit_summary.csv"
  )
)

readr::write_csv(
  nested_model_comparisons,
  file.path(
    output_dir,
    "phase4D_step1_nested_model_comparisons.csv"
  )
)

readr::write_csv(
  primary_omnibus_tests,
  file.path(
    output_dir,
    "phase4D_step1_primary_omnibus_tests.csv"
  )
)

readr::write_csv(
  final_coefficient_table,
  file.path(
    output_dir,
    "phase4D_step1_full_candidate_coefficients.csv"
  )
)

readr::write_csv(
  bias_reduced_diagnostics,
  file.path(
    output_dir,
    "phase4D_step1_bias_reduced_diagnostics.csv"
  )
)

saveRDS(
  analysis_complete,
  file.path(
    output_dir,
    "learner_analysis_phase4D_complete_cases.rds"
  )
)

saveRDS(
  list(
    model_null = model_null,
    model_background = model_background,
    model_attitudes = model_attitudes,
    model_primary = model_primary,
    model_teaching_adjusted = model_teaching_adjusted,
    model_awareness_factor = model_awareness_factor,
    model_comfort_factor = model_comfort_factor,
    model_usefulness_factor = model_usefulness_factor,
    model_three_item_composite =
      model_three_item_composite,
    model_primary_bias_reduced =
      model_primary_bias_reduced,
    model_fit_summary = model_fit_summary,
    nested_model_comparisons =
      nested_model_comparisons,
    primary_omnibus_tests =
      primary_omnibus_tests,
    final_coefficient_table =
      final_coefficient_table,
    preferred_model_distribution =
      preferred_model_distribution,
    human_review_summary =
      human_review_summary,
    sparse_cell_summary =
      sparse_cell_summary,
    attitude_correlation =
      attitude_correlation,
    feedback_experience_mean =
      feedback_experience_mean,
    feedback_experience_3item_mean =
      feedback_experience_3item_mean,
    preferred_model_levels =
      preferred_model_short_levels,
    likert_levels = likert_levels
  ),
  file.path(
    output_dir,
    "phase4D_step1_model_checkpoint.rds"
  )
)

capture.output(
  summary(model_primary),
  file = file.path(
    output_dir,
    "phase4D_step1_full_candidate_model_summary.txt"
  )
)

capture.output(
  summary(model_primary_bias_reduced),
  file = file.path(
    output_dir,
    "phase4D_step1_bias_reduced_model_summary.txt"
  )
)

capture.output(
  sessionInfo(),
  file = file.path(
    output_dir,
    "phase4D_step1_session_info.txt"
  )
)

cat(
  "\nPhase 4D Step 1 completed.\n",
  "Review these printed outputs before freezing the final model:\n",
  "  1. Preferred-model distribution and human-review summary\n",
  "  2. Sparse and zero predictor-by-outcome cells\n",
  "  3. Model-fit summary and nested likelihood-ratio comparisons\n",
  "  4. Full-candidate term-level omnibus tests\n",
  "  5. Full-candidate multinomial coefficients and relative risk ratios\n",
  "  6. Maximum-likelihood convergence and Hessian diagnostics\n",
  "  7. Mean-bias-reduced multinomial sensitivity-model summary\n",
  "\n",
  "Do not select the final model from individual Wald p-values alone. ",
  "Prioritize the theoretical model sequence, likelihood-ratio tests, AIC/BIC, ",
  "coefficient stability, sparse-cell diagnostics, and agreement between the ",
  "maximum-likelihood and bias-reduced estimates.\n",
  sep = ""
)
