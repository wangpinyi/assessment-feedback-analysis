# ==============================================================================
# PHASE 4B: ORDINAL REGRESSION FOR AI COMFORT
# Learner Assessment and Feedback Survey
# ==============================================================================
# Purpose
#   1. Model learners' ordered responses to the AI-comfort item.
#   2. Estimate associations with prior AI experience, AI awareness, and the
#      Phase 4A feedback-experience composite.
#   3. evaluate the proportional-odds assumption and model convergence.
#   4. conduct sensitivity analyses for teaching experience, awareness coding,
#      the three-item feedback composite, and the link function.
#   5. export odds ratios, predicted probabilities, diagnostic tables, and
#      UF-themed report-ready figures.
#
# PRIMARY MODEL
#   ai_comfort ~ prior_ai_experience
#                + ai_awareness_score
#                + feedback_experience_half_point
#
# RATIONALE
#   - ai_comfort is a five-level ordered outcome, so a cumulative-link model is
#     preferable to ordinary least squares.
#   - prior AI experience is modeled as a categorical factor because its
#     association need not be linear across the four experience categories.
#   - AI awareness is represented by a 0-1-2 score because the three categories
#     form a clear progression. A factor-coded sensitivity model checks this
#     linear-step assumption.
#   - the feedback composite is centered and divided by 0.5. Its odds ratio is
#     therefore interpreted for a one-half-point increase on the original
#     1-to-5 composite scale.
#   - teaching experience is added only as a sensitivity adjustment to preserve
#     parsimony with n = 170.
#
# VARIABLES DELIBERATELY EXCLUDED FROM THE PRIMARY MODEL
#   - gender: nearly invariant in the current sample.
#   - age_group: overlaps strongly with teaching experience and includes one
#     "Prefer not to say" response.
#   - overall_feedback_quality: severe ceiling effect and conceptual overlap with
#     the Phase 4A feedback-experience composite.
#   - ai_as_useful_as_human: reserved as the Phase 4C outcome.
#   - preferred_feedback_model: reserved for Phase 4D.
#   - duration variables: deliberately excluded from the project analysis.
#
# IMPORTANT
#   - Run this script from the assessment-feedback-analysis RStudio project.
#   - The preferred input is the Phase 4A analysis-ready RDS. If that file is
#     absent, the script falls back to the Phase 3 RDS and reconstructs the same
#     composites.
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. START CLEAN AND LOAD PACKAGES
# ------------------------------------------------------------------------------

rm(list = ls())
graphics.off()
options(scipen = 999)

# Run once if needed:
# install.packages(c("tidyverse", "here", "ordinal", "emmeans"))

library(tidyverse)
library(here)
library(ordinal)
library(emmeans)

here::i_am(
  "scripts/04B_ordinal_regression_ai_comfort.R"
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

output_dir <- here(
  "output",
  "learner_ai_comfort_ordinal"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!dir.exists(output_dir)) {
  stop("The Phase 4B output directory could not be created.")
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

# This follows the most recent Phase 4A decision: exclude the specificity item
# from the three-item sensitivity composite.
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

  message(
    "Using the Phase 4A analysis-ready dataset:\n",
    phase4A_rds_path
  )

} else if (file.exists(phase3_rds_path)) {

  learner <- readRDS(phase3_rds_path)
  input_source <- "Phase 3 RDS with Phase 4A composites reconstructed"

  warning(
    "The Phase 4A analysis-ready RDS was not found. ",
    "The script will reconstruct the two Phase 4A composites from the Phase 3 RDS."
  )

  missing_feedback_items <- setdiff(
    feedback_items_4item,
    names(learner)
  )

  if (length(missing_feedback_items) > 0) {
    stop(
      "The Phase 3 dataset is missing required feedback items: ",
      paste(missing_feedback_items, collapse = ", ")
    )
  }

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

  learner <- bind_cols(
    learner,
    feedback_numeric |>
      set_names(numeric_item_names)
  )

  three_item_numeric_names <- paste0(
    feedback_items_3item,
    "_num"
  )

  learner <- learner |>
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

} else {

  stop(
    paste0(
      "Neither required input file was found.\n\n",
      "Expected Phase 4A file:\n", phase4A_rds_path, "\n\n",
      "Fallback Phase 3 file:\n", phase3_rds_path
    )
  )
}

cat("Input source:", input_source, "\n")
cat("Rows:", nrow(learner), "\n")
cat("Columns:", ncol(learner), "\n")


# ------------------------------------------------------------------------------
# 3. VALIDATE REQUIRED VARIABLES
# ------------------------------------------------------------------------------

required_variables <- c(
  "case_id",
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
    "Required Phase 4B variables are missing: ",
    paste(missing_variables, collapse = ", ")
  )
}

if (nrow(learner) != 170) {
  warning(
    "The current dataset contains ",
    nrow(learner),
    " rows rather than the expected 170. ",
    "Confirm that the intended processed file was loaded."
  )
}

if (anyDuplicated(learner$case_id) > 0) {
  stop("Duplicate case IDs were detected.")
}


# ------------------------------------------------------------------------------
# 4. RECODE THE OUTCOME AND PREDICTORS
# ------------------------------------------------------------------------------

ai_comfort_levels <- likert_levels

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

analysis_data <- learner |>
  mutate(
    # Ordered outcome: lower comfort to higher comfort
    ai_comfort = factor(
      as.character(ai_comfort),
      levels = ai_comfort_levels,
      ordered = TRUE
    ),

    # Categorical factor with "No experience" as the reference level
    prior_ai_experience = factor(
      as.character(prior_ai_experience),
      levels = prior_ai_experience_levels
    ),

    # Preserve a labeled factor for descriptive tables and sensitivity analysis
    ai_awareness_factor = factor(
      as.character(ai_awareness),
      levels = ai_awareness_levels
    ),

    # Primary awareness coding: 0 = none, 1 = somewhat, 2 = fully aware
    ai_awareness_score = case_when(
      as.character(ai_awareness) == "Not aware at all" ~ 0,
      as.character(ai_awareness) == "Somewhat aware" ~ 1,
      as.character(ai_awareness) == "Yes, fully aware" ~ 2,
      TRUE ~ NA_real_
    ),

    teaching_experience = factor(
      as.character(teaching_experience),
      levels = teaching_experience_levels
    )
  )

# Center the feedback variables and scale them so one model unit equals a
# 0.5-point increase on the original 1-to-5 composite scale.
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
# 5. CHECK LABELS, MISSINGNESS, AND SPARSE CELLS
# ------------------------------------------------------------------------------

unexpected_ai_comfort <- setdiff(
  unique(
    na.omit(
      as.character(learner$ai_comfort)
    )
  ),
  ai_comfort_levels
)

if (length(unexpected_ai_comfort) > 0) {
  stop(
    "Unexpected AI-comfort labels were detected: ",
    paste(unexpected_ai_comfort, collapse = ", ")
  )
}

unexpected_prior_experience <- setdiff(
  unique(
    na.omit(
      as.character(learner$prior_ai_experience)
    )
  ),
  prior_ai_experience_levels
)

if (length(unexpected_prior_experience) > 0) {
  stop(
    "Unexpected prior-AI-experience labels were detected: ",
    paste(unexpected_prior_experience, collapse = ", ")
  )
}

unexpected_awareness <- setdiff(
  unique(
    na.omit(
      as.character(learner$ai_awareness)
    )
  ),
  ai_awareness_levels
)

if (length(unexpected_awareness) > 0) {
  stop(
    "Unexpected AI-awareness labels were detected: ",
    paste(unexpected_awareness, collapse = ", ")
  )
}

model_variables <- c(
  "ai_comfort",
  "prior_ai_experience",
  "ai_awareness_score",
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
    ai_comfort,
    prior_ai_experience,
    ai_awareness_score,
    feedback_experience_half_point,
    feedback_experience_3item_half_point,
    teaching_experience
  )

cat(
  "Complete cases available for all Phase 4B models:",
  nrow(analysis_complete),
  "\n"
)

if (nrow(analysis_complete) < 150) {
  warning(
    "Fewer than 150 complete cases remain. ",
    "Review missingness before interpreting the models."
  )
}

outcome_distribution <- analysis_complete |>
  count(
    ai_comfort,
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

prior_experience_crosstab <- analysis_complete |>
  count(
    prior_ai_experience,
    ai_comfort,
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
    ai_comfort,
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
    ai_comfort,
    .drop = FALSE,
    name = "n"
  ) |>
  group_by(teaching_experience) |>
  mutate(
    row_percent = 100 * n / sum(n)
  ) |>
  ungroup()

sparse_cell_summary <- bind_rows(
  prior_experience_crosstab |>
    transmute(
      predictor = "Prior AI experience",
      predictor_level = as.character(prior_ai_experience),
      ai_comfort = as.character(ai_comfort),
      n
    ),

  awareness_crosstab |>
    transmute(
      predictor = "AI awareness",
      predictor_level = as.character(ai_awareness_factor),
      ai_comfort = as.character(ai_comfort),
      n
    ),

  teaching_experience_crosstab |>
    transmute(
      predictor = "Teaching experience",
      predictor_level = as.character(teaching_experience),
      ai_comfort = as.character(ai_comfort),
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


# ------------------------------------------------------------------------------
# 6. FIT CUMULATIVE-LINK MODELS
# ------------------------------------------------------------------------------

clm_control_settings <- ordinal::clm.control(
  maxIter = 100,
  gradTol = 0.000001
)

# Intercept-only benchmark
model_null <- ordinal::clm(
  ai_comfort ~ 1,
  data = analysis_complete,
  link = "logit",
  threshold = "flexible",
  control = clm_control_settings
)

# AI-background model
model_background <- ordinal::clm(
  ai_comfort ~
    prior_ai_experience +
    ai_awareness_score,
  data = analysis_complete,
  link = "logit",
  threshold = "flexible",
  control = clm_control_settings
)

# Primary Phase 4B model
model_primary <- ordinal::clm(
  ai_comfort ~
    prior_ai_experience +
    ai_awareness_score +
    feedback_experience_half_point,
  data = analysis_complete,
  link = "logit",
  threshold = "flexible",
  control = clm_control_settings
)

# Sensitivity model 1: add teaching experience
model_teaching_adjusted <- ordinal::clm(
  ai_comfort ~
    prior_ai_experience +
    ai_awareness_score +
    feedback_experience_half_point +
    teaching_experience,
  data = analysis_complete,
  link = "logit",
  threshold = "flexible",
  control = clm_control_settings
)

# Sensitivity model 2: treat awareness as a categorical factor
model_awareness_factor <- ordinal::clm(
  ai_comfort ~
    prior_ai_experience +
    ai_awareness_factor +
    feedback_experience_half_point,
  data = analysis_complete,
  link = "logit",
  threshold = "flexible",
  control = clm_control_settings
)

# Sensitivity model 3: replace the four-item composite with the three-item score
model_three_item_composite <- ordinal::clm(
  ai_comfort ~
    prior_ai_experience +
    ai_awareness_score +
    feedback_experience_3item_half_point,
  data = analysis_complete,
  link = "logit",
  threshold = "flexible",
  control = clm_control_settings
)

# Sensitivity model 4: probit rather than logit link
model_probit <- ordinal::clm(
  ai_comfort ~
    prior_ai_experience +
    ai_awareness_score +
    feedback_experience_half_point,
  data = analysis_complete,
  link = "probit",
  threshold = "flexible",
  control = clm_control_settings
)


# ------------------------------------------------------------------------------
# 7. VERIFY CONVERGENCE AND NUMERICAL STABILITY
# ------------------------------------------------------------------------------

# Extract convergence and numerical diagnostics robustly across ordinal versions.
# Some installed versions return model$convergence as a multi-element object
# rather than a single numeric code.
extract_clm_diagnostics <- function(model) {
  
  convergence_raw <- model[["convergence"]]
  convergence_flat <- unlist(
    convergence_raw,
    recursive = TRUE,
    use.names = TRUE
  )
  
  convergence_code <- NA_integer_
  
  # Prefer an explicitly named convergence-code element when available.
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
  
  # Otherwise use the first element that can be converted to a finite number.
  if (is.na(convergence_code)) {
    numeric_candidates <- suppressWarnings(
      as.numeric(convergence_flat)
    )
    
    numeric_candidates <- numeric_candidates[
      is.finite(numeric_candidates)
    ]
    
    if (length(numeric_candidates) > 0) {
      convergence_code <- as.integer(
        numeric_candidates[1]
      )
    }
  }
  
  # Obtain the maximum absolute gradient directly when possible.
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
  
  # Obtain the Hessian condition number, computing it if the stored value is
  # unavailable or nonnumeric.
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
      ": the convergence code could not be extracted. ",
      "Gradient and Hessian diagnostics will still be evaluated."
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
      signif(diagnostics$hessian_condition, 4),
      ". Sparse cells or weakly identified coefficients may be present."
    )
  }
  
  invisible(diagnostics)
}

models_to_check <- list(
  "Null model" = model_null,
  "Background model" = model_background,
  "Primary model" = model_primary,
  "Teaching-adjusted model" = model_teaching_adjusted,
  "Awareness-factor model" = model_awareness_factor,
  "Three-item-composite model" = model_three_item_composite,
  "Probit model" = model_probit
)

purrr::iwalk(
  models_to_check,
  ~ check_clm_model(.x, .y)
)

cat("\nPrimary model summary:\n")
print(summary(model_primary))


# ------------------------------------------------------------------------------
# 8. MODEL FIT AND NESTED MODEL COMPARISONS
# ------------------------------------------------------------------------------
extract_model_fit <- function(fitted_model, model_name) {
  
  model_log_likelihood <- logLik(fitted_model)
  diagnostics <- extract_clm_diagnostics(fitted_model)
  
  tibble(
    model = model_name,
    n = as.integer(fitted_model$nobs),
    number_of_parameters =
      attr(model_log_likelihood, "df"),
    log_likelihood =
      as.numeric(model_log_likelihood),
    AIC = AIC(fitted_model),
    BIC = BIC(fitted_model),
    convergence_code =
      diagnostics$convergence_code,
    convergence_message =
      diagnostics$convergence_message,
    maximum_absolute_gradient =
      diagnostics$maximum_gradient,
    hessian_condition_number =
      diagnostics$hessian_condition,
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

# The score-coded awareness model is nested within the factor-coded model:
# the factor model permits a non-equally-spaced awareness association.
# Null model versus AI-background model
comparison_null_background <- anova(
  model_null,
  model_background
)

# AI-background model versus primary model
comparison_background_primary <- anova(
  model_background,
  model_primary
)

# Primary model versus teaching-adjusted model
comparison_primary_teaching <- anova(
  model_primary,
  model_teaching_adjusted
)

# Score-coded versus factor-coded awareness
comparison_awareness_coding <- anova(
  model_primary,
  model_awareness_factor
)

# Omnibus tests for predictors in the primary model
primary_omnibus_tests <- anova(
  model_primary,
  type = "II"
)

cat("\nNull versus AI-background model:\n")
print(comparison_null_background)

cat("\nAI-background versus primary model:\n")
print(comparison_background_primary)

cat("\nPrimary versus teaching-adjusted model:\n")
print(comparison_primary_teaching)

cat("\nScore-coded versus factor-coded awareness:\n")
print(comparison_awareness_coding)

cat("\nPrimary-model omnibus tests:\n")
print(primary_omnibus_tests)



# ------------------------------------------------------------------------------
# 9. CREATE A CLEAN ODDS-RATIO TABLE
# ------------------------------------------------------------------------------

tidy_clm_location <- function(model, model_name) {
  
  coefficient_matrix <- coef(summary(model))
  
  coefficient_table <- as.data.frame(
    coefficient_matrix
  ) |>
    rownames_to_column(
      var = "parameter"
    ) |>
    as_tibble() |>
    filter(
      parameter %in% names(model$beta)
    ) |>
    transmute(
      model = model_name,
      parameter,
      estimate = Estimate,
      standard_error = `Std. Error`,
      z_value = `z value`,
      p_value = `Pr(>|z|)`,
      confidence_low_log_odds =
        estimate - qnorm(0.975) * standard_error,
      confidence_high_log_odds =
        estimate + qnorm(0.975) * standard_error,
      odds_ratio = exp(estimate),
      confidence_low_odds_ratio =
        exp(confidence_low_log_odds),
      confidence_high_odds_ratio =
        exp(confidence_high_log_odds)
    )
  
  coefficient_table
}

primary_odds_ratios <- tidy_clm_location(
  model_primary,
  "Primary cumulative-logit model"
) |>
  mutate(
    predictor = case_when(
      str_detect(
        parameter,
        fixed("prior_ai_experienceMinimal experience")
      ) ~ "Prior AI experience: Minimal vs. none",
      
      str_detect(
        parameter,
        fixed("prior_ai_experienceSome experience")
      ) ~ "Prior AI experience: Some vs. none",
      
      str_detect(
        parameter,
        fixed("prior_ai_experienceExtensive experience")
      ) ~ "Prior AI experience: Extensive vs. none",
      
      parameter == "ai_awareness_score" ~
        "AI awareness: one-category increase",
      
      parameter == "feedback_experience_half_point" ~
        "Feedback experience: 0.5-point increase",
      
      TRUE ~ parameter
    )
  ) |>
  select(
    model,
    predictor,
    parameter,
    estimate,
    standard_error,
    z_value,
    p_value,
    odds_ratio,
    confidence_low_odds_ratio,
    confidence_high_odds_ratio
  )

print(
  primary_odds_ratios,
  n = Inf,
  width = Inf
)

teaching_adjusted_odds_ratios <- tidy_clm_location(
  model_teaching_adjusted,
  "Teaching-adjusted sensitivity model"
)

three_item_odds_ratios <- tidy_clm_location(
  model_three_item_composite,
  "Three-item-composite sensitivity model"
)

awareness_factor_odds_ratios <- tidy_clm_location(
  model_awareness_factor,
  "Factor-coded-awareness sensitivity model"
)


# ------------------------------------------------------------------------------
# 10. TEST THE PROPORTIONAL-ODDS AND SCALE ASSUMPTIONS
# ------------------------------------------------------------------------------

run_diagnostic_safely <- function(test_expression) {
  
  tryCatch(
    test_expression,
    error = function(e) {
      structure(
        list(
          message = conditionMessage(e)
        ),
        class = "phase4B_diagnostic_error"
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
  
  if (inherits(result, "phase4B_diagnostic_error")) {
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

# Interpretation:
#   - For the logit model, a small p-value in nominal_test indicates evidence
#     that a predictor's effect differs across cumulative cut points.
#   - Do not automatically fit a partial proportional-odds model solely because
#     of one borderline p-value. First inspect convergence, sparse cells, and
#     whether the non-proportional pattern is substantively meaningful.


### Awareness Scale Model
model_awareness_scale <- ordinal::clm(
  ai_comfort ~
    prior_ai_experience +
    ai_awareness_score +
    feedback_experience_half_point,
  
  scale = ~ ai_awareness_score,
  
  data = analysis_complete,
  link = "logit",
  threshold = "flexible",
  control = clm_control_settings
)

summary(model_awareness_scale)


### Compare it with this primary model
comparison_awareness_scale <- anova(
  model_primary,
  model_awareness_scale
)

print(comparison_awareness_scale)


### Create a fit comparisons
awareness_scale_fit <- bind_rows(
  extract_model_fit(
    model_primary,
    "Primary proportional-odds model"
  ),
  
  extract_model_fit(
    model_awareness_scale,
    "Awareness-scale model"
  )
)

print(
  awareness_scale_fit,
  n = Inf,
  width = Inf
)


### Sensitivity check
model_awareness_scale_factor <- ordinal::clm(
  ai_comfort ~
    prior_ai_experience +
    ai_awareness_score +
    feedback_experience_half_point,
  
  scale = ~ ai_awareness_factor,
  
  data = analysis_complete,
  link = "logit",
  threshold = "flexible",
  control = clm_control_settings
)

summary(model_awareness_scale_factor)

comparison_scale_coding <- anova(
  model_awareness_scale,
  model_awareness_scale_factor
)

print(comparison_scale_coding)


###-----------------------
# Freeze the final model #
###----------------------
final_phase4B_model <- model_awareness_scale



# ------------------------------------------------------------------------------
# 11. ESTIMATED PROBABILITIES FOR REPORTING
# ------------------------------------------------------------------------------

# Probability of responding at least "Somewhat agree":
# this is the exceedance probability above the third category.
agreement_cut <- paste(
  ai_comfort_levels[3],
  ai_comfort_levels[4],
  sep = "|"
)



### First calculate adjusted probabilities for each awareness level while:
### holding back feedback experience as its sample mean, represent by 0 after centering
### averaging over prior AI experience according to its observed sample distribution

awareness_probability_final <- emmeans::emmeans(
  final_phase4B_model,
  specs = ~ ai_awareness_score,
  mode = "exc.prob",
  at = list(
    cut = agreement_cut,
    ai_awareness_score = 0:2,
    feedback_experience_half_point = 0
  ),
  weights = "proportional"
) |>
  as.data.frame() |>
  as_tibble() |>
  transmute(
    ai_awareness_score,
    
    ai_awareness = factor(
      ai_awareness_score,
      levels = 0:2,
      labels = c(
        "Not aware at all",
        "Somewhat aware",
        "Fully aware"
      )
    ),
    
    predicted_probability = exc.prob,
    standard_error = SE,
    confidence_low = asymp.LCL,
    confidence_high = asymp.UCL
  )

### Inspect it
print(
  awareness_probability_final,
  n = Inf,
  width = Inf
)


### Define the helper
standardize_probability_table <- function(emm_object) {
  
  output <- emm_object |>
    as.data.frame() |>
    as_tibble()
  
  probability_column <- intersect(
    c("exc.prob", "prob"),
    names(output)
  )[1]
  
  lower_column <- intersect(
    c("asymp.LCL", "lower.CL"),
    names(output)
  )[1]
  
  upper_column <- intersect(
    c("asymp.UCL", "upper.CL"),
    names(output)
  )[1]
  
  if (
    is.na(probability_column) ||
    is.na(lower_column) ||
    is.na(upper_column)
  ) {
    stop(
      "Expected probability or confidence-interval columns ",
      "were not found in the emmeans output."
    )
  }
  
  output |>
    rename(
      predicted_probability =
        all_of(probability_column),
      
      confidence_low =
        all_of(lower_column),
      
      confidence_high =
        all_of(upper_column)
    )
}



### Create the primary report figure
uf_blue <- "#0021A5"
uf_orange <- "#FA4616"

awareness_probability_plot_final <- ggplot(
  awareness_probability_final,
  aes(
    x = ai_awareness,
    y = predicted_probability
  )
) +
  geom_errorbar(
    aes(
      ymin = confidence_low,
      ymax = confidence_high
    ),
    width = 0.12,
    linewidth = 0.8,
    color = uf_blue
  ) +
  geom_point(
    size = 4,
    color = uf_orange
  ) +
  geom_text(
    aes(
      label = scales::percent(
        predicted_probability,
        accuracy = 1
      )
    ),
    vjust = -1.2,
    size = 4
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = scales::label_percent(
      accuracy = 1
    )
  ) +
  labs(
    title = stringr::str_wrap(
      "Adjusted Probability of Comfort With AI-Generated Feedback",
      width = 62
    ),
    
    subtitle = stringr::str_wrap(
      paste0(
        "Probability of selecting Somewhat agree or Strongly agree, ",
        "by awareness that AI was used"
      ),
      width = 90
    ),
    
    x = "Awareness That AI Was Used",
    y = "Predicted probability",
    
    caption = stringr::str_wrap(
      paste0(
        "Predictions are from the final cumulative-link location-scale model. ",
        "Feedback experience is held at its sample mean of 4.80, and prior AI ",
        "experience is averaged using its observed sample distribution. ",
        "Error bars represent 95% confidence intervals."
      ),
      width = 120
    )
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(
      size = 11
    ),
    
    plot.title.position = "plot",
    plot.caption.position = "plot",
    
    plot.title = element_text(
      face = "bold",
      size = 16,
      lineheight = 1.05,
      margin = margin(
        b = 6
      )
    ),
    
    plot.subtitle = element_text(
      size = 11,
      lineheight = 1.08,
      margin = margin(
        b = 10
      )
    ),
    
    plot.caption = element_text(
      size = 8.5,
      hjust = 0,
      lineheight = 1.08,
      margin = margin(
        t = 10
      )
    ),
    
    plot.margin = margin(
      t = 14,
      r = 24,
      b = 18,
      l = 24
    )
  )

print(
  awareness_probability_plot_final
)


### Examine feedback experience across awareness levels
feedback_values_for_prediction <- seq(
  from = 4.00,
  to = 5.00,
  by = 0.25
)

feedback_half_point_grid <- (
  feedback_values_for_prediction -
    feedback_experience_mean
) / 0.5

feedback_probability_final <- emmeans::emmeans(
  final_phase4B_model,
  specs =
    ~ feedback_experience_half_point |
    ai_awareness_score,
  mode = "exc.prob",
  at = list(
    cut = agreement_cut,
    ai_awareness_score = 0:2,
    feedback_experience_half_point =
      feedback_half_point_grid
  ),
  weights = "proportional"
) |>
  standardize_probability_table() |>
  mutate(
    ai_awareness = factor(
      ai_awareness_score,
      levels = 0:2,
      labels = c(
        "Not aware at all",
        "Somewhat aware",
        "Fully aware"
      )
    ),
    
    feedback_experience =
      feedback_experience_mean +
      0.5 * feedback_experience_half_point
  )



### Inspect the table
print(
  feedback_probability_final,
  n = Inf,
  width = Inf
)

stopifnot(
  nrow(feedback_probability_final) == 15,
  all(
    feedback_probability_final$
      predicted_probability >= 0
  ),
  all(
    feedback_probability_final$
      predicted_probability <= 1
  ),
  all(
    feedback_probability_final$
      confidence_low <=
      feedback_probability_final$
      predicted_probability
  ),
  all(
    feedback_probability_final$
      confidence_high >=
      feedback_probability_final$
      predicted_probability
  )
)




### Facet the figure
feedback_probability_plot_faceted <- ggplot(
  feedback_probability_final,
  aes(
    x = feedback_experience,
    y = predicted_probability
  )
) +
  geom_ribbon(
    aes(
      ymin = confidence_low,
      ymax = confidence_high
    ),
    fill = uf_orange,
    alpha = 0.15
  ) +
  geom_line(
    color = uf_blue,
    linewidth = 1.15
  ) +
  geom_point(
    color = uf_blue,
    size = 2.8
  ) +
  facet_wrap(
    ~ ai_awareness,
    nrow = 1
  ) +
  scale_x_continuous(
    breaks = feedback_values_for_prediction
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = scales::label_percent(
      accuracy = 1
    )
  ) +
  labs(
    title = "Adjusted Probability of AI Comfort by Feedback Experience",
    subtitle = paste0(
      "Predicted probability of selecting Somewhat agree or Strongly agree, ",
      "shown separately by awareness that AI was used"
    ),
    x = "Feedback-Experience Composite",
    y = "Predicted probability"
  ) +
  labs(
    caption = paste0(
      "Predictions are from the final cumulative-link location-scale model; ",
      "prior AI experience is averaged using its observed sample distribution. ",
      "Shaded bands represent 95% confidence intervals. The displayed range of ",
      "4.00–5.00 contains 165 of 170 respondents (97.1%); ",
      "121 respondents (71.2%) scored 5.00."
    )
  )+
  theme_minimal(
    base_size = 12
  ) +
  theme(
    panel.grid.minor = element_blank(),
    
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(
      size = 11
    )
  )

print(
  feedback_probability_plot_faceted
)


### Distribution figure
feedback_distribution_plot <- analysis_complete |>
  count(
    feedback_experience,
    name = "n"
  ) |>
  ggplot(
    aes(
      x = factor(feedback_experience),
      y = n
    )
  ) +
  geom_col(
    fill = "#0021A5",
    width = 0.75
  ) +
  geom_text(
    aes(label = n),
    vjust = -0.35,
    size = 3.8
  ) +
  labs(
    title = "Distribution of the Feedback-Experience Composite",
    subtitle = "The composite exhibited a pronounced ceiling effect",
    x = "Feedback-Experience Composite",
    y = "Number of respondents"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(
      face = "bold"
    )
  )

print(feedback_distribution_plot)


# ==============================================================================
# PHASE 4B FINAL EXPORT
# Learner Assessment and Feedback Survey
# ==============================================================================
# Run this block after completing the Phase 4B analyses in the same R session.
# It exports the preferred awareness-scale model, benchmark model, diagnostics,
# predicted probabilities, final figures, model-ready data, and reproducibility
# files.
# ==============================================================================

library(tidyverse)
library(here)
library(ordinal)

# ------------------------------------------------------------------------------
# 1. CONFIRM REQUIRED OBJECTS
# ------------------------------------------------------------------------------

required_objects <- c(
  "analysis_complete",
  "model_primary",
  "model_awareness_scale",
  "model_awareness_scale_factor",
  "final_phase4B_model",
  "model_fit_summary",
  "awareness_scale_fit",
  "comparison_awareness_scale",
  "comparison_scale_coding",
  "nominal_test_table",
  "scale_test_table",
  "primary_odds_ratios",
  "awareness_probability_final",
  "feedback_probability_final",
  "awareness_probability_plot_final",
  "feedback_probability_plot_faceted"
)

missing_objects <- required_objects[
  !vapply(
    required_objects,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(missing_objects) > 0) {
  stop(
    paste0(
      "The following required Phase 4B objects are missing:\n\n",
      paste(missing_objects, collapse = "\n"),
      "\n\nRerun the relevant analysis sections before exporting."
    )
  )
}

# ------------------------------------------------------------------------------
# 2. CREATE FINAL OUTPUT DIRECTORY
# ------------------------------------------------------------------------------

phase4B_final_dir <- here(
  "output",
  "learner_ai_comfort_ordinal",
  "final"
)

dir.create(
  phase4B_final_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!dir.exists(phase4B_final_dir)) {
  stop("The final Phase 4B output directory could not be created.")
}

cat(
  "Final Phase 4B files will be saved to:\n",
  phase4B_final_dir,
  "\n"
)

# ------------------------------------------------------------------------------
# 3. FINAL MODEL COEFFICIENT TABLE
# ------------------------------------------------------------------------------

final_coefficient_matrix <- coef(
  summary(final_phase4B_model)
)

final_model_coefficients <- as.data.frame(
  final_coefficient_matrix
) |>
  rownames_to_column(
    var = "parameter"
  ) |>
  as_tibble() |>
  rename(
    estimate = Estimate,
    standard_error = `Std. Error`,
    z_value = `z value`,
    p_value = `Pr(>|z|)`
  ) |>
  mutate(
    component = case_when(
      parameter %in% names(final_phase4B_model$beta) ~
        "Location effect",
      
      parameter %in% names(final_phase4B_model$zeta) ~
        "Log-scale effect",
      
      parameter %in% names(final_phase4B_model$alpha) ~
        "Threshold",
      
      TRUE ~
        "Other"
    ),
    
    confidence_low =
      estimate - qnorm(0.975) * standard_error,
    
    confidence_high =
      estimate + qnorm(0.975) * standard_error,
    
    significance = case_when(
      p_value < .001 ~ "***",
      p_value < .01 ~ "**",
      p_value < .05 ~ "*",
      p_value < .10 ~ ".",
      TRUE ~ ""
    )
  ) |>
  select(
    component,
    parameter,
    estimate,
    standard_error,
    z_value,
    p_value,
    confidence_low,
    confidence_high,
    significance
  )

print(
  final_model_coefficients,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------------------------
# 4. FINAL SCALE-EFFECT INTERPRETATION TABLE
# ------------------------------------------------------------------------------

awareness_scale_estimate <- unname(
  final_phase4B_model$zeta[
    "ai_awareness_score"
  ]
)

if (
  length(awareness_scale_estimate) != 1 ||
  !is.finite(awareness_scale_estimate)
) {
  stop(
    "The AI-awareness scale coefficient could not be extracted ",
    "from the final model."
  )
}

awareness_scale_interpretation <- tibble(
  contrast = c(
    "One-category increase in AI awareness",
    "Two-category increase: fully aware versus not aware"
  ),
  
  log_scale_change = c(
    awareness_scale_estimate,
    2 * awareness_scale_estimate
  ),
  
  latent_scale_multiplier = exp(
    log_scale_change
  ),
  
  percent_change_in_latent_scale =
    100 * (
      latent_scale_multiplier - 1
    )
)

print(
  awareness_scale_interpretation,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------------------------
# 5. FINAL MODEL-SELECTION TABLE
# ------------------------------------------------------------------------------

extract_final_fit <- function(fitted_model, model_name) {
  
  model_log_likelihood <- logLik(
    fitted_model
  )
  
  tibble(
    model = model_name,
    n = as.integer(
      fitted_model$nobs
    ),
    number_of_parameters =
      attr(
        model_log_likelihood,
        "df"
      ),
    log_likelihood =
      as.numeric(
        model_log_likelihood
      ),
    AIC = AIC(
      fitted_model
    ),
    BIC = BIC(
      fitted_model
    ),
    maximum_absolute_gradient =
      fitted_model$maxGradient,
    hessian_condition_number =
      fitted_model$cond.H,
    link =
      fitted_model$link
  )
}

final_model_selection <- bind_rows(
  extract_final_fit(
    model_primary,
    "Benchmark proportional-odds model"
  ),
  
  extract_final_fit(
    model_awareness_scale,
    "Final linear awareness-scale model"
  ),
  
  extract_final_fit(
    model_awareness_scale_factor,
    "Factor-coded awareness-scale sensitivity model"
  )
) |>
  mutate(
    delta_AIC =
      AIC - min(AIC),
    
    delta_BIC =
      BIC - min(BIC)
  )

print(
  final_model_selection,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------------------------
# 6. CONVERT MODEL-COMPARISON OBJECTS TO CLEAN TABLES
# ------------------------------------------------------------------------------

comparison_awareness_scale_table <- as.data.frame(
  comparison_awareness_scale
) |>
  rownames_to_column(
    var = "model"
  ) |>
  as_tibble()

comparison_scale_coding_table <- as.data.frame(
  comparison_scale_coding
) |>
  rownames_to_column(
    var = "model"
  ) |>
  as_tibble()

# ------------------------------------------------------------------------------
# 7. FEEDBACK-COMPOSITE DISTRIBUTION AND CEILING SUMMARY
# ------------------------------------------------------------------------------

feedback_distribution <- analysis_complete |>
  count(
    feedback_experience,
    name = "n"
  ) |>
  arrange(
    feedback_experience
  ) |>
  mutate(
    percent =
      100 * n / sum(n)
  )

feedback_ceiling_summary <- analysis_complete |>
  summarise(
    total_n = n(),
    
    mean =
      mean(
        feedback_experience
      ),
    
    standard_deviation =
      sd(
        feedback_experience
      ),
    
    minimum =
      min(
        feedback_experience
      ),
    
    percentile_05 =
      as.numeric(
        quantile(
          feedback_experience,
          .05
        )
      ),
    
    median =
      median(
        feedback_experience
      ),
    
    percentile_95 =
      as.numeric(
        quantile(
          feedback_experience,
          .95
        )
      ),
    
    maximum =
      max(
        feedback_experience
      ),
    
    maximum_score_n =
      sum(
        feedback_experience == 5
      ),
    
    maximum_score_percent =
      100 * mean(
        feedback_experience == 5
      ),
    
    displayed_range_n =
      sum(
        feedback_experience >= 4 &
          feedback_experience <= 5
      ),
    
    displayed_range_percent =
      100 * mean(
        feedback_experience >= 4 &
          feedback_experience <= 5
      )
  )

print(
  feedback_ceiling_summary,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------------------------
# 8. FINAL DECISION SUMMARY
# ------------------------------------------------------------------------------

phase4B_final_decision <- tibble(
  feature = c(
    "Final outcome",
    "Final model",
    "Location predictors",
    "Scale predictor",
    "Link function",
    "Threshold structure",
    "Final model AIC",
    "Final model BIC",
    "Final model log likelihood",
    "Awareness-scale versus benchmark LRT p-value",
    "Factor-coded versus linear scale LRT p-value",
    "Primary effect-size presentation",
    "Benchmark model role",
    "Displayed feedback-composite range",
    "Reason for restricted plotted range"
  ),
  
  result = c(
    "ai_comfort",
    "Cumulative-link location-scale model",
    paste(
      "Prior AI experience;",
      "AI awareness score;",
      "feedback-experience composite"
    ),
    "AI awareness score",
    "Logit",
    "Flexible",
    sprintf(
      "%.2f",
      AIC(
        final_phase4B_model
      )
    ),
    sprintf(
      "%.2f",
      BIC(
        final_phase4B_model
      )
    ),
    sprintf(
      "%.2f",
      as.numeric(
        logLik(
          final_phase4B_model
        )
      )
    ),
    "0.034",
    "0.947",
    "Adjusted predicted probabilities with 95% confidence intervals",
    "Sensitivity/benchmark proportional-odds odds ratios",
    "4.00 to 5.00",
    paste0(
      sprintf(
        "%.1f%%",
        feedback_ceiling_summary$
          displayed_range_percent
      ),
      " of respondents scored within the displayed range; ",
      sprintf(
        "%.1f%%",
        feedback_ceiling_summary$
          maximum_score_percent
      ),
      " scored at the maximum of 5.00."
    )
  )
)

print(
  phase4B_final_decision,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------------------------
# 9. EXPORT CSV TABLES
# ------------------------------------------------------------------------------

write_csv(
  final_model_coefficients,
  file.path(
    phase4B_final_dir,
    "phase4B_final_model_coefficients.csv"
  ),
  na = ""
)

write_csv(
  awareness_scale_interpretation,
  file.path(
    phase4B_final_dir,
    "phase4B_awareness_scale_interpretation.csv"
  ),
  na = ""
)

write_csv(
  final_model_selection,
  file.path(
    phase4B_final_dir,
    "phase4B_final_model_selection.csv"
  ),
  na = ""
)

write_csv(
  comparison_awareness_scale_table,
  file.path(
    phase4B_final_dir,
    "phase4B_comparison_awareness_scale.csv"
  ),
  na = ""
)

write_csv(
  comparison_scale_coding_table,
  file.path(
    phase4B_final_dir,
    "phase4B_comparison_scale_coding.csv"
  ),
  na = ""
)

write_csv(
  nominal_test_table,
  file.path(
    phase4B_final_dir,
    "phase4B_nominal_effects_diagnostic.csv"
  ),
  na = ""
)

write_csv(
  scale_test_table,
  file.path(
    phase4B_final_dir,
    "phase4B_scale_effects_diagnostic.csv"
  ),
  na = ""
)

write_csv(
  awareness_probability_final,
  file.path(
    phase4B_final_dir,
    "phase4B_adjusted_probability_by_awareness.csv"
  ),
  na = ""
)

write_csv(
  feedback_probability_final,
  file.path(
    phase4B_final_dir,
    "phase4B_adjusted_probability_by_feedback_and_awareness.csv"
  ),
  na = ""
)

write_csv(
  primary_odds_ratios,
  file.path(
    phase4B_final_dir,
    "phase4B_benchmark_proportional_odds_ratios.csv"
  ),
  na = ""
)

write_csv(
  feedback_distribution,
  file.path(
    phase4B_final_dir,
    "phase4B_feedback_composite_distribution.csv"
  ),
  na = ""
)

write_csv(
  feedback_ceiling_summary,
  file.path(
    phase4B_final_dir,
    "phase4B_feedback_composite_ceiling_summary.csv"
  ),
  na = ""
)

write_csv(
  phase4B_final_decision,
  file.path(
    phase4B_final_dir,
    "phase4B_final_analysis_decision.csv"
  ),
  na = ""
)

write_csv(
  analysis_complete,
  file.path(
    phase4B_final_dir,
    "learner_analysis_phase4B_final_model_ready.csv"
  ),
  na = ""
)

# ------------------------------------------------------------------------------
# 10. EXPORT FINAL FIGURES
# ------------------------------------------------------------------------------

ggsave(
  filename = file.path(
    phase4B_final_dir,
    "phase4B_adjusted_probability_by_awareness.png"
  ),
  plot = awareness_probability_plot_final,
  width = 11,
  height = 7.25,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(
    phase4B_final_dir,
    "phase4B_adjusted_probability_by_awareness.pdf"
  ),
  plot = awareness_probability_plot_final,
  width = 11,
  height = 7.25,
  units = "in",
  bg = "white"
)

ggsave(
  filename = file.path(
    phase4B_final_dir,
    "phase4B_adjusted_probability_by_feedback_faceted.png"
  ),
  plot = feedback_probability_plot_faceted,
  width = 12,
  height = 6.5,
  dpi = 300
)

ggsave(
  filename = file.path(
    phase4B_final_dir,
    "phase4B_adjusted_probability_by_feedback_faceted.pdf"
  ),
  plot = feedback_probability_plot_faceted,
  width = 12,
  height = 6.5
)

if (
  exists(
    "feedback_probability_plot_final",
    inherits = TRUE
  )
) {
  ggsave(
    filename = file.path(
      phase4B_final_dir,
      "phase4B_adjusted_probability_by_feedback_combined.png"
    ),
    plot = feedback_probability_plot_final,
    width = 10,
    height = 6.5,
    dpi = 300
  )
  
  ggsave(
    filename = file.path(
      phase4B_final_dir,
      "phase4B_adjusted_probability_by_feedback_combined.pdf"
    ),
    plot = feedback_probability_plot_final,
    width = 10,
    height = 6.5
  )
}

if (
  exists(
    "feedback_distribution_plot",
    inherits = TRUE
  )
) {
  ggsave(
    filename = file.path(
      phase4B_final_dir,
      "phase4B_feedback_composite_distribution.png"
    ),
    plot = feedback_distribution_plot,
    width = 9,
    height = 6,
    dpi = 300
  )
  
  ggsave(
    filename = file.path(
      phase4B_final_dir,
      "phase4B_feedback_composite_distribution.pdf"
    ),
    plot = feedback_distribution_plot,
    width = 9,
    height = 6
  )
}

# ------------------------------------------------------------------------------
# 11. SAVE R OBJECTS AND MODEL-READY DATA
# ------------------------------------------------------------------------------

saveRDS(
  final_phase4B_model,
  file.path(
    phase4B_final_dir,
    "phase4B_final_awareness_scale_model.rds"
  )
)

saveRDS(
  analysis_complete,
  file.path(
    phase4B_final_dir,
    "learner_analysis_phase4B_final_model_ready.rds"
  )
)

saveRDS(
  list(
    final_model =
      final_phase4B_model,
    
    benchmark_model =
      model_primary,
    
    factor_scale_sensitivity_model =
      model_awareness_scale_factor,
    
    final_model_coefficients =
      final_model_coefficients,
    
    final_model_selection =
      final_model_selection,
    
    awareness_scale_interpretation =
      awareness_scale_interpretation,
    
    comparison_awareness_scale =
      comparison_awareness_scale,
    
    comparison_scale_coding =
      comparison_scale_coding,
    
    nominal_effects_diagnostic =
      nominal_test_table,
    
    scale_effects_diagnostic =
      scale_test_table,
    
    adjusted_probability_by_awareness =
      awareness_probability_final,
    
    adjusted_probability_by_feedback_and_awareness =
      feedback_probability_final,
    
    benchmark_odds_ratios =
      primary_odds_ratios,
    
    feedback_distribution =
      feedback_distribution,
    
    feedback_ceiling_summary =
      feedback_ceiling_summary,
    
    final_analysis_decision =
      phase4B_final_decision
  ),
  file.path(
    phase4B_final_dir,
    "phase4B_final_analysis_objects.rds"
  )
)

# ------------------------------------------------------------------------------
# 12. SAVE TEXT SUMMARIES AND SESSION INFORMATION
# ------------------------------------------------------------------------------

writeLines(
  capture.output(
    summary(
      final_phase4B_model
    )
  ),
  file.path(
    phase4B_final_dir,
    "phase4B_final_model_summary.txt"
  )
)

writeLines(
  capture.output(
    summary(
      model_primary
    )
  ),
  file.path(
    phase4B_final_dir,
    "phase4B_benchmark_model_summary.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    phase4B_final_dir,
    "phase4B_session_info.txt"
  )
)

# ------------------------------------------------------------------------------
# 13. FINAL EXPORT VALIDATION
# ------------------------------------------------------------------------------

required_final_files <- c(
  "phase4B_final_model_coefficients.csv",
  "phase4B_awareness_scale_interpretation.csv",
  "phase4B_final_model_selection.csv",
  "phase4B_comparison_awareness_scale.csv",
  "phase4B_comparison_scale_coding.csv",
  "phase4B_nominal_effects_diagnostic.csv",
  "phase4B_scale_effects_diagnostic.csv",
  "phase4B_adjusted_probability_by_awareness.csv",
  "phase4B_adjusted_probability_by_feedback_and_awareness.csv",
  "phase4B_benchmark_proportional_odds_ratios.csv",
  "phase4B_feedback_composite_distribution.csv",
  "phase4B_feedback_composite_ceiling_summary.csv",
  "phase4B_final_analysis_decision.csv",
  "learner_analysis_phase4B_final_model_ready.csv",
  "learner_analysis_phase4B_final_model_ready.rds",
  "phase4B_adjusted_probability_by_awareness.png",
  "phase4B_adjusted_probability_by_awareness.pdf",
  "phase4B_adjusted_probability_by_feedback_faceted.png",
  "phase4B_adjusted_probability_by_feedback_faceted.pdf",
  "phase4B_final_awareness_scale_model.rds",
  "phase4B_final_analysis_objects.rds",
  "phase4B_final_model_summary.txt",
  "phase4B_benchmark_model_summary.txt",
  "phase4B_session_info.txt"
)

phase4B_export_check <- tibble(
  file =
    required_final_files,
  
  exists =
    file.exists(
      file.path(
        phase4B_final_dir,
        required_final_files
      )
    )
)

print(
  phase4B_export_check,
  n = Inf
)

stopifnot(
  all(
    phase4B_export_check$exists
  )
)

message(
  paste0(
    "\nPhase 4B final export completed successfully.\n",
    "Files were saved to:\n",
    phase4B_final_dir,
    "\n"
  )
)


