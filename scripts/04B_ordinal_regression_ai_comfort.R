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
  
  # Extract everything before constructing the tibble.
  # This prevents tibble column names from masking the model object.
  model_log_likelihood <- logLik(fitted_model)
  diagnostics <- extract_clm_diagnostics(fitted_model)
  model_n <- extract_clm_nobs(fitted_model)
  
  model_aic <- AIC(fitted_model)
  model_bic <- BIC(fitted_model)
  model_link <- fitted_model$link
  
  tibble(
    model = model_name,
    
    n = model_n,
    
    number_of_parameters =
      attr(model_log_likelihood, "df"),
    
    log_likelihood =
      as.numeric(model_log_likelihood),
    
    AIC = model_aic,
    
    BIC = model_bic,
    
    convergence_code =
      diagnostics$convergence_code,
    
    convergence_message =
      diagnostics$convergence_message,
    
    maximum_absolute_gradient =
      diagnostics$maximum_gradient,
    
    hessian_condition_number =
      diagnostics$hessian_condition,
    
    link = model_link
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
comparison_awareness_coding <- anova(
  model_primary,
  model_awareness_factor
)

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

standardize_probability_table <- function(emm_object) {

  output <- as.data.frame(
    emm_object
  ) |>
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
    is.na(probability_column) |
      is.na(lower_column) |
      is.na(upper_column)
  ) {
    stop(
      "The emmeans probability output did not contain the expected ",
      "probability and confidence-limit columns."
    )
  }

  names(output)[names(output) == probability_column] <-
    "predicted_probability"

  names(output)[names(output) == lower_column] <-
    "confidence_low"

  names(output)[names(output) == upper_column] <-
    "confidence_high"

  output
}

# 11A. Probability distribution across all five response categories by prior
#      AI experience, holding numeric predictors at their sample means.
prior_full_probabilities <- emmeans::emmeans(
  model_primary,
  ~ ai_comfort | prior_ai_experience,
  mode = "prob"
) |>
  as.data.frame() |>
  as_tibble()

# 11B. Probability of at least "Somewhat agree" by prior AI experience.
prior_agreement_probability <- emmeans::emmeans(
  model_primary,
  ~ prior_ai_experience,
  mode = "exc.prob",
  at = list(
    cut = agreement_cut
  )
) |>
  standardize_probability_table() |>
  mutate(
    prior_ai_experience = factor(
      prior_ai_experience,
      levels = prior_ai_experience_levels
    )
  )

# 11C. Probability of at least "Somewhat agree" across awareness levels.
awareness_agreement_probability <- emmeans::emmeans(
  model_primary,
  ~ ai_awareness_score,
  mode = "exc.prob",
  at = list(
    cut = agreement_cut,
    ai_awareness_score = 0:2
  )
) |>
  standardize_probability_table() |>
  mutate(
    ai_awareness = factor(
      ai_awareness_score,
      levels = 0:2,
      labels = ai_awareness_levels
    )
  )

# 11D. Probability of at least "Somewhat agree" across the observed high-density
#      range of the four-item feedback composite.
feedback_values_for_prediction <- seq(
  from = 4.00,
  to = 5.00,
  by = 0.25
)

feedback_half_point_grid <- (
  feedback_values_for_prediction -
    feedback_experience_mean
) / 0.5

feedback_agreement_probability <- emmeans::emmeans(
  model_primary,
  ~ feedback_experience_half_point,
  mode = "exc.prob",
  at = list(
    cut = agreement_cut,
    feedback_experience_half_point =
      feedback_half_point_grid
  )
) |>
  standardize_probability_table() |>
  mutate(
    feedback_experience =
      feedback_experience_mean +
      0.5 * feedback_experience_half_point
  )

print(
  prior_agreement_probability,
  n = Inf,
  width = Inf
)

print(
  awareness_agreement_probability,
  n = Inf,
  width = Inf
)

print(
  feedback_agreement_probability,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 12. CREATE UF-THEMED REPORT-READY FIGURES
# ------------------------------------------------------------------------------

uf_blue <- "#0021A5"
uf_orange <- "#FA4616"

prior_probability_plot <- ggplot(
  prior_agreement_probability,
  aes(
    x = prior_ai_experience,
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
    size = 3.8,
    color = uf_orange
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = scales::label_percent(
      accuracy = 1
    )
  ) +
  labs(
    title = "Predicted Probability of Comfort With AI Feedback",
    subtitle = paste0(
      "Probability of selecting Somewhat agree or Strongly agree, ",
      "by prior AI experience"
    ),
    x = "Prior AI experience",
    y = "Predicted probability"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    ),
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      size = 11
    )
  )

awareness_probability_plot <- ggplot(
  awareness_agreement_probability,
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
    size = 3.8,
    color = uf_orange
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = scales::label_percent(
      accuracy = 1
    )
  ) +
  labs(
    title = "Predicted Probability of Comfort With AI Feedback",
    subtitle = paste0(
      "Probability of selecting Somewhat agree or Strongly agree, ",
      "by awareness that AI was used"
    ),
    x = "AI awareness",
    y = "Predicted probability"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(
      angle = 15,
      hjust = 1
    ),
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      size = 11
    )
  )

feedback_probability_plot <- ggplot(
  feedback_agreement_probability,
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
    alpha = 0.18,
    fill = uf_orange
  ) +
  geom_line(
    linewidth = 1.1,
    color = uf_blue
  ) +
  geom_point(
    size = 2.7,
    color = uf_blue
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
    title = "Predicted Probability of Comfort With AI Feedback",
    subtitle = paste0(
      "Probability of selecting Somewhat agree or Strongly agree, ",
      "across the feedback-experience composite"
    ),
    x = "Feedback-experience composite",
    y = "Predicted probability"
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      size = 11
    )
  )

print(prior_probability_plot)
print(awareness_probability_plot)
print(feedback_probability_plot)


# ------------------------------------------------------------------------------
# 13. EXPORT TABLES, FIGURES, MODELS, AND SESSION INFORMATION
# ------------------------------------------------------------------------------

write_csv(
  missingness_summary,
  file.path(
    output_dir,
    "phase4B_missingness_summary.csv"
  )
)

write_csv(
  outcome_distribution,
  file.path(
    output_dir,
    "phase4B_ai_comfort_distribution.csv"
  )
)

write_csv(
  prior_experience_crosstab,
  file.path(
    output_dir,
    "phase4B_ai_comfort_by_prior_experience.csv"
  )
)

write_csv(
  awareness_crosstab,
  file.path(
    output_dir,
    "phase4B_ai_comfort_by_awareness.csv"
  )
)

write_csv(
  teaching_experience_crosstab,
  file.path(
    output_dir,
    "phase4B_ai_comfort_by_teaching_experience.csv"
  )
)

write_csv(
  sparse_cell_summary,
  file.path(
    output_dir,
    "phase4B_sparse_cell_summary.csv"
  )
)

write_csv(
  model_fit_summary,
  file.path(
    output_dir,
    "phase4B_model_fit_summary.csv"
  )
)

write_csv(
  primary_odds_ratios,
  file.path(
    output_dir,
    "phase4B_primary_odds_ratios.csv"
  )
)

write_csv(
  teaching_adjusted_odds_ratios,
  file.path(
    output_dir,
    "phase4B_teaching_adjusted_odds_ratios.csv"
  )
)

write_csv(
  awareness_factor_odds_ratios,
  file.path(
    output_dir,
    "phase4B_awareness_factor_odds_ratios.csv"
  )
)

write_csv(
  three_item_odds_ratios,
  file.path(
    output_dir,
    "phase4B_three_item_composite_odds_ratios.csv"
  )
)

write_csv(
  nominal_test_table,
  file.path(
    output_dir,
    "phase4B_nominal_test.csv"
  )
)

write_csv(
  scale_test_table,
  file.path(
    output_dir,
    "phase4B_scale_test.csv"
  )
)

write_csv(
  as.data.frame(comparison_null_background) |>
    rownames_to_column("model") |>
    as_tibble(),
  file.path(
    output_dir,
    "phase4B_comparison_null_background.csv"
  )
)

write_csv(
  as.data.frame(comparison_background_primary) |>
    rownames_to_column("model") |>
    as_tibble(),
  file.path(
    output_dir,
    "phase4B_comparison_background_primary.csv"
  )
)

write_csv(
  as.data.frame(comparison_primary_teaching) |>
    rownames_to_column("model") |>
    as_tibble(),
  file.path(
    output_dir,
    "phase4B_comparison_primary_teaching.csv"
  )
)

write_csv(
  as.data.frame(comparison_awareness_coding) |>
    rownames_to_column("model") |>
    as_tibble(),
  file.path(
    output_dir,
    "phase4B_comparison_awareness_coding.csv"
  )
)

write_csv(
  as.data.frame(primary_omnibus_tests) |>
    rownames_to_column("term") |>
    as_tibble(),
  file.path(
    output_dir,
    "phase4B_primary_omnibus_tests.csv"
  )
)

write_csv(
  prior_full_probabilities,
  file.path(
    output_dir,
    "phase4B_full_probabilities_by_prior_experience.csv"
  )
)

write_csv(
  prior_agreement_probability,
  file.path(
    output_dir,
    "phase4B_agreement_probability_by_prior_experience.csv"
  )
)

write_csv(
  awareness_agreement_probability,
  file.path(
    output_dir,
    "phase4B_agreement_probability_by_awareness.csv"
  )
)

write_csv(
  feedback_agreement_probability,
  file.path(
    output_dir,
    "phase4B_agreement_probability_by_feedback_experience.csv"
  )
)

ggsave(
  filename = file.path(
    output_dir,
    "phase4B_predicted_agreement_prior_experience.png"
  ),
  plot = prior_probability_plot,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(
    output_dir,
    "phase4B_predicted_agreement_awareness.png"
  ),
  plot = awareness_probability_plot,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(
    output_dir,
    "phase4B_predicted_agreement_feedback_experience.png"
  ),
  plot = feedback_probability_plot,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(
    output_dir,
    "phase4B_predicted_agreement_prior_experience.pdf"
  ),
  plot = prior_probability_plot,
  width = 9,
  height = 6
)

ggsave(
  filename = file.path(
    output_dir,
    "phase4B_predicted_agreement_awareness.pdf"
  ),
  plot = awareness_probability_plot,
  width = 9,
  height = 6
)

ggsave(
  filename = file.path(
    output_dir,
    "phase4B_predicted_agreement_feedback_experience.pdf"
  ),
  plot = feedback_probability_plot,
  width = 9,
  height = 6
)

saveRDS(
  analysis_complete,
  file.path(
    output_dir,
    "learner_analysis_phase4B_model_ready.rds"
  )
)

write_csv(
  analysis_complete,
  file.path(
    output_dir,
    "learner_analysis_phase4B_model_ready.csv"
  ),
  na = ""
)

saveRDS(
  list(
    input_source = input_source,
    feedback_experience_mean =
      feedback_experience_mean,
    feedback_experience_3item_mean =
      feedback_experience_3item_mean,
    model_null = model_null,
    model_background = model_background,
    model_primary = model_primary,
    model_teaching_adjusted =
      model_teaching_adjusted,
    model_awareness_factor =
      model_awareness_factor,
    model_three_item_composite =
      model_three_item_composite,
    model_probit = model_probit,
    nominal_test = nominal_test_result,
    scale_test = scale_test_result,
    primary_odds_ratios =
      primary_odds_ratios,
    model_fit_summary =
      model_fit_summary
  ),
  file.path(
    output_dir,
    "phase4B_analysis_objects.rds"
  )
)

writeLines(
  capture.output(
    summary(model_primary)
  ),
  file.path(
    output_dir,
    "phase4B_primary_model_summary.txt"
  )
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    output_dir,
    "phase4B_session_info.txt"
  )
)


# ------------------------------------------------------------------------------
# 14. FINAL VALIDATION
# ------------------------------------------------------------------------------

required_exports <- c(
  "phase4B_ai_comfort_distribution.csv",
  "phase4B_model_fit_summary.csv",
  "phase4B_primary_odds_ratios.csv",
  "phase4B_nominal_test.csv",
  "phase4B_scale_test.csv",
  "phase4B_agreement_probability_by_prior_experience.csv",
  "phase4B_agreement_probability_by_awareness.csv",
  "phase4B_agreement_probability_by_feedback_experience.csv",
  "phase4B_predicted_agreement_prior_experience.png",
  "phase4B_predicted_agreement_awareness.png",
  "phase4B_predicted_agreement_feedback_experience.png",
  "learner_analysis_phase4B_model_ready.rds",
  "phase4B_analysis_objects.rds",
  "phase4B_primary_model_summary.txt",
  "phase4B_session_info.txt"
)

export_check <- tibble(
  file = required_exports,
  exists = file.exists(
    file.path(
      output_dir,
      required_exports
    )
  )
)

print(
  export_check,
  n = Inf
)

stopifnot(
  all(export_check$exists)
)

message(
  "Phase 4B completed successfully. Outputs were saved to:\n",
  output_dir
)
