# ==============================================================================
# Learner Experience Survey: Phase 2
# Audit response categories, review response duration, and recode variables
# ==============================================================================

rm(list = ls())
graphics.off()
options(scipen = 999)

library(tidyverse)
library(here)

here::i_am("scripts/02_audit_recode_learner.R")

learner <- readRDS(
  here(
    "data_processed",
    "learner_analysis_phase1.rds"
  )
)

cat("Rows:", nrow(learner), "\n")
cat("Columns:", ncol(learner), "\n")

## categorical lables
categorical_variables <- learner |>
  select(
    teaching_experience,
    gender,
    age_group,
    first_submission_mastery,
    rubric_clear_aligned,
    feedback_specific_relevant,
    feedback_helped_improve,
    feedback_timely,
    overall_feedback_quality,
    prior_ai_experience,
    ai_awareness,
    ai_comfort,
    ai_as_useful_as_human,
    preferred_feedback_model
  )

category_audit <- purrr::imap_dfr(
  categorical_variables,
  function(x, variable_name) {
    tibble(response = x) |>
      count(response, name = "n", .drop = FALSE) |>
      mutate(
        variable = variable_name,
        percent = round(100 * n / sum(n), 1),
        .before = 1
      )
  }
)

print(category_audit, n = Inf)

readr::write_csv(
  category_audit,
  here(
    "output",
    "quality_checks",
    "learner_category_audit.csv"
  ),
  na = ""
)

## Quality flags
learner_qc <- learner |>
  mutate(
    duration_minutes = duration_seconds / 60,
    
    duration_flag = case_when(
      duration_seconds < 60 ~ "Under 1 minute",
      duration_seconds >= 60 &
        duration_seconds < 120 ~ "1 to under 2 minutes",
      duration_seconds > 3600 ~ "Over 1 hour",
      TRUE ~ "Not flagged"
    )
  )

## Count the flags
duration_summary <- learner_qc |>
  count(duration_flag, name = "n") |>
  mutate(
    percent = round(100 * n / sum(n), 1)
  )

print(duration_summary)

## Reviewed the flagged cases
flagged_duration_cases <- learner_qc |>
  filter(duration_flag != "Not flagged") |>
  select(
    case_id,
    duration_seconds,
    duration_minutes,
    duration_flag,
    first_submission_mastery,
    overall_feedback_quality,
    ai_comfort,
    most_valuable_text,
    improvement_text,
    additional_ai_comments
  ) |>
  arrange(duration_seconds)

print(flagged_duration_cases, n = Inf)

## SAVE
readr::write_csv(
  flagged_duration_cases,
  here(
    "output",
    "quality_checks",
    "learner_flagged_duration_cases.csv"
  ),
  na = ""
)

## Recode
likert_levels <- c(
  "Strongly disagree",
  "Somewhat disagree",
  "Neither agree nor disagree",
  "Somewhat agree",
  "Strongly agree"
)

feedback_likert_variables <- c(
  "rubric_clear_aligned",
  "feedback_specific_relevant",
  "feedback_helped_improve",
  "feedback_timely"
)

ai_likert_variables <- c(
  "ai_comfort",
  "ai_as_useful_as_human"
)

learner_recoded <- learner_qc |>
  mutate(
    across(
      all_of(
        c(
          feedback_likert_variables,
          ai_likert_variables
        )
      ),
      ~ factor(
        .x,
        levels = likert_levels,
        ordered = TRUE
      )
    )
  )

## Recode overall feedback quality
quality_levels <- c(
  "Poor",
  "Fair",
  "Good",
  "Excellent"
)

learner_recoded <- learner_recoded |>
  mutate(
    overall_feedback_quality = factor(
      overall_feedback_quality,
      levels = quality_levels,
      ordered = TRUE
    )
  )

## Recode mastery as a nominal factor
learner_recoded <- learner_recoded |>
  mutate(
    first_submission_mastery = factor(
      first_submission_mastery,
      levels = c(
        "No, I didn't.",
        "Unsure",
        "Yes, I did."
      )
    )
  )

## Checking
recode_check <- learner_recoded |>
  summarise(
    across(
      all_of(
        c(
          feedback_likert_variables,
          ai_likert_variables,
          "overall_feedback_quality",
          "first_submission_mastery"
        )
      ),
      ~ sum(is.na(.x))
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "missing_after_recode"
  )

print(recode_check)

## create a review dataset
open_text_review <- learner_recoded |>
  select(
    case_id,
    most_valuable_text,
    improvement_text,
    additional_ai_comments
  )

 ## Save the Phase 2 dataset

saveRDS(
  learner_recoded,
  here(
    "data_processed",
    "learner_analysis_phase2_recoded.rds"
  )
)

readr::write_csv(
  learner_recoded,
  here(
    "data_processed",
    "learner_analysis_phase2_recoded.csv"
  ),
  na = ""
)

cat("Phase 2 recoded dataset saved successfully.\n")