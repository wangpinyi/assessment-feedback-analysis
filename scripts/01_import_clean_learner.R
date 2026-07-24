# ==============================================================================
# Learner Experience Survey: Phase 1
# Set up project, import labeled Qualtrics workbook, identify valid cases,
# remove identifying metadata, rename variables, verify, and save analysis files.
# ==============================================================================

# Run this installation line ONCE, then comment it out:
# install.packages(c("tidyverse", "readxl", "janitor", "here", "skimr"))

# 1. Start each session with a clean environment -------------------------------
rm(list = ls())
graphics.off()
options(scipen = 999)

# 2. Load packages --------------------------------------------------------------
library(tidyverse)
library(readxl)
library(janitor)
library(here)
library(skimr)

# 3. Confirm that R is working inside the RStudio Project -----------------------
here::i_am("scripts/01_import_clean_learner.R")
cat("Project root:", here(), "\n")

# 4. Define paths ---------------------------------------------------------------
learner_file <- here(
  "data_raw",
  "Labeled_Learner Experience Survey_ Assessment & Feedback Process_July 23, 2026_18.08.xlsx"
)

if (!file.exists(learner_file)) {
  stop(
    "The learner workbook was not found. Confirm that it is in data_raw/ and ",
    "that the filename exactly matches the name in learner_file."
  )
}

# 5. Import the labeled workbook ------------------------------------------------
# Importing all columns as text is intentional. It prevents automatic type
# guessing and preserves every response category exactly as displayed in Excel.
learner_import <- read_excel(
  path = learner_file,
  sheet = 1,
  col_types = "text",
  na = c("")
)

cat("Imported rows, including the Qualtrics question-label row:",
    nrow(learner_import), "\n")
cat("Imported columns:", ncol(learner_import), "\n")

# 6. Preserve a question/variable dictionary -----------------------------------
# The first row below the Excel headers contains the full Qualtrics question text.
question_dictionary <- tibble(
  original_variable = names(learner_import),
  variable = janitor::make_clean_names(names(learner_import)),
  question_text = learner_import[1, ] |>
    unlist(use.names = FALSE) |>
    as.character()
)

# 7. Remove the question-label row and clean column names -----------------------
learner_all <- learner_import |>
  slice(-1) |>
  clean_names() |>
  mutate(raw_export_row = row_number())

# 8. Parse only the administrative fields needed for screening -----------------
learner_all <- learner_all |>
  mutate(
    progress = readr::parse_number(progress),
    duration_seconds = readr::parse_number(duration_in_seconds),
    finished_flag = str_to_lower(str_trim(finished)) == "true",
    preview_flag = str_to_lower(str_trim(status)) == "survey preview",
    include_analysis = finished_flag & progress == 100 & !preview_flag,
    exclusion_reason = case_when(
      preview_flag ~ "Survey preview",
      is.na(finished_flag) | !finished_flag ~ "Not finished",
      is.na(progress) | progress < 100 ~ "Progress below 100",
      TRUE ~ "Included"
    )
  )

# 9. Audit exclusions before deleting anything ---------------------------------
exclusion_summary <- learner_all |>
  count(exclusion_reason, name = "n") |>
  arrange(desc(n))

print(exclusion_summary)

cat("\nTotal exported response records:", nrow(learner_all), "\n")
cat("Records included for analysis:",
    sum(learner_all$include_analysis, na.rm = TRUE), "\n")

#   Total exported response records: 222
#   Included: 170
#   Survey preview excluded: 1
#   Not finished excluded: 51

# Stop automatically if the expected analysis sample is not reproduced.
if (sum(learner_all$include_analysis, na.rm = TRUE) != 170) {
  warning(
    "The included sample is not 170. Recheck the file version and the ",
    "screening conditions before proceeding."
  )
}

# 10. Build a de-identified learner analysis dataset ----------------------------
# We create a study-specific case ID and select only fields needed for analysis.
# IP address, coordinates, response ID, recipient fields, and timestamps are not
# carried into the analysis dataset.
learner_analysis <- learner_all |>
  filter(include_analysis) |>
  mutate(case_id = sprintf("L%03d", row_number())) |>
  transmute(
    case_id,
    duration_seconds,
    
    # Participant characteristics
    teaching_experience = q2,
    gender = q5,
    age_group = q23,
    first_submission_mastery = q24,
    
    # Assessment and feedback experience
    rubric_clear_aligned = q14,
    feedback_specific_relevant = q8,
    feedback_helped_improve = q9,
    feedback_timely = q10,
    overall_feedback_quality = q11,
    most_valuable_text = q20,
    improvement_text = q21,
    
    # AI experience and perceptions
    prior_ai_experience = q6,
    ai_awareness = q16,
    ai_comfort = q17,
    ai_as_useful_as_human = q18,
    preferred_feedback_model = q19,
    additional_ai_comments = q22
  ) |>
  mutate(
    across(
      where(is.character),
      ~ .x |> str_squish() |> na_if("")
    )
  )

# 11. Verify the imported analysis dataset -------------------------------------
cat("\nAnalysis dataset dimensions:\n")
print(dim(learner_analysis))

cat("\nVariable structure:\n")
glimpse(learner_analysis)

cat("\nDuplicate case IDs:", anyDuplicated(learner_analysis$case_id), "\n")

# Missingness table
missingness_summary <- learner_analysis |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_n"
  ) |>
  mutate(
    total_n = nrow(learner_analysis),
    missing_percent = round(100 * missing_n / total_n, 1)
  ) |>
  arrange(desc(missing_percent), variable)

print(missingness_summary, n = Inf)

# Key category checks. These are still text labels; no ordinal scores have been
# assigned at this stage.
cat("\nMastery status:\n")
print(learner_analysis |> count(first_submission_mastery, sort = TRUE))

cat("\nOverall feedback quality:\n")
print(learner_analysis |> count(overall_feedback_quality, sort = TRUE))

cat("\nAI comfort:\n")
print(learner_analysis |> count(ai_comfort, sort = TRUE))

cat("\nPreferred feedback model:\n")
print(learner_analysis |> count(preferred_feedback_model, sort = TRUE))

# Optional compact overview in the Viewer/Console
skimr::skim(learner_analysis)


# Create output directories
processed_dir <- here::here("data_processed")
quality_dir <- here::here("output", "quality_checks")

dir.create(
  processed_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  quality_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!dir.exists(processed_dir)) {
  stop("Could not create data_processed directory.")
}

if (!dir.exists(quality_dir)) {
  stop("Could not create output/quality_checks directory.")
}

# Define output files
analysis_rds_file <- file.path(
  processed_dir,
  "learner_analysis_phase1.rds"
)

analysis_csv_file <- file.path(
  processed_dir,
  "learner_analysis_phase1.csv"
)

dictionary_file <- file.path(
  processed_dir,
  "learner_question_dictionary.csv"
)

exclusion_file <- file.path(
  quality_dir,
  "learner_exclusion_summary.csv"
)

missingness_file <- file.path(
  quality_dir,
  "learner_missingness_summary.csv"
)

# Save files
saveRDS(
  learner_analysis,
  analysis_rds_file
)

readr::write_csv(
  learner_analysis,
  analysis_csv_file,
  na = ""
)

readr::write_csv(
  question_dictionary,
  dictionary_file,
  na = ""
)

readr::write_csv(
  exclusion_summary,
  exclusion_file,
  na = ""
)

readr::write_csv(
  missingness_summary,
  missingness_file,
  na = ""
)

cat("All files saved successfully.\n")
cat("Quality-check folder:", quality_dir, "\n")