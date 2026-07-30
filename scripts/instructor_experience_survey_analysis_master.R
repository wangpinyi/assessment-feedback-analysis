# ==============================================================================
# INSTRUCTOR EXPERIENCE SURVEY: ASSESSMENT & FEEDBACK PROCESS
# Master analysis script
#
# Purpose:
#   Replicate the phased learner-survey workflow for the instructor survey while
#   respecting the smaller instructor sample.
#
# Primary quantitative analytic sample expected from the July 23, 2026 export:
#   n = 31 usable completed responses
#
# Additional substantive partial response:
#   n = 1; excluded from primary quantitative models but retained for available-
#   item and open-text analysis.
#
# Important analytic constraints:
#   1. Duration is intentionally excluded from analysis.
#   2. Gender and educator/instructor experience are descriptive only because
#      the observed distributions have little or no variation.
#   3. Do not run EFA, CFA, SEM, or large multivariable models with n ≈ 31.
#   4. Inferential analyses are exploratory; emphasize estimates, confidence
#      intervals, exact tests, and convergence with qualitative comments.
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. PACKAGES
# ------------------------------------------------------------------------------

required_packages <- c(
  "tidyverse",
  "readxl",
  "here",
  "janitor",
  "psych",
  "broom",
  "brglm2",
  "openxlsx",
  "scales"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Install the following packages before running this script: ",
      paste(missing_packages, collapse = ", "),
      "\nExample: install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    )
  )
}

library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(psych)
library(broom)
library(brglm2)
library(openxlsx)
library(scales)


# ------------------------------------------------------------------------------
# 1. PROJECT PATHS
# ------------------------------------------------------------------------------

# Recommended: place the raw workbook in data_raw/ and rename it to:
# instructor_experience_survey_raw.xlsx
#
# The script also recognizes the original Qualtrics-export filename.

candidate_input_paths <- c(
  here(
    "data_raw",
    "instructor_experience_survey_raw.xlsx"
  ),
  here(
    "data_raw",
    paste0(
      "Labeled_Instructor Experience Survey_ Assessment & Feedback Process",
      " - Copy_July 23, 2026_18.04(1).xlsx"
    )
  )
)

existing_input_paths <- candidate_input_paths[
  file.exists(candidate_input_paths)
]

if (length(existing_input_paths) == 0) {
  stop(
    paste0(
      "Instructor survey workbook not found.\n",
      "Place it in: ",
      here("data_raw"),
      "\nRecommended filename: instructor_experience_survey_raw.xlsx"
    )
  )
}

input_path <- existing_input_paths[[1]]

processed_dir <- here("data_processed")
table_dir <- here("output", "instructor", "tables")
figure_dir <- here("output", "instructor", "figures")
coding_dir <- here("output", "instructor", "qualitative")

walk(
  c(
    processed_dir,
    table_dir,
    figure_dir,
    coding_dir
  ),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 2. UF FIGURE THEME
# ------------------------------------------------------------------------------

uf_blue <- "#0021A5"
uf_orange <- "#FA4616"
uf_light_blue <- "#6C9AC3"
uf_light_orange <- "#F2A900"
uf_gray <- "#6C757D"
uf_light_gray <- "#E9ECEF"

theme_instructor <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(
        face = "bold",
        color = uf_blue,
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        color = uf_gray,
        margin = margin(b = 10)
      ),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.title = element_text(face = "bold"),
      plot.caption = element_text(
        color = uf_gray,
        hjust = 0
      )
    )
}


# ==============================================================================
# PHASE 1: IMPORT, QUESTION DICTIONARY, AND DATA-QUALITY REVIEW
# ==============================================================================

# ------------------------------------------------------------------------------
# 3. IMPORT THE QUALTRICS WORKBOOK
# ------------------------------------------------------------------------------

# Import everything as text. This prevents the second Qualtrics header row
# (question wording) from forcing inconsistent column types.

instructor_import <- read_excel(
  path = input_path,
  sheet = 1,
  col_types = "text"
) |>
  clean_names()

# Qualtrics labeled exports use:
#   Row 1: short variable names in the workbook header
#   Row 2: full question wording
#   Remaining rows: responses

analysis_name_map <- c(
  q1 = "instructor_cohorts",
  q2 = "gender",
  q3 = "educator_experience",
  q4 = "prior_ai_experience",
  q5 = "prior_ai_grading_tool",
  q8 = "review_time",
  q9 = "time_impact",
  q10 = "workflow_fit",
  q11 = "workflow_comment",
  q13 = "rubric_alignment",
  q14 = "feedback_clarity",
  q15 = "score_accuracy",
  q17 = "score_trust",
  q18 = "current_model_appropriate",
  q19 = "continuation_recommendation",
  q19_2_text = "continuation_modification_text",
  q20 = "preferred_ai_model",
  q21 = "additional_comment"
)

question_dictionary <- tibble(
  raw_variable = names(instructor_import),
  question_text = unlist(
    instructor_import[1, ],
    use.names = FALSE
  )
) |>
  mutate(
    analysis_variable = recode(
      raw_variable,
      !!!analysis_name_map,
      .default = raw_variable
    )
  )

write_csv(
  question_dictionary,
  file.path(
    processed_dir,
    "instructor_question_dictionary.csv"
  )
)


# ------------------------------------------------------------------------------
# 4. REMOVE QUESTION-TEXT ROW AND CREATE COMPLETION FLAGS
# ------------------------------------------------------------------------------

answer_variables_raw <- c(
  "q1",
  "q2",
  "q3",
  "q4",
  "q5",
  "q8",
  "q9",
  "q10",
  "q11",
  "q13",
  "q14",
  "q15",
  "q17",
  "q18",
  "q19",
  "q19_2_text",
  "q20",
  "q21"
)

instructor_raw_responses <- instructor_import |>
  slice(-1) |>
  mutate(
    across(
      everything(),
      \(x) na_if(str_trim(as.character(x)), "")
    ),
    progress_numeric = suppressWarnings(
      as.numeric(progress)
    ),
    finished_flag = str_to_lower(
      finished
    ) == "true",
    answer_count = rowSums(
      across(
        all_of(answer_variables_raw),
        \(x) !is.na(x)
      )
    ),
    primary_eligible =
      finished_flag &
      progress_numeric == 100 &
      answer_count > 0,
    partial_substantive =
      answer_count > 0 &
      !primary_eligible
  )

quality_summary <- tribble(
  ~quality_indicator, ~n,
  "Imported response rows", nrow(instructor_raw_responses),
  "Finished at 100% progress", sum(
    instructor_raw_responses$finished_flag &
      instructor_raw_responses$progress_numeric == 100,
    na.rm = TRUE
  ),
  "Finished/100% but blank survey answers", sum(
    instructor_raw_responses$finished_flag &
      instructor_raw_responses$progress_numeric == 100 &
      instructor_raw_responses$answer_count == 0,
    na.rm = TRUE
  ),
  "Substantive partial responses", sum(
    instructor_raw_responses$partial_substantive,
    na.rm = TRUE
  ),
  "Primary quantitative analytic sample", sum(
    instructor_raw_responses$primary_eligible,
    na.rm = TRUE
  )
)

print(quality_summary)

primary_raw <- instructor_raw_responses |>
  filter(primary_eligible)

partial_raw <- instructor_raw_responses |>
  filter(partial_substantive)

all_substantive_raw <- instructor_raw_responses |>
  filter(answer_count > 0)

if (nrow(primary_raw) != 31) {
  warning(
    paste0(
      "The primary analytic sample is ",
      nrow(primary_raw),
      ", not the expected 31. Review quality_summary."
    )
  )
}


# ==============================================================================
# PHASE 2: VARIABLE RENAMING AND RECODING
# ==============================================================================

# ------------------------------------------------------------------------------
# 5. RECODING FUNCTION
# ------------------------------------------------------------------------------

standard_agreement_levels <- c(
  "Strongly disagree",
  "Somewhat disagree",
  "Neither agree nor disagree",
  "Somewhat agree",
  "Strongly agree"
)

quality_agreement_levels <- c(
  "Strongly disagree",
  "Disagree",
  "Neutral",
  "Agree",
  "Strongly agree"
)

recode_instructor_data <- function(data) {

  data |>
    rename(
      instructor_cohorts = q1,
      gender = q2,
      educator_experience = q3,
      prior_ai_experience = q4,
      prior_ai_grading_tool = q5,
      review_time = q8,
      time_impact = q9,
      workflow_fit = q10,
      workflow_comment = q11,
      rubric_alignment = q13,
      feedback_clarity = q14,
      score_accuracy = q15,
      score_trust = q17,
      current_model_appropriate = q18,
      continuation_recommendation = q19,
      continuation_modification_text = q19_2_text,
      preferred_ai_model = q20,
      additional_comment = q21
    ) |>
    mutate(
      # Normalize capitalization across related agreement scales.
      across(
        c(
          rubric_alignment,
          feedback_clarity
        ),
        \(x) recode(
          x,
          "Strongly Disagree" = "Strongly disagree",
          "Strongly Agree" = "Strongly agree",
          .default = x
        )
      ),

      instructor_cohorts = factor(
        instructor_cohorts,
        levels = c(
          "1 cohort",
          "2-3 cohorts",
          "4-5 cohorts",
          "6 or more cohorts"
        ),
        ordered = TRUE
      ),

      gender = factor(
        gender
      ),

      educator_experience = factor(
        educator_experience,
        levels = c(
          "0-3",
          "4-10",
          "11-15",
          "16-20",
          "20+"
        ),
        ordered = TRUE
      ),

      prior_ai_experience = factor(
        prior_ai_experience,
        levels = c(
          "No experience",
          "Minimal experience",
          "Some experience",
          "Extensive experience"
        ),
        ordered = TRUE
      ),

      prior_ai_grading_tool = factor(
        prior_ai_grading_tool,
        levels = c(
          "No",
          "Not sure",
          "Yes"
        )
      ),

      review_time = factor(
        review_time,
        levels = c(
          "Less than 3 minutes",
          "3-5 minutes",
          "6-10 minutes",
          "11-15 minutes",
          "16-20 minutes",
          "More than 20 minutes"
        ),
        ordered = TRUE
      ),

      time_impact = factor(
        time_impact,
        levels = c(
          "Added significant time",
          "Added some time",
          "Made no difference",
          "Saved some time",
          "Saved significant time"
        ),
        ordered = TRUE
      ),

      workflow_fit = factor(
        workflow_fit,
        levels = standard_agreement_levels,
        ordered = TRUE
      ),

      rubric_alignment = factor(
        rubric_alignment,
        levels = quality_agreement_levels,
        ordered = TRUE
      ),

      feedback_clarity = factor(
        feedback_clarity,
        levels = quality_agreement_levels,
        ordered = TRUE
      ),

      score_accuracy = factor(
        score_accuracy,
        levels = c(
          "Poor",
          "Fair",
          "Good",
          "Excellent"
        ),
        ordered = TRUE
      ),

      score_trust = factor(
        score_trust,
        levels = standard_agreement_levels,
        ordered = TRUE
      ),

      current_model_appropriate = factor(
        current_model_appropriate,
        levels = standard_agreement_levels,
        ordered = TRUE
      ),

      continuation_recommendation = factor(
        continuation_recommendation,
        levels = c(
          "Continue as-is",
          "Continue with modifications (please describe)",
          "Discontinue"
        )
      ),

      preferred_ai_model = factor(
        preferred_ai_model,
        levels = c(
          paste0(
            "AI grades and delivers feedback automatically; ",
            "I spot-check a random sample."
          ),
          paste0(
            "AI flags low-confidence submissions for my review; ",
            "the rest are auto-finalized"
          ),
          paste0(
            "AI provides a first-pass grade and feedback; ",
            "I review and approve every submission."
          ),
          paste0(
            "AI provides formative feedback only; ",
            "I grade all submissions independently."
          ),
          "I have no strong preference."
        )
      ),

      preferred_model_group = case_when(
        preferred_ai_model %in% c(
          paste0(
            "AI grades and delivers feedback automatically; ",
            "I spot-check a random sample."
          ),
          paste0(
            "AI flags low-confidence submissions for my review; ",
            "the rest are auto-finalized"
          )
        ) ~ "Limited human review",

        preferred_ai_model ==
          paste0(
            "AI provides a first-pass grade and feedback; ",
            "I review and approve every submission."
          ) ~ "Universal human review",

        preferred_ai_model ==
          paste0(
            "AI provides formative feedback only; ",
            "I grade all submissions independently."
          ) ~ "Human-led grading",

        preferred_ai_model ==
          "I have no strong preference." ~
          "No strong preference",

        TRUE ~ NA_character_
      ),

      preferred_model_group = factor(
        preferred_model_group,
        levels = c(
          "Limited human review",
          "Universal human review",
          "Human-led grading",
          "No strong preference"
        )
      ),

      # Numeric ordinal scores for exploratory association analyses.
      instructor_cohorts_score = as.integer(
        instructor_cohorts
      ),

      educator_experience_score = as.integer(
        educator_experience
      ),

      prior_ai_experience_score = as.integer(
        prior_ai_experience
      ) - 1,

      review_time_score = as.integer(
        review_time
      ),

      time_impact_score = case_when(
        time_impact == "Added significant time" ~ -2,
        time_impact == "Added some time" ~ -1,
        time_impact == "Made no difference" ~ 0,
        time_impact == "Saved some time" ~ 1,
        time_impact == "Saved significant time" ~ 2,
        TRUE ~ NA_real_
      ),

      workflow_fit_score = as.integer(
        workflow_fit
      ),

      rubric_alignment_score = as.integer(
        rubric_alignment
      ),

      feedback_clarity_score = as.integer(
        feedback_clarity
      ),

      score_accuracy_score = as.integer(
        score_accuracy
      ),

      score_trust_score = as.integer(
        score_trust
      ),

      current_model_appropriate_score = as.integer(
        current_model_appropriate
      ),

      # Binary outcomes and predictors for exact/bias-reduced analyses.
      support_continuation = case_when(
        is.na(continuation_recommendation) ~ NA_integer_,
        continuation_recommendation == "Discontinue" ~ 0L,
        TRUE ~ 1L
      ),

      continuation_support_label = factor(
        support_continuation,
        levels = c(0, 1),
        labels = c(
          "Discontinue",
          "Continue in some form"
        )
      ),

      time_saving_binary = case_when(
        is.na(time_impact) ~ NA_character_,
        time_impact %in% c(
          "Saved some time",
          "Saved significant time"
        ) ~ "Saved time",
        TRUE ~ "No saving or added time"
      ),

      workflow_fit_positive = case_when(
        is.na(workflow_fit_score) ~ NA_character_,
        workflow_fit_score >= 4 ~ "Agree",
        TRUE ~ "Neutral/disagree"
      ),

      trust_positive = case_when(
        is.na(score_trust_score) ~ NA_character_,
        score_trust_score >= 4 ~ "Agree",
        TRUE ~ "Neutral/disagree"
      ),

      model_appropriate_positive = case_when(
        is.na(current_model_appropriate_score) ~ NA_character_,
        current_model_appropriate_score >= 4 ~ "Agree",
        TRUE ~ "Neutral/disagree"
      ),

      human_oversight_score = case_when(
        preferred_model_group ==
          "Limited human review" ~ 1,
        preferred_model_group ==
          "Universal human review" ~ 2,
        preferred_model_group ==
          "Human-led grading" ~ 3,
        TRUE ~ NA_real_
      ),

      # Rescale the four evaluation indicators to 0-1 before averaging.
      # This prevents the four-point accuracy item from receiving less weight.
      evaluation_item_count = rowSums(
        across(
          c(
            rubric_alignment_score,
            feedback_clarity_score,
            score_accuracy_score,
            score_trust_score
          ),
          \(x) !is.na(x)
        )
      ),

      ai_evaluation_index = if_else(
        evaluation_item_count >= 3,
        rowMeans(
          cbind(
            (rubric_alignment_score - 1) / 4,
            (feedback_clarity_score - 1) / 4,
            (score_accuracy_score - 1) / 3,
            (score_trust_score - 1) / 4
          ),
          na.rm = TRUE
        ),
        NA_real_
      )
    )
}


# ------------------------------------------------------------------------------
# 6. CREATE PRIMARY AND ALL-SUBSTANTIVE DATASETS
# ------------------------------------------------------------------------------

instructor_analysis <- recode_instructor_data(
  primary_raw
)

instructor_all_substantive <- recode_instructor_data(
  all_substantive_raw
)

instructor_partial <- recode_instructor_data(
  partial_raw
)


# ------------------------------------------------------------------------------
# 7. MISSINGNESS REVIEW
# ------------------------------------------------------------------------------

analysis_variables <- c(
  "instructor_cohorts",
  "gender",
  "educator_experience",
  "prior_ai_experience",
  "prior_ai_grading_tool",
  "review_time",
  "time_impact",
  "workflow_fit",
  "workflow_comment",
  "rubric_alignment",
  "feedback_clarity",
  "score_accuracy",
  "score_trust",
  "current_model_appropriate",
  "continuation_recommendation",
  "continuation_modification_text",
  "preferred_ai_model",
  "additional_comment"
)

missingness_summary <- map_dfr(
  analysis_variables,
  \(variable_name) {
    tibble(
      variable = variable_name,
      analytic_n = nrow(instructor_analysis),
      nonmissing_n = sum(
        !is.na(
          instructor_analysis[[variable_name]]
        )
      ),
      missing_n = sum(
        is.na(
          instructor_analysis[[variable_name]]
        )
      ),
      percent_missing = 100 * missing_n / analytic_n
    )
  }
)

print(missingness_summary)


# ------------------------------------------------------------------------------
# ELIGIBILITY-ADJUSTED MISSINGNESS FOR THE MODIFICATION FOLLOW-UP
# ------------------------------------------------------------------------------

modification_followup_missingness <- instructor_analysis |>
  filter(
    continuation_recommendation ==
      "Continue with modifications (please describe)"
  ) |>
  summarise(
    variable = "continuation_modification_text",
    eligible_n = n(),
    nonmissing_n = sum(
      !is.na(
        continuation_modification_text
      )
    ),
    missing_n = sum(
      is.na(
        continuation_modification_text
      )
    ),
    percent_missing_among_eligible =
      100 * missing_n / eligible_n
  )

print(
  modification_followup_missingness
)



# ------------------------------------------------------------------------------
# 8. SAVE PHASE 1-2 DATA
# ------------------------------------------------------------------------------

saveRDS(
  instructor_analysis,
  file.path(
    processed_dir,
    "instructor_analysis_phase2_recoded.rds"
  )
)

write_csv(
  instructor_analysis,
  file.path(
    processed_dir,
    "instructor_analysis_phase2_recoded.csv"
  )
)

write_csv(
  instructor_partial,
  file.path(
    processed_dir,
    "instructor_substantive_partial_response.csv"
  )
)

write.xlsx(
  x = list(
    Quality_Summary = quality_summary,
    Missingness = missingness_summary,
    Question_Dictionary = question_dictionary
  ),
  file = file.path(
    table_dir,
    "instructor_phase1_quality_checks.xlsx"
  ),
  overwrite = TRUE
)


# ==============================================================================
# PHASE 3: DESCRIPTIVE ANALYSIS
# ==============================================================================

# ------------------------------------------------------------------------------
# 9. FREQUENCY-TABLE FUNCTION
# ------------------------------------------------------------------------------

make_frequency_table <- function(
    data,
    variable_name,
    variable_label
) {

  valid_n <- sum(
    !is.na(
      data[[variable_name]]
    )
  )

  data |>
    transmute(
      response = as.character(
        .data[[variable_name]]
      )
    ) |>
    mutate(
      response = replace_na(
        response,
        "Missing"
      )
    ) |>
    count(
      response,
      name = "n"
    ) |>
    mutate(
      variable = variable_name,
      variable_label = variable_label,
      analytic_n = nrow(data),
      valid_n = valid_n,
      percent_total = 100 * n / analytic_n,
      percent_valid = if_else(
        response == "Missing" |
          valid_n == 0,
        NA_real_,
        100 * n / valid_n
      )
    ) |>
    select(
      variable,
      variable_label,
      response,
      n,
      analytic_n,
      valid_n,
      percent_total,
      percent_valid
    )
}


# ------------------------------------------------------------------------------
# 10. DESCRIPTIVE TABLES
# ------------------------------------------------------------------------------

frequency_specs <- tribble(
  ~variable, ~label,
  "instructor_cohorts",
  "Number of literacy-course cohorts taught",
  "gender",
  "Gender",
  "educator_experience",
  "Years in educator or instructional roles",
  "prior_ai_experience",
  "Prior experience using AI",
  "prior_ai_grading_tool",
  "Prior use of AI grading/feedback tools",
  "review_time",
  "Review time per submission",
  "time_impact",
  "Perceived effect on grading time",
  "workflow_fit",
  "Fit with existing grading workflow",
  "rubric_alignment",
  "AI feedback aligned with rubric",
  "feedback_clarity",
  "AI feedback clear, specific, and constructive",
  "score_accuracy",
  "Accuracy of AI-generated scores",
  "score_trust",
  "Trust in AI scores as a review starting point",
  "current_model_appropriate",
  "Appropriateness of the current model",
  "continuation_recommendation",
  "Recommendation about continuing the current model",
  "preferred_model_group",
  "Preferred future oversight model"
)

descriptive_tables <- pmap(
  frequency_specs,
  \(variable, label) {
    make_frequency_table(
      instructor_analysis,
      variable,
      label
    )
  }
)

names(descriptive_tables) <- c(
  "Instructor_Cohorts",
  "Gender",
  "Educator_Experience",
  "Prior_AI_Experience",
  "Prior_AI_Grading",
  "Review_Time",
  "Time_Impact",
  "Workflow_Fit",
  "Rubric_Alignment",
  "Feedback_Clarity",
  "Score_Accuracy",
  "Score_Trust",
  "Model_Appropriate",
  "Continuation",
  "Preferred_Model"
)

descriptive_long <- bind_rows(
  descriptive_tables
)

open_text_response_counts <- tibble(
  open_text_source = c(
    "Workflow comment",
    "Requested modification",
    "Additional comment"
  ),
  nonmissing_n_primary = c(
    sum(
      !is.na(
        instructor_analysis$workflow_comment
      )
    ),
    sum(
      !is.na(
        instructor_analysis$continuation_modification_text
      )
    ),
    sum(
      !is.na(
        instructor_analysis$additional_comment
      )
    )
  ),
  nonmissing_n_all_substantive = c(
    sum(
      !is.na(
        instructor_all_substantive$workflow_comment
      )
    ),
    sum(
      !is.na(
        instructor_all_substantive$continuation_modification_text
      )
    ),
    sum(
      !is.na(
        instructor_all_substantive$additional_comment
      )
    )
  )
)

descriptive_export <- c(
  list(
    Quality_Summary = quality_summary,
    Open_Text_Counts = open_text_response_counts,
    All_Frequencies = descriptive_long
  ),
  descriptive_tables
)

write.xlsx(
  x = descriptive_export,
  file = file.path(
    table_dir,
    "instructor_phase3_descriptive_tables.xlsx"
  ),
  overwrite = TRUE
)

print(descriptive_tables)

# ------------------------------------------------------------------------------
# 11. DESCRIPTIVE FIGURE FUNCTION
# ------------------------------------------------------------------------------

make_bar_plot <- function(
    data,
    variable_name,
    title,
    subtitle = NULL,
    fill_color = uf_blue
) {

  plot_data <- data |>
    filter(
      !is.na(
        .data[[variable_name]]
      )
    ) |>
    count(
      response = .data[[variable_name]],
      name = "n",
      .drop = FALSE
    ) |>
    filter(n > 0) |>
    mutate(
      percent = 100 * n / sum(n),
      response_label = as.character(
        response
      ),
      response_label = fct_reorder(
        response_label,
        percent
      ),
      display_label = sprintf(
        "%.1f%% (n = %d)",
        percent,
        n
      )
    )

  ggplot(
    plot_data,
    aes(
      x = response_label,
      y = percent
    )
  ) +
    geom_col(
      fill = fill_color,
      width = 0.72
    ) +
    geom_text(
      aes(
        label = display_label
      ),
      hjust = -0.05,
      size = 3.7
    ) +
    coord_flip(
      clip = "off"
    ) +
    scale_y_continuous(
      labels = label_percent(
        scale = 1
      ),
      expand = expansion(
        mult = c(0, 0.25)
      )
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Percent of valid responses",
      caption = paste0(
        "Valid n = ",
        sum(
          !is.na(
            data[[variable_name]]
          )
        )
      )
    ) +
    theme_instructor()
}



# ------------------------------------------------------------------------------
# 12. PHASE 3 FIGURES
# ------------------------------------------------------------------------------

review_time_plot <- make_bar_plot(
  instructor_analysis,
  "review_time",
  "Instructor Review Time per Submission",
  "Time spent reviewing AI-generated grades and feedback",
  uf_blue
)

ggsave(
  filename = file.path(
    figure_dir,
    "instructor_review_time.png"
  ),
  plot = review_time_plot,
  width = 9,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

time_impact_plot <- make_bar_plot(
  instructor_analysis,
  "time_impact",
  "Perceived Effect of AI Assistance on Grading Time",
  NULL,
  uf_orange
)

ggsave(
  filename = file.path(
    figure_dir,
    "instructor_time_impact.png"
  ),
  plot = time_impact_plot,
  width = 9,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

accuracy_plot <- make_bar_plot(
  instructor_analysis,
  "score_accuracy",
  "Perceived Accuracy of AI-Generated Scores",
  "Relative to scores instructors would have assigned",
  uf_blue
)

ggsave(
  filename = file.path(
    figure_dir,
    "instructor_score_accuracy.png"
  ),
  plot = accuracy_plot,
  width = 9,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

continuation_plot <- make_bar_plot(
  instructor_analysis,
  "continuation_recommendation",
  "Recommendation for the Current AI-Assisted Grading Model",
  NULL,
  uf_orange
)

ggsave(
  filename = file.path(
    figure_dir,
    "instructor_continuation_recommendation.png"
  ),
  plot = continuation_plot,
  width = 10,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

preferred_model_plot <- make_bar_plot(
  instructor_analysis,
  "preferred_model_group",
  "Preferred Future Model for AI-Assisted Grading",
  "Models grouped by the extent of human review",
  uf_blue
)

ggsave(
  filename = file.path(
    figure_dir,
    "instructor_preferred_model.png"
  ),
  plot = preferred_model_plot,
  width = 9,
  height = 5.5,
  dpi = 300,
  bg = "white"
)


# ------------------------------------------------------------------------------
# 13. CROSS-ITEM POSITIVE / NEUTRAL / NEGATIVE FIGURE
# ------------------------------------------------------------------------------

evaluation_direction_data <- instructor_analysis |>
  select(
    workflow_fit,
    rubric_alignment,
    feedback_clarity,
    score_trust,
    current_model_appropriate
  ) |>
  mutate(
    across(
      everything(),
      as.character
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "response"
  ) |>
  filter(
    !is.na(response)
  ) |>
  mutate(
    direction = case_when(
      response %in% c(
        "Strongly disagree",
        "Somewhat disagree",
        "Disagree"
      ) ~ "Negative",
      
      response %in% c(
        "Neither agree nor disagree",
        "Neutral"
      ) ~ "Neutral",
      
      response %in% c(
        "Somewhat agree",
        "Agree",
        "Strongly agree"
      ) ~ "Positive",
      
      TRUE ~ NA_character_
    ),
    
    item = recode(
      item,
      workflow_fit =
        "Workflow fit",
      rubric_alignment =
        "Rubric alignment",
      feedback_clarity =
        "Feedback clarity",
      score_trust =
        "Trust as starting point",
      current_model_appropriate =
        "Current model appropriate"
    ),
    
    item = factor(
      item,
      levels = rev(
        c(
          "Workflow fit",
          "Rubric alignment",
          "Feedback clarity",
          "Trust as starting point",
          "Current model appropriate"
        )
      )
    ),
    
    direction = factor(
      direction,
      levels = c(
        "Negative",
        "Neutral",
        "Positive"
      )
    )
  ) |>
  filter(
    !is.na(direction)
  ) |>
  count(
    item,
    direction,
    name = "n",
    .drop = FALSE
  ) |>
  complete(
    item,
    direction,
    fill = list(
      n = 0
    )
  ) |>
  group_by(
    item
  ) |>
  mutate(
    valid_n = sum(n),
    percent = 100 * n / valid_n
  ) |>
  ungroup()

print(
  evaluation_direction_data,
  n = Inf
)


evaluation_direction_plot <- ggplot(
  evaluation_direction_data,
  aes(
    x = item,
    y = percent,
    fill = direction
  )
) +
  geom_col(
    width = 0.72,
    position = position_stack(
      reverse = TRUE
    )
  ) +
  geom_text(
    aes(
      label = if_else(
        n > 0,
        sprintf(
          "%.1f%%",
          percent
        ),
        ""
      )
    ),
    position = position_stack(
      vjust = 0.5,
      reverse = TRUE
    ),
    size = 3.5
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Negative" = uf_light_blue,
      "Neutral" = uf_light_gray,
      "Positive" = uf_orange
    ),
    breaks = c(
      "Negative",
      "Neutral",
      "Positive"
    ),
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = seq(
      0,
      100,
      by = 20
    ),
    labels = label_percent(
      scale = 1
    ),
    expand = expansion(
      mult = c(
        0,
        0.01
      )
    )
  ) +
  labs(
    title =
      "Instructor Evaluation of AI-Assisted Grading",
    subtitle = paste0(
      "Responses harmonized as negative, neutral, ",
      "or positive across item-specific scales"
    ),
    x = NULL,
    y = "Percent of valid responses",
    fill = "Response direction",
    caption = paste0(
      "Primary analytic sample n = ",
      nrow(
        instructor_analysis
      )
    )
  ) +
  theme_instructor() +
  theme(
    legend.position = "bottom"
  )

print(
  evaluation_direction_plot
)

ggsave(
  filename = file.path(
    figure_dir,
    "instructor_ai_evaluation_direction.png"
  ),
  plot = evaluation_direction_plot,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)



# ==============================================================================
# PHASE 4A: EVALUATION-ITEM ASSOCIATIONS AND RELIABILITY
# ==============================================================================

# The four indicators below form an exploratory AI-evaluation set:
#   rubric alignment, feedback clarity, score accuracy, and score trust.
#
# Because n is small and response cells are sparse:
#   - Spearman correlations are primary.
#   - Standardized alpha is descriptive.
#   - Polychoric results are sensitivity checks only.
#   - Do not conduct factor analysis.

evaluation_score_variables <- c(
  "rubric_alignment_score",
  "feedback_clarity_score",
  "score_accuracy_score",
  "score_trust_score"
)

evaluation_scores <- instructor_analysis |>
  select(
    all_of(
      evaluation_score_variables
    )
  ) |>
  drop_na()

alpha_object <- psych::alpha(
  evaluation_scores,
  check.keys = FALSE,
  warnings = FALSE
)

reliability_summary <- tibble(
  analytic_n = nrow(
    evaluation_scores
  ),
  
  number_of_items = ncol(
    evaluation_scores
  ),
  
  raw_alpha = as.numeric(
    alpha_object$total$raw_alpha
  ),
  
  standardized_alpha = as.numeric(
    alpha_object$total$std.alpha
  ),
  
  average_interitem_correlation = as.numeric(
    alpha_object$total$average_r
  )
)

print(
  reliability_summary
)

alpha_item_statistics <- alpha_object$item.stats |>
  as.data.frame() |>
  rownames_to_column(
    "item"
  )

alpha_if_deleted <- alpha_object$alpha.drop |>
  as.data.frame() |>
  rownames_to_column(
    "item"
  )

spearman_correlation_matrix <- cor(
  evaluation_scores,
  use = "pairwise.complete.obs",
  method = "spearman"
) |>
  as.data.frame() |>
  rownames_to_column(
    "item"
  )

polychoric_object <- tryCatch(
  {
    psych::polychoric(
      evaluation_scores
    )
  },
  error = function(e) {
    message(
      "Polychoric analysis was not estimable: ",
      conditionMessage(e)
    )
    NULL
  }
)

if (!is.null(polychoric_object)) {

  polychoric_correlation_matrix <-
    polychoric_object$rho |>
    as.data.frame() |>
    rownames_to_column(
      "item"
    )

} else {

  polychoric_correlation_matrix <- tibble(
    note = paste0(
      "Polychoric correlations were not estimable. ",
      "Use Spearman correlations as the primary result."
    )
  )
}

write.xlsx(
  x = list(
    Reliability_Summary = reliability_summary,
    Item_Statistics = alpha_item_statistics,
    Alpha_If_Deleted = alpha_if_deleted,
    Spearman_Matrix = spearman_correlation_matrix,
    Polychoric_Sensitivity =
      polychoric_correlation_matrix
  ),
  file = file.path(
    table_dir,
    "instructor_phase4A_evaluation_diagnostics.xlsx"
  ),
  overwrite = TRUE
)

# ------------------------------------------------------------------------------
# PHASE 4A DIAGNOSTIC OUTPUTS
# ------------------------------------------------------------------------------

alpha_item_statistics <- alpha_item_statistics |>
  as_tibble()

alpha_if_deleted <- alpha_if_deleted |>
  as_tibble()

spearman_correlation_matrix <-
  spearman_correlation_matrix |>
  as_tibble()

polychoric_correlation_matrix <-
  polychoric_correlation_matrix |>
  as_tibble()


print(
  reliability_summary,
  width = Inf
)

print(
  alpha_item_statistics,
  n = Inf,
  width = Inf
)

print(
  alpha_if_deleted,
  n = Inf,
  width = Inf
)

print(
  spearman_correlation_matrix,
  n = Inf,
  width = Inf
)

print(
  polychoric_correlation_matrix,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# PAIRWISE SPEARMAN TESTS WITH MULTIPLE-TEST ADJUSTMENT
# ------------------------------------------------------------------------------

evaluation_item_pairs <- combn(
  evaluation_score_variables,
  2,
  simplify = FALSE
)

spearman_pairwise_results <- map_dfr(
  evaluation_item_pairs,
  \(item_pair) {
    
    item_1 <- item_pair[[1]]
    item_2 <- item_pair[[2]]
    
    pair_data <- instructor_analysis |>
      select(
        all_of(
          c(
            item_1,
            item_2
          )
        )
      ) |>
      drop_na()
    
    correlation_test <- suppressWarnings(
      cor.test(
        pair_data[[item_1]],
        pair_data[[item_2]],
        method = "spearman",
        exact = FALSE
      )
    )
    
    tibble(
      item_1 = item_1,
      item_2 = item_2,
      analytic_n = nrow(
        pair_data
      ),
      spearman_rho = unname(
        correlation_test$estimate
      ),
      statistic = unname(
        correlation_test$statistic
      ),
      p_value = correlation_test$p.value
    )
  }
) |>
  mutate(
    p_value_bh = p.adjust(
      p_value,
      method = "BH"
    )
  ) |>
  arrange(
    p_value
  )

print(
  spearman_pairwise_results,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# FINAL PHASE 4A EXPORT
# ------------------------------------------------------------------------------

polychoric_method_note <- tibble(
  issue = c(
    "Primary association method",
    "Unequal response alternatives",
    "Sparse contingency-table cells",
    "Composite interpretation"
  ),
  
  note = c(
    paste0(
      "Spearman correlations were treated as the primary association ",
      "estimates because the indicators were ordinal and the sample was small."
    ),
    
    paste0(
      "Score accuracy used four response categories, whereas the other ",
      "evaluation indicators used five response categories."
    ),
    
    paste0(
      "Six zero-frequency cells were adjusted using a continuity correction ",
      "in the polychoric analysis. Polychoric correlations were therefore ",
      "retained only as a sensitivity analysis."
    ),
    
    paste0(
      "The four indicators showed modest internal consistency and were ",
      "retained as related but distinct evaluation dimensions. The composite ",
      "index should be used only in exploratory sensitivity analyses."
    )
  )
)

write.xlsx(
  x = list(
    Reliability_Summary =
      reliability_summary,
    
    Item_Statistics =
      alpha_item_statistics,
    
    Alpha_If_Deleted =
      alpha_if_deleted,
    
    Spearman_Matrix =
      spearman_correlation_matrix,
    
    Spearman_Pairwise_Tests =
      spearman_pairwise_results,
    
    Polychoric_Sensitivity =
      polychoric_correlation_matrix,
    
    Method_Note =
      polychoric_method_note
  ),
  
  file = file.path(
    table_dir,
    "instructor_phase4A_evaluation_diagnostics.xlsx"
  ),
  
  overwrite = TRUE
)



# ==============================================================================
# PHASE 4B: EXPLORATORY BIVARIATE ASSOCIATIONS
# ==============================================================================

# ------------------------------------------------------------------------------
# 14. SPEARMAN ASSOCIATIONS
# ------------------------------------------------------------------------------

association_pairs <- tribble(
  ~predictor, ~outcome, ~interpretation,
  "review_time_score",
  "time_impact_score",
  "Review time versus perceived time saving",
  "time_impact_score",
  "workflow_fit_score",
  "Time saving versus workflow fit",
  "time_impact_score",
  "score_trust_score",
  "Time saving versus trust",
  "time_impact_score",
  "current_model_appropriate_score",
  "Time saving versus model appropriateness",
  "workflow_fit_score",
  "score_trust_score",
  "Workflow fit versus trust",
  "rubric_alignment_score",
  "feedback_clarity_score",
  "Rubric alignment versus feedback clarity",
  "rubric_alignment_score",
  "score_accuracy_score",
  "Rubric alignment versus score accuracy",
  "feedback_clarity_score",
  "score_accuracy_score",
  "Feedback clarity versus score accuracy",
  "score_accuracy_score",
  "score_trust_score",
  "Score accuracy versus trust",
  "score_trust_score",
  "current_model_appropriate_score",
  "Trust versus model appropriateness",
  "current_model_appropriate_score",
  "human_oversight_score",
  "Model appropriateness versus preferred human oversight"
)

run_spearman <- function(
    data,
    predictor,
    outcome,
    interpretation
) {

  analysis_complete <- data |>
    select(
      all_of(
        c(
          predictor,
          outcome
        )
      )
    ) |>
    drop_na()

  if (
    nrow(analysis_complete) < 5 ||
    n_distinct(
      analysis_complete[[predictor]]
    ) < 2 ||
    n_distinct(
      analysis_complete[[outcome]]
    ) < 2
  ) {
    return(
      tibble(
        predictor = predictor,
        outcome = outcome,
        interpretation = interpretation,
        analytic_n = nrow(
          analysis_complete
        ),
        spearman_rho = NA_real_,
        statistic = NA_real_,
        p_value = NA_real_
      )
    )
  }

  test_result <- cor.test(
    analysis_complete[[predictor]],
    analysis_complete[[outcome]],
    method = "spearman",
    exact = FALSE
  )

  tibble(
    predictor = predictor,
    outcome = outcome,
    interpretation = interpretation,
    analytic_n = nrow(
      analysis_complete
    ),
    spearman_rho = unname(
      test_result$estimate
    ),
    statistic = unname(
      test_result$statistic
    ),
    p_value = test_result$p.value
  )
}

spearman_results <- pmap_dfr(
  association_pairs,
  \(predictor, outcome, interpretation) {
    run_spearman(
      instructor_analysis,
      predictor,
      outcome,
      interpretation
    )
  }
) |>
  mutate(
    p_value_bh = p.adjust(
      p_value,
      method = "BH"
    )
  )


# ------------------------------------------------------------------------------
# DIRECTIONALLY STANDARDIZED FISHER EXACT TESTS
#
# OR > 1 means higher odds of recommending continuation in the positive group
# relative to the reference group.
# ------------------------------------------------------------------------------

fisher_specs_directional <- tribble(
  ~predictor,
  ~predictor_label,
  ~positive_level,
  ~reference_level,
  
  "time_saving_binary",
  "Perceived time saving",
  "Saved time",
  "No saving or added time",
  
  "workflow_fit_positive",
  "Positive workflow fit",
  "Agree",
  "Neutral/disagree",
  
  "trust_positive",
  "Positive trust",
  "Agree",
  "Neutral/disagree",
  
  "model_appropriate_positive",
  "Positive assessment of current model",
  "Agree",
  "Neutral/disagree"
)


run_directional_fisher <- function(
    data,
    predictor,
    predictor_label,
    positive_level,
    reference_level
) {
  
  analysis_complete <- data |>
    transmute(
      predictor_group = factor(
        .data[[predictor]],
        levels = c(
          positive_level,
          reference_level
        ),
        labels = c(
          "Positive",
          "Reference"
        )
      ),
      
      continuation_group = factor(
        support_continuation,
        levels = c(
          1,
          0
        ),
        labels = c(
          "Continue",
          "Discontinue"
        )
      )
    ) |>
    drop_na()
  
  contingency_table <- table(
    analysis_complete$predictor_group,
    analysis_complete$continuation_group
  )
  
  if (
    !all(
      dim(contingency_table) == c(2, 2)
    )
  ) {
    return(
      tibble(
        predictor = predictor,
        predictor_label = predictor_label,
        analytic_n = nrow(analysis_complete),
        positive_continue_n = NA_integer_,
        positive_discontinue_n = NA_integer_,
        reference_continue_n = NA_integer_,
        reference_discontinue_n = NA_integer_,
        odds_ratio = NA_real_,
        confidence_low = NA_real_,
        confidence_high = NA_real_,
        p_value = NA_real_,
        note = "A 2 x 2 table could not be formed."
      )
    )
  }
  
  test_result <- fisher.test(
    contingency_table
  )
  
  tibble(
    predictor = predictor,
    predictor_label = predictor_label,
    analytic_n = nrow(
      analysis_complete
    ),
    
    positive_continue_n =
      unname(
        contingency_table[
          "Positive",
          "Continue"
        ]
      ),
    
    positive_discontinue_n =
      unname(
        contingency_table[
          "Positive",
          "Discontinue"
        ]
      ),
    
    reference_continue_n =
      unname(
        contingency_table[
          "Reference",
          "Continue"
        ]
      ),
    
    reference_discontinue_n =
      unname(
        contingency_table[
          "Reference",
          "Discontinue"
        ]
      ),
    
    odds_ratio = unname(
      test_result$estimate
    ),
    
    confidence_low =
      test_result$conf.int[1],
    
    confidence_high =
      test_result$conf.int[2],
    
    p_value =
      test_result$p.value,
    
    note = paste0(
      "OR > 1 indicates higher odds of continuation ",
      "in the positive group."
    )
  )
}


fisher_results_directional <- pmap_dfr(
  fisher_specs_directional,
  \(predictor,
    predictor_label,
    positive_level,
    reference_level) {
    
    run_directional_fisher(
      instructor_analysis,
      predictor,
      predictor_label,
      positive_level,
      reference_level
    )
  }
) |>
  mutate(
    p_value_bh = p.adjust(
      p_value,
      method = "BH"
    )
  ) |>
  arrange(
    p_value
  )

print(
  fisher_results_directional,
  n = Inf,
  width = Inf
)


# ==============================================================================
# PHASE 4C: EXPLORATORY CONTINUATION MODELS
# ==============================================================================

# Outcome:
#   1 = Continue as-is OR continue with modifications
#   0 = Discontinue
#
# There are relatively few discontinuation responses. Therefore:
#   - Fit one predictor at a time.
#   - Use mean bias-reduced logistic regression.
#   - Do not combine all predictors into one model.
#   - Treat estimates as exploratory.

continuation_predictors <- tribble(
  ~predictor, ~predictor_label,
  
  "time_impact_score",
  "Perceived time impact (one-category increase)",
  
  "workflow_fit_score",
  "Workflow fit (one-category increase)",
  
  "score_trust_score",
  "Trust in AI scores (one-category increase)",
  
  "current_model_appropriate_score",
  "Current model appropriateness (one-category increase)",
  
  "ai_evaluation_index_10",
  "AI evaluation index (0.10-unit increase)"
)

# ------------------------------------------------------------------------------
# REVISED BIAS-REDUCED LOGISTIC MODEL FUNCTION
# ------------------------------------------------------------------------------

fit_bias_reduced_model <- function(
    data,
    predictor,
    predictor_label
) {
  
  model_data <- data |>
    select(
      support_continuation,
      all_of(
        predictor
      )
    ) |>
    drop_na()
  
  model_formula <- reformulate(
    predictor,
    response = "support_continuation"
  )
  
  fitted_model <- glm(
    formula = model_formula,
    data = model_data,
    family = binomial(
      link = "logit"
    ),
    method = brglm2::brglmFit,
    type = "AS_mean"
  )
  
  coefficient_table <- summary(
    fitted_model
  )$coefficients
  
  predictor_row <- coefficient_table[
    predictor,
    ,
    drop = FALSE
  ]
  
  log_odds_estimate <- predictor_row[
    1,
    "Estimate"
  ]
  
  standard_error <- predictor_row[
    1,
    "Std. Error"
  ]
  
  z_value <- predictor_row[
    1,
    "z value"
  ]
  
  p_value <- predictor_row[
    1,
    "Pr(>|z|)"
  ]
  
  critical_value <- qnorm(
    0.975
  )
  
  confidence_low_log_odds <-
    log_odds_estimate -
    critical_value * standard_error
  
  confidence_high_log_odds <-
    log_odds_estimate +
    critical_value * standard_error
  
  tibble(
    predictor = predictor,
    predictor_label = predictor_label,
    analytic_n = nrow(
      model_data
    ),
    
    log_odds = log_odds_estimate,
    
    standard_error_log_odds =
      standard_error,
    
    z_value = z_value,
    
    odds_ratio = exp(
      log_odds_estimate
    ),
    
    confidence_low = exp(
      confidence_low_log_odds
    ),
    
    confidence_high = exp(
      confidence_high_log_odds
    ),
    
    p_value = p_value,
    
    log_likelihood = as.numeric(
      logLik(
        fitted_model
      )
    ),
    
    aic = AIC(
      fitted_model
    ),
    
    converged = fitted_model$converged
  )
}

# ------------------------------------------------------------------------------
# RESCALE AI EVALUATION INDEX FOR INTERPRETATION
# ------------------------------------------------------------------------------

instructor_analysis <- instructor_analysis |>
  mutate(
    ai_evaluation_index_10 =
      ai_evaluation_index * 10
  )

stopifnot(
  "ai_evaluation_index_10" %in%
    names(instructor_analysis)
)

instructor_analysis |>
  summarise(
    analytic_n = sum(
      !is.na(ai_evaluation_index_10)
    ),
    minimum = min(
      ai_evaluation_index_10,
      na.rm = TRUE
    ),
    maximum = max(
      ai_evaluation_index_10,
      na.rm = TRUE
    ),
    mean = mean(
      ai_evaluation_index_10,
      na.rm = TRUE
    ),
    standard_deviation = sd(
      ai_evaluation_index_10,
      na.rm = TRUE
    )
  ) |>
  print()


### Continuation model results
continuation_model_results <- pmap_dfr(
  continuation_predictors,
  \(predictor, predictor_label) {
    fit_bias_reduced_model(
      instructor_analysis,
      predictor,
      predictor_label
    )
  }
) |>
  mutate(
    p_value_bh = p.adjust(
      p_value,
      method = "BH"
    )
  )

print(
  continuation_model_results,
  n = Inf,
  width = Inf
)

### Print the complete 4B results
print(
  spearman_results |>
    arrange(
      p_value
    ),
  n = Inf,
  width = Inf
)

print(
  fisher_results |>
    arrange(
      p_value
    ),
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# FINAL PHASE 4B OPERATIONAL ASSOCIATION FAMILY
# ------------------------------------------------------------------------------

phase4B_operational_results <- spearman_results |>
  filter(
    interpretation %in% c(
      "Review time versus perceived time saving",
      "Time saving versus workflow fit",
      "Time saving versus trust",
      "Time saving versus model appropriateness",
      "Workflow fit versus trust",
      "Trust versus model appropriateness",
      "Model appropriateness versus preferred human oversight"
    )
  ) |>
  mutate(
    p_value_bh = p.adjust(
      p_value,
      method = "BH"
    )
  ) |>
  arrange(
    p_value
  )

print(
  phase4B_operational_results,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# FINAL PHASE 4B-4C EXPORT
# ------------------------------------------------------------------------------

phase4BC_method_notes <- tibble(
  analysis = c(
    "Phase 4B Spearman associations",
    "Phase 4B Fisher exact tests",
    "Phase 4C continuation models",
    "AI evaluation index"
  ),
  
  note = c(
    paste0(
      "Benjamini-Hochberg adjustment was applied across the seven ",
      "operational Phase 4B associations. Evaluation-item correlations ",
      "were analyzed separately in Phase 4A."
    ),
    
    paste0(
      "Fisher tests used dichotomized predictors and were treated as ",
      "sensitivity analyses. Odds ratios greater than 1 indicate higher ",
      "odds of continuation in the favorable predictor group."
    ),
    
    paste0(
      "Separate mean bias-reduced logistic regressions were fitted because ",
      "only eight instructors recommended discontinuation. No multivariable ",
      "continuation model was estimated."
    ),
    
    paste0(
      "The AI evaluation index was retained only as an exploratory ",
      "sensitivity predictor because its four indicators demonstrated ",
      "modest internal consistency."
    )
  )
)

write.xlsx(
  x = list(
    Phase4B_Spearman =
      phase4B_operational_results,
    
    Phase4B_Fisher_Sensitivity =
      fisher_results_directional,
    
    Phase4C_Bias_Reduced_Models =
      continuation_model_results,
    
    Method_Notes =
      phase4BC_method_notes
  ),
  
  file = file.path(
    table_dir,
    "instructor_phase4B_4C_results.xlsx"
  ),
  
  overwrite = TRUE
)

# ------------------------------------------------------------------------------
# PHASE 4D: PREFERRED MODEL BY CONTINUATION RECOMMENDATION
# ------------------------------------------------------------------------------

preferred_by_continuation <- instructor_analysis |>
  count(
    continuation_recommendation,
    preferred_model_group,
    .drop = FALSE,
    name = "n"
  ) |>
  group_by(
    continuation_recommendation
  ) |>
  mutate(
    group_n = sum(n),
    row_percent = 100 * n / group_n
  ) |>
  ungroup()

print(
  preferred_by_continuation,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------------------------
# PHASE 4D: PREFERRED MODEL BY CURRENT-MODEL APPROPRIATENESS
# ------------------------------------------------------------------------------

preferred_by_appropriateness <- instructor_analysis |>
  mutate(
    appropriateness_group = case_when(
      current_model_appropriate_score >= 4 ~
        "Agree current model is appropriate",
      
      current_model_appropriate_score == 3 ~
        "Neutral",
      
      current_model_appropriate_score <= 2 ~
        "Disagree current model is appropriate",
      
      TRUE ~ NA_character_
    ),
    
    appropriateness_group = factor(
      appropriateness_group,
      levels = c(
        "Disagree current model is appropriate",
        "Neutral",
        "Agree current model is appropriate"
      )
    )
  ) |>
  count(
    appropriateness_group,
    preferred_model_group,
    .drop = FALSE,
    name = "n"
  ) |>
  group_by(
    appropriateness_group
  ) |>
  mutate(
    group_n = sum(n),
    row_percent = 100 * n / group_n
  ) |>
  ungroup()

print(
  preferred_by_appropriateness,
  n = Inf,
  width = Inf
)



# ------------------------------------------------------------------------------
# PHASE 4D FIGURE 1:
# PREFERRED MODEL BY CONTINUATION RECOMMENDATION
# ------------------------------------------------------------------------------

preferred_continuation_plot_data <-
  preferred_by_continuation |>
  mutate(
    continuation_label = recode(
      as.character(
        continuation_recommendation
      ),
      "Continue as-is" =
        "Continue as-is",
      "Continue with modifications (please describe)" =
        "Continue with modifications",
      "Discontinue" =
        "Discontinue"
    ),
    
    continuation_label = factor(
      continuation_label,
      levels = c(
        "Discontinue",
        "Continue with modifications",
        "Continue as-is"
      )
    ),
    
    preferred_model_group = factor(
      preferred_model_group,
      levels = c(
        "Human-led grading",
        "Universal human review",
        "Limited human review",
        "No strong preference"
      )
    ),
    
    display_label = if_else(
      n > 0,
      paste0(
        sprintf(
          "%.1f%%",
          row_percent
        ),
        "\n(n = ",
        n,
        ")"
      ),
      ""
    )
  )


preferred_continuation_plot <- ggplot(
  preferred_continuation_plot_data,
  aes(
    x = continuation_label,
    y = row_percent,
    fill = preferred_model_group
  )
) +
  geom_col(
    width = 0.72
  ) +
  geom_text(
    aes(
      label = display_label
    ),
    position = position_stack(
      vjust = 0.5
    ),
    size = 3.2
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Human-led grading" =
        uf_light_blue,
      "Universal human review" =
        uf_blue,
      "Limited human review" =
        uf_orange,
      "No strong preference" =
        uf_light_gray
    ),
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = seq(
      0,
      100,
      by = 20
    ),
    labels = label_percent(
      scale = 1
    ),
    expand = expansion(
      mult = c(
        0,
        0.01
      )
    )
  ) +
  labs(
    title =
      "Preferred AI Oversight Model by Continuation Recommendation",
    subtitle =
      "Percentages calculated within each continuation group",
    x = NULL,
    y = "Percent within recommendation group",
    fill = "Preferred oversight model",
    caption =
      "Primary analytic sample n = 31"
  ) +
  theme_instructor() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical"
  )

print(
  preferred_continuation_plot
)

ggsave(
  filename = file.path(
    figure_dir,
    "instructor_preferred_model_by_continuation.png"
  ),
  plot = preferred_continuation_plot,
  width = 11,
  height = 6.5,
  dpi = 300,
  bg = "white"
)


# ------------------------------------------------------------------------------
# PHASE 4D FIGURE 2:
# PREFERRED MODEL BY CURRENT-MODEL APPROPRIATENESS
# ------------------------------------------------------------------------------

preferred_appropriateness_plot <- ggplot(
  preferred_appropriateness_plot_data,
  aes(
    x = appropriateness_label,
    y = row_percent,
    fill = preferred_model_group
  )
) +
  geom_col(
    width = 0.72
  ) +
  geom_text(
    aes(
      y = label_position,
      label = display_label,
      color = label_color
    ),
    size = 3.3
  ) +
  scale_color_identity() +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Human-led grading" = "#B56576",
      "Universal human review" = "#6D597A",
      "Limited human review" = "#2A9D8F",
      "No strong preference" = "#D9D9D9"
    ),
    
    breaks = c(
      "No strong preference",
      "Limited human review",
      "Universal human review",
      "Human-led grading"
    ),
    
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = seq(
      0,
      100,
      by = 20
    ),
    labels = label_percent(
      scale = 1
    ),
    expand = expansion(
      mult = c(
        0,
        0.01
      )
    )
  ) +
  labs(
    title =
      "Preferred AI Oversight Model by Perceived Appropriateness",
    subtitle =
      "Percentages calculated within each appropriateness group",
    x = NULL,
    y = "Percent within appropriateness group",
    fill = "Preferred oversight model",
    caption =
      "Primary analytic sample n = 31"
  ) +
  theme_instructor() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical"
  )

print(
  preferred_appropriateness_plot
)

ggsave(
  filename = file.path(
    figure_dir,
    "instructor_preferred_model_by_appropriateness.png"
  ),
  plot = preferred_appropriateness_plot,
  width = 11,
  height = 6.5,
  dpi = 300,
  bg = "white"
)


# ==============================================================================
# PHASE 5: OPEN-ENDED CODING TEMPLATE
# ==============================================================================

# Include all substantive respondents for available open-text analysis,
# including the one partial response. Report the number of comments separately
# for each source.

open_text_long <- bind_rows(
  instructor_all_substantive |>
    transmute(
      response_id,
      comment_source =
        "Workflow experience",
      text = workflow_comment
    ),

  instructor_all_substantive |>
    transmute(
      response_id,
      comment_source =
        "Requested modification",
      text =
        continuation_modification_text
    ),

  instructor_all_substantive |>
    transmute(
      response_id,
      comment_source =
        "Additional evaluation comment",
      text = additional_comment
    )
) |>
  filter(
    !is.na(text),
    str_squish(text) != ""
  ) |>
  mutate(
    comment_id = sprintf(
      "IC%03d",
      row_number()
    ),
    coder = NA_character_,
    code_time_efficiency = NA_integer_,
    code_accuracy_or_rubric_alignment = NA_integer_,
    code_feedback_clarity_or_formatting = NA_integer_,
    code_personalization_or_human_judgment = NA_integer_,
    code_invalid_or_complex_submissions = NA_integer_,
    code_actionable_learner_improvement = NA_integer_,
    code_accessibility_or_submission_format = NA_integer_,
    code_language_or_cultural_responsiveness = NA_integer_,
    code_instructor_capacity_or_consistency = NA_integer_,
    code_human_oversight_preference = NA_integer_,
    code_ethical_or_policy_tension = NA_integer_,
    emergent_code = NA_character_,
    analytic_memo = NA_character_
  ) |>
  select(
    comment_id,
    response_id,
    comment_source,
    text,
    everything()
  )

starter_codebook <- tribble(
  ~code, ~definition, ~include_when, ~exclude_when,
  "time_efficiency",
  "AI saves, adds, or redistributes grading time.",
  "Mentions speed, workload, review time, or efficiency.",
  "General satisfaction without a time/workload reference.",

  "accuracy_or_rubric_alignment",
  "AI scoring or feedback matches rubric expectations or instructor judgment.",
  "Mentions scoring accuracy, rubric criteria, false mastery, or misclassification.",
  "Comments only about writing style or formatting.",

  "feedback_clarity_or_formatting",
  "The readability, organization, specificity, or presentation of AI feedback.",
  "Mentions difficult formatting, clarity, specificity, or readability.",
  "Comments only about whether the score was correct.",

  "personalization_or_human_judgment",
  "Need for contextualized, individualized, reflective, or instructor-informed feedback.",
  "Mentions personalization, critical thinking, nuance, or human review.",
  "Generic preference for AI without a human-judgment rationale.",

  "invalid_or_complex_submissions",
  "Performance with incomplete, invalid, unusual, or conceptually complex submissions.",
  "Mentions invalid assignments, incorrect work, edge cases, or domain misunderstandings.",
  "General accuracy comments without an identifiable complex case.",

  "actionable_learner_improvement",
  "Whether feedback tells learners how to improve practice or revise work.",
  "Mentions next steps, reflection, improvement guidance, or actionability.",
  "General positive/negative feedback without an improvement mechanism.",

  "accessibility_or_submission_format",
  "Technology access, file-conversion, document-format, or submission constraints.",
  "Mentions PDF/Word requirements, conversion, technical access, or usability barriers.",
  "Feedback formatting that concerns readability rather than submission access.",

  "language_or_cultural_responsiveness",
  "Language access or responsiveness to learner linguistic/cultural needs.",
  "Mentions Spanish, translation, language support, or culturally responsive feedback.",
  "General personalization without language/culture content.",

  "instructor_capacity_or_consistency",
  "Instructor knowledge, calibration, consistency, or preparation needed for oversight.",
  "Mentions instructor understanding, calibration, expertise, or consistent review.",
  "Human review preferences without capacity or consistency concerns.",

  "human_oversight_preference",
  "Desired location or intensity of human review in the grading process.",
  "Mentions universal review, flagged review, formative-only AI, or spot checking.",
  "Human-centered comments that do not address oversight design.",

  "ethical_or_policy_tension",
  "Perceived contradiction, fairness issue, or policy concern associated with AI use.",
  "Mentions inconsistent expectations, ethical tension, fairness, or legitimacy.",
  "Technical accuracy or workflow concerns without ethical/policy content."
)

coding_instructions <- tibble(
  instruction = c(
    "Use 1 = present, 0 = absent, and blank = not yet coded.",
    "Apply multiple codes to a comment when warranted.",
    "Code the full comment as the initial unit of analysis.",
    "Use emergent_code for important content not captured by the starter codebook.",
    "After first-cycle coding, consolidate overlapping codes and select illustrative quotations.",
    "Do not report quotations that could identify an instructor."
  )
)

write.xlsx(
  x = list(
    Coding_Template = open_text_long,
    Starter_Codebook = starter_codebook,
    Coding_Instructions = coding_instructions
  ),
  file = file.path(
    coding_dir,
    "instructor_open_text_coding_template.xlsx"
  ),
  overwrite = TRUE
)


# ==============================================================================
# PHASE 6: MIXED-METHODS JOINT DISPLAY TEMPLATE
# ==============================================================================

joint_display_template <- tibble(
  analytic_domain = c(
    "Efficiency and review time",
    "Workflow integration",
    "Feedback alignment and clarity",
    "Score accuracy and trust",
    "Appropriateness and continuation",
    "Preferred human oversight model",
    "Accessibility, responsiveness, and implementation safeguards"
  ),
  quantitative_evidence = NA_character_,
  qualitative_evidence = NA_character_,
  integrated_interpretation = NA_character_,
  design_or_policy_recommendation = NA_character_,
  strength_of_evidence = NA_character_
)

write.xlsx(
  x = list(
    Joint_Display = joint_display_template
  ),
  file = file.path(
    table_dir,
    "instructor_phase6_joint_display_template.xlsx"
  ),
  overwrite = TRUE
)


# ==============================================================================
# 17. FINAL CONSOLE SUMMARY
# ==============================================================================

cat(
  "\n============================================================\n",
  "INSTRUCTOR SURVEY ANALYSIS COMPLETE\n",
  "============================================================\n",
  "Primary quantitative analytic n: ",
  nrow(instructor_analysis),
  "\n",
  "All substantive respondents for open text: ",
  nrow(instructor_all_substantive),
  "\n",
  "Processed data: ",
  file.path(
    processed_dir,
    "instructor_analysis_phase2_recoded.rds"
  ),
  "\n",
  "Tables: ",
  table_dir,
  "\n",
  "Figures: ",
  figure_dir,
  "\n",
  "Qualitative coding template: ",
  coding_dir,
  "\n",
  "============================================================\n",
  sep = ""
)

writeLines(
  capture.output(
    sessionInfo()
  ),
  con = file.path(
    table_dir,
    "instructor_analysis_session_info.txt"
  )
)
