# ==============================================================================
# Learner Experience Survey: Phase 3
# Descriptive analysis
# ==============================================================================

rm(list = ls())
graphics.off()
options(scipen = 999)

library(tidyverse)
library(here)

here::i_am("scripts/03_descriptive_learner.R")

learner <- readRDS(
  here(
    "data_processed",
    "learner_analysis_phase2_recoded.rds"
  )
)

cat("Rows:", nrow(learner), "\n")
cat("Columns:", ncol(learner), "\n")

stopifnot(nrow(learner) == 170)


## review variable names
cat("\nVariable names:\n")

print(
  names(learner)
)

## review the dataset structure
cat("\nDataset structure:\n")

glimpse(learner)

## Factor-level audit
variables_to_audit <- c(
  "teaching_experience",
  "gender",
  "age_group",
  "first_submission_mastery",
  "rubric_clear_aligned",
  "feedback_specific_relevant",
  "feedback_helped_improve",
  "feedback_timely",
  "overall_feedback_quality",
  "prior_ai_experience",
  "ai_awareness",
  "ai_comfort",
  "ai_as_useful_as_human",
  "preferred_feedback_model",
  "duration_flag"
)

factor_audit <- purrr::map_dfr(
  variables_to_audit,
  function(variable_name) {
    
    x <- learner[[variable_name]]
    
    tibble(
      variable = variable_name,
      
      variable_class = paste(
        class(x),
        collapse = ", "
      ),
      
      ordered_factor = is.ordered(x),
      
      factor_levels = if (is.factor(x)) {
        paste(
          levels(x),
          collapse = " | "
        )
      } else {
        NA_character_
      },
      
      missing_n = sum(is.na(x))
    )
  }
)

print(
  factor_audit,
  n = Inf,
  width = Inf
)

## Check the most important recoded variables
learner |>
  count(
    rubric_clear_aligned,
    .drop = FALSE
  )

learner |>
  count(
    overall_feedback_quality,
    .drop = FALSE
  )

learner |>
  count(
    ai_comfort,
    .drop = FALSE
  )

learner |>
  count(
    ai_as_useful_as_human,
    .drop = FALSE
  )


# ==============================================================================
# Phase 3, Step 2: Audit remaining categorical variables
# ==============================================================================

remaining_categorical_variables <- c(
  "teaching_experience",
  "gender",
  "age_group",
  "prior_ai_experience",
  "ai_awareness",
  "preferred_feedback_model",
  "duration_flag"
)

remaining_category_audit <- purrr::map_dfr(
  remaining_categorical_variables,
  function(variable_name) {
    
    learner |>
      transmute(
        response = .data[[variable_name]]
      ) |>
      count(
        response,
        name = "n",
        .drop = FALSE
      ) |>
      mutate(
        variable = variable_name,
        valid_n = sum(
          n[!is.na(response)]
        ),
        percent = if_else(
          !is.na(response),
          round(
            100 * n / valid_n,
            1
          ),
          NA_real_
        ),
        .before = 1
      )
  }
)

print(
  remaining_category_audit,
  n = Inf,
  width = Inf
)

## Save the audit
quality_dir <- here::here(
  "output",
  "quality_checks"
)

dir.create(
  quality_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  remaining_category_audit,
  file.path(
    quality_dir,
    "learner_phase3_remaining_category_audit.csv"
  ),
  na = ""
)

## missingness check
remaining_missingness <- learner |>
  summarise(
    across(
      all_of(
        remaining_categorical_variables
      ),
      ~ sum(is.na(.x))
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_n"
  )

print(
  remaining_missingness,
  n = Inf
)

# ==============================================================================
# Phase 3, Step 3: Recode remaining categorical variables
# ==============================================================================

learner_desc <- learner |>
  mutate(
    # Teaching experience: ordered from least to most experience
    teaching_experience = factor(
      teaching_experience,
      levels = c(
        "0-3 years",
        "4-10 years",
        "11-20 years",
        "20+ years"
      ),
      ordered = TRUE
    ),
    
    # Gender: nominal variable
    gender = factor(
      gender,
      levels = c(
        "Female",
        "Male",
        "Prefer not to say"
      )
    ),
    
    # Age: ordered for display, but "Prefer not to say" is not ordinal
    age_group = factor(
      age_group,
      levels = c(
        "<30",
        "31-40",
        "41-50",
        "50+",
        "Prefer not to say"
      )
    ),
    
    # AI experience: ordered from no experience to extensive experience
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
    
    # Awareness: ordered from no awareness to full awareness
    ai_awareness = factor(
      ai_awareness,
      levels = c(
        "Not aware at all",
        "Somewhat aware",
        "Yes, fully aware"
      ),
      ordered = TRUE
    ),
    
    # Duration flag: quality-control variable, not a substantive scale
    duration_flag = factor(
      duration_flag,
      levels = c(
        "Under 1 minute",
        "1 to under 2 minutes",
        "Not flagged",
        "Over 1 hour"
      )
    )
  )

## ordinal age variable
learner_desc <- learner_desc |>
  mutate(
    age_group_ordered = case_when(
      as.character(age_group) == "<30" ~ "<30",
      as.character(age_group) == "31-40" ~ "31-40",
      as.character(age_group) == "41-50" ~ "41-50",
      as.character(age_group) == "50+" ~ "50+",
      TRUE ~ NA_character_
    ),
    
    age_group_ordered = factor(
      age_group_ordered,
      levels = c(
        "<30",
        "31-40",
        "41-50",
        "50+"
      ),
      ordered = TRUE
    )
  )

##Create a shorter preferred-model variable
learner_desc <- learner_desc |>
  mutate(
    preferred_model_short = case_when(
      str_starts(
        preferred_feedback_model,
        "Human instructor grades everything"
      ) ~ "Human grades; AI supports",
      
      str_starts(
        preferred_feedback_model,
        "AI provides initial feedback"
      ) ~ "AI drafts; human reviews every submission",
      
      str_starts(
        preferred_feedback_model,
        "AI provides all feedback"
      ) ~ "AI provides feedback; human reviews disputes",
      
      preferred_feedback_model == "I have no preference." ~
        "No preference",
      
      is.na(preferred_feedback_model) ~ NA_character_,
      
      TRUE ~ "Unclassified response"
    ),
    
    preferred_model_short = factor(
      preferred_model_short,
      levels = c(
        "Human grades; AI supports",
        "AI drafts; human reviews every submission",
        "AI provides feedback; human reviews disputes",
        "No preference"
      )
    )
  )

##Check that no response was accidentally classified as unrecognized
learner_desc |>
  count(
    preferred_model_short,
    .drop = FALSE
  ) |>
  print(n = Inf)

sum(
  as.character(
    learner_desc$preferred_model_short
  ) == "Unclassified response",
  na.rm = TRUE
)


# ==============================================================================
# Phase 3, Step 4A: Save descriptive-ready dataset
# ==============================================================================

descriptive_ready_file <- here::here(
  "data_processed",
  "learner_analysis_phase3_descriptive_ready.rds"
)

if (!file.exists(descriptive_ready_file)) {
  saveRDS(
    learner_desc,
    descriptive_ready_file
  )
  
  message(
    "Saved descriptive-ready dataset: ",
    descriptive_ready_file
  )
} else {
  message(
    "File already exists and was not overwritten: ",
    descriptive_ready_file
  )
}

## Step 4B: Create the descriptive output folder
learner_descriptive_dir <- here::here(
  "output",
  "learner_descriptive"
)

dir.create(
  learner_descriptive_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.exists(learner_descriptive_dir)

### Step 4C: Define the participant-characteristic variables
profile_variable_labels <- c(
  teaching_experience =
    "Teaching experience",
  
  gender =
    "Gender",
  
  age_group =
    "Age group",
  
  first_submission_mastery =
    "Achieved mastery on first submission",
  
  prior_ai_experience =
    "Prior AI experience",
  
  ai_awareness =
    "Awareness that AI supported feedback"
)

### Step 4D: Create the participant-characteristics table
learner_profile_table <- purrr::imap_dfr(
  profile_variable_labels,
  function(display_label, variable_name) {
    
    learner_desc |>
      transmute(
        response = as.character(
          .data[[variable_name]]
        )
      ) |>
      count(
        response,
        name = "n",
        .drop = FALSE
      ) |>
      mutate(
        variable =
          variable_name,
        
        characteristic =
          display_label,
        
        valid_n = sum(
          n[!is.na(response)]
        ),
        
        percent = if_else(
          !is.na(response),
          round(
            100 * n / valid_n,
            1
          ),
          NA_real_
        )
      ) |>
      select(
        variable,
        characteristic,
        response,
        n,
        valid_n,
        percent
      )
  }
)

print(
  learner_profile_table,
  n = Inf,
  width = Inf
)

### Step 4E: Confirm that percentages equal 100%
profile_percent_check <- learner_profile_table |>
  filter(
    !is.na(response)
  ) |>
  group_by(
    variable,
    characteristic
  ) |>
  summarise(
    total_n = sum(n),
    percent_sum = sum(percent),
    .groups = "drop"
  )

print(
  profile_percent_check,
  n = Inf
)

### Step 4F: Create an n (%) reporting column
learner_profile_reporting <- learner_profile_table |>
  filter(
    !is.na(response)
  ) |>
  mutate(
    n_percent = paste0(
      n,
      " (",
      sprintf("%.1f", percent),
      "%)"
    )
  ) |>
  select(
    characteristic,
    response,
    n,
    percent,
    n_percent
  )

print(
  learner_profile_reporting,
  n = Inf,
  width = Inf
)



### Step 4G: Save the table 
profile_output_file <- file.path(
  learner_descriptive_dir,
  "learner_participant_characteristics.csv"
)

if (!file.exists(profile_output_file)) {
  
  readr::write_csv(
    learner_profile_reporting,
    profile_output_file,
    na = ""
  )
  
  message(
    "Saved participant-characteristics table: ",
    profile_output_file
  )
  
} else {
  
  message(
    "File already exists and was not overwritten: ",
    profile_output_file
  )
}

###Step 4H: Correct the category order
profile_response_order <- c(
  # Teaching experience
  "0-3 years",
  "4-10 years",
  "11-20 years",
  "20+ years",
  
  # Gender
  "Female",
  "Male",
  "Prefer not to say",
  
  # Age
  "<30",
  "31-40",
  "41-50",
  "50+",
  
  # Mastery
  "No, I didn't.",
  "Unsure",
  "Yes, I did.",
  
  # AI experience
  "No experience",
  "Minimal experience",
  "Some experience",
  "Extensive experience",
  
  # AI awareness
  "Not aware at all",
  "Somewhat aware",
  "Yes, fully aware"
)

characteristic_order <- c(
  "Teaching experience",
  "Gender",
  "Age group",
  "Achieved mastery on first submission",
  "Prior AI experience",
  "Awareness that AI supported feedback"
)

learner_profile_reporting <- learner_profile_reporting |>
  mutate(
    characteristic = factor(
      characteristic,
      levels = characteristic_order
    ),
    
    response_order = match(
      response,
      profile_response_order
    )
  ) |>
  arrange(
    characteristic,
    response_order
  ) |>
  select(
    -response_order
  )

print(
  learner_profile_reporting,
  n = Inf,
  width = Inf
)

### Step 5A: Define variables and labels
feedback_items <- c(
  "rubric_clear_aligned",
  "feedback_specific_relevant",
  "feedback_helped_improve",
  "feedback_timely"
)

feedback_item_labels <- c(
  rubric_clear_aligned =
    "Rubric was clear and aligned",
  
  feedback_specific_relevant =
    "Feedback was specific and relevant",
  
  feedback_helped_improve =
    "Feedback identified strengths and improvements",
  
  feedback_timely =
    "Feedback was timely"
)

likert_levels <- c(
  "Strongly disagree",
  "Somewhat disagree",
  "Neither agree nor disagree",
  "Somewhat agree",
  "Strongly agree"
)


### Step 5B: Convert the four items to long format
feedback_long <- learner_desc |>
  select(
    case_id,
    all_of(feedback_items)
  ) |>
  pivot_longer(
    cols = all_of(feedback_items),
    names_to = "item",
    values_to = "response"
  ) |>
  mutate(
    item = factor(
      item,
      levels = feedback_items,
      labels = unname(
        feedback_item_labels[feedback_items]
      )
    ),
    
    response = factor(
      as.character(response),
      levels = likert_levels,
      ordered = TRUE
    )
  )


### Step 5C: Calculate frequencies and percentages
feedback_distribution <- feedback_long |>
  count(
    item,
    response,
    name = "n",
    .drop = FALSE
  ) |>
  group_by(item) |>
  mutate(
    valid_n = sum(n),
    
    percent = round(
      100 * n / valid_n,
      1
    )
  ) |>
  ungroup()

print(
  feedback_distribution,
  n = Inf,
  width = Inf
)

### Step 5E: Calculate favorable responses
feedback_favorable <- feedback_long |>
  group_by(item) |>
  summarise(
    valid_n = sum(
      !is.na(response)
    ),
    
    strongly_agree_n = sum(
      response == "Strongly agree",
      na.rm = TRUE
    ),
    
    strongly_agree_percent = round(
      100 * strongly_agree_n / valid_n,
      1
    ),
    
    somewhat_agree_n = sum(
      response == "Somewhat agree",
      na.rm = TRUE
    ),
    
    favorable_n = sum(
      response %in% c(
        "Somewhat agree",
        "Strongly agree"
      ),
      na.rm = TRUE
    ),
    
    favorable_percent = round(
      100 * favorable_n / valid_n,
      1
    ),
    
    neutral_n = sum(
      response == "Neither agree nor disagree",
      na.rm = TRUE
    ),
    
    unfavorable_n = sum(
      response %in% c(
        "Somewhat disagree",
        "Strongly disagree"
      ),
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

print(
  feedback_favorable,
  n = Inf,
  width = Inf
)


### Step 5F: Save these tables 
feedback_distribution_file <- file.path(
  learner_descriptive_dir,
  "learner_feedback_item_distributions.csv"
)

feedback_favorable_file <- file.path(
  learner_descriptive_dir,
  "learner_feedback_favorable_percentages.csv"
)

if (!file.exists(feedback_distribution_file)) {
  readr::write_csv(
    feedback_distribution,
    feedback_distribution_file,
    na = ""
  )
} else {
  message(
    "Existing file was not overwritten: ",
    feedback_distribution_file
  )
}

if (!file.exists(feedback_favorable_file)) {
  readr::write_csv(
    feedback_favorable,
    feedback_favorable_file,
    na = ""
  )
} else {
  message(
    "Existing file was not overwritten: ",
    feedback_favorable_file
  )
}


# ==============================================================================
# Phase 3, Step 6: Overall feedback quality
# ==============================================================================

quality_levels <- c(
  "Poor",
  "Fair",
  "Good",
  "Excellent"
)

quality_distribution <- learner_desc |>
  transmute(
    response = factor(
      as.character(
        overall_feedback_quality
      ),
      levels = quality_levels,
      ordered = TRUE
    )
  ) |>
  count(
    response,
    name = "n",
    .drop = FALSE
  ) |>
  mutate(
    valid_n = sum(n),
    
    percent = round(
      100 * n / valid_n,
      1
    )
  )

print(
  quality_distribution,
  n = Inf,
  width = Inf
)

### Step 6B: Create the quality summary
quality_summary <- learner_desc |>
  summarise(
    valid_n = sum(
      !is.na(overall_feedback_quality)
    ),
    
    poor_n = sum(
      overall_feedback_quality == "Poor",
      na.rm = TRUE
    ),
    
    fair_n = sum(
      overall_feedback_quality == "Fair",
      na.rm = TRUE
    ),
    
    good_n = sum(
      overall_feedback_quality == "Good",
      na.rm = TRUE
    ),
    
    excellent_n = sum(
      overall_feedback_quality == "Excellent",
      na.rm = TRUE
    ),
    
    excellent_percent = round(
      100 * excellent_n / valid_n,
      1
    ),
    
    favorable_n = sum(
      overall_feedback_quality %in%
        c(
          "Good",
          "Excellent"
        ),
      na.rm = TRUE
    ),
    
    favorable_percent = round(
      100 * favorable_n / valid_n,
      1
    )
  )

print(
  quality_summary,
  n = Inf,
  width = Inf
)


### Step 6C: Check the percentage total
quality_check <- quality_distribution |>
  summarise(
    total_n = sum(n),
    percent_sum = sum(percent)
  )

print(quality_check)


### Step 6D: Save
quality_distribution_file <- file.path(
  learner_descriptive_dir,
  "learner_overall_feedback_quality_distribution.csv"
)

quality_summary_file <- file.path(
  learner_descriptive_dir,
  "learner_overall_feedback_quality_summary.csv"
)

if (!file.exists(quality_distribution_file)) {
  
  readr::write_csv(
    quality_distribution,
    quality_distribution_file,
    na = ""
  )
  
  message(
    "Saved: ",
    quality_distribution_file
  )
  
} else {
  
  message(
    "Existing file was not overwritten: ",
    quality_distribution_file
  )
}

if (!file.exists(quality_summary_file)) {
  
  readr::write_csv(
    quality_summary,
    quality_summary_file,
    na = ""
  )
  
  message(
    "Saved: ",
    quality_summary_file
  )
  
} else {
  
  message(
    "Existing file was not overwritten: ",
    quality_summary_file
  )
}


# ==============================================================================
# Phase 3, Step 7: Learner perceptions of AI-supported feedback
# ==============================================================================

ai_items <- c(
  "ai_comfort",
  "ai_as_useful_as_human"
)

ai_item_labels <- c(
  ai_comfort =
    "Comfortable with AI-supported feedback",
  
  ai_as_useful_as_human =
    "AI feedback can be as useful as human feedback"
)

### Step 7B: Convert the AI items to long format

ai_long <- learner_desc |>
  select(
    case_id,
    all_of(ai_items)
  ) |>
  pivot_longer(
    cols = all_of(ai_items),
    names_to = "item",
    values_to = "response"
  ) |>
  mutate(
    item = factor(
      item,
      levels = ai_items,
      labels = unname(
        ai_item_labels[ai_items]
      )
    ),
    
    response = factor(
      as.character(response),
      levels = likert_levels,
      ordered = TRUE
    )
  )

### Step 7C: Calculate the full response distributions
ai_distribution <- ai_long |>
  count(
    item,
    response,
    name = "n",
    .drop = FALSE
  ) |>
  group_by(item) |>
  mutate(
    valid_n = sum(n),
    
    percent = round(
      100 * n / valid_n,
      1
    )
  ) |>
  ungroup()

print(
  ai_distribution,
  n = Inf,
  width = Inf
)

### Step 7E: Calculate favorable and unfavorable responses
ai_favorable <- ai_long |>
  group_by(item) |>
  summarise(
    valid_n = sum(
      !is.na(response)
    ),
    
    strongly_agree_n = sum(
      response == "Strongly agree",
      na.rm = TRUE
    ),
    
    strongly_agree_percent = round(
      100 * strongly_agree_n / valid_n,
      1
    ),
    
    somewhat_agree_n = sum(
      response == "Somewhat agree",
      na.rm = TRUE
    ),
    
    favorable_n = sum(
      response %in% c(
        "Somewhat agree",
        "Strongly agree"
      ),
      na.rm = TRUE
    ),
    
    favorable_percent = round(
      100 * favorable_n / valid_n,
      1
    ),
    
    neutral_n = sum(
      response == "Neither agree nor disagree",
      na.rm = TRUE
    ),
    
    neutral_percent = round(
      100 * neutral_n / valid_n,
      1
    ),
    
    unfavorable_n = sum(
      response %in% c(
        "Somewhat disagree",
        "Strongly disagree"
      ),
      na.rm = TRUE
    ),
    
    unfavorable_percent = round(
      100 * unfavorable_n / valid_n,
      1
    ),
    
    .groups = "drop"
  )

print(
  ai_favorable,
  n = Inf,
  width = Inf
)

### Step 7F: Save the AI tables
ai_distribution_file <- file.path(
  learner_descriptive_dir,
  "learner_ai_item_distributions.csv"
)

ai_favorable_file <- file.path(
  learner_descriptive_dir,
  "learner_ai_favorable_percentages.csv"
)

if (!file.exists(ai_distribution_file)) {
  
  readr::write_csv(
    ai_distribution,
    ai_distribution_file,
    na = ""
  )
  
  message(
    "Saved: ",
    ai_distribution_file
  )
  
} else {
  
  message(
    "Existing file was not overwritten: ",
    ai_distribution_file
  )
}

if (!file.exists(ai_favorable_file)) {
  
  readr::write_csv(
    ai_favorable,
    ai_favorable_file,
    na = ""
  )
  
  message(
    "Saved: ",
    ai_favorable_file
  )
  
} else {
  
  message(
    "Existing file was not overwritten: ",
    ai_favorable_file
  )
}


# ==============================================================================
# Phase 3, Step 8: Preferred feedback model
# ==============================================================================

preferred_model_distribution <- learner_desc |>
  transmute(
    preferred_model =
      preferred_model_short
  ) |>
  count(
    preferred_model,
    name = "n",
    .drop = FALSE
  ) |>
  mutate(
    valid_n = sum(
      n[!is.na(preferred_model)]
    ),
    
    percent = if_else(
      !is.na(preferred_model),
      round(
        100 * n / valid_n,
        1
      ),
      NA_real_
    )
  )

print(
  preferred_model_distribution,
  n = Inf,
  width = Inf
)


### Step 8B: Create a human-oversight summary
preferred_model_summary <- learner_desc |>
  summarise(
    valid_n = sum(
      !is.na(preferred_model_short)
    ),
    
    human_grades_ai_supports_n = sum(
      preferred_model_short ==
        "Human grades; AI supports",
      na.rm = TRUE
    ),
    
    ai_drafts_human_reviews_n = sum(
      preferred_model_short ==
        "AI drafts; human reviews every submission",
      na.rm = TRUE
    ),
    
    selective_human_review_n = sum(
      preferred_model_short ==
        "AI provides feedback; human reviews disputes",
      na.rm = TRUE
    ),
    
    no_preference_n = sum(
      preferred_model_short ==
        "No preference",
      na.rm = TRUE
    ),
    
    universal_human_review_n =
      human_grades_ai_supports_n +
      ai_drafts_human_reviews_n,
    
    universal_human_review_percent = round(
      100 *
        universal_human_review_n /
        valid_n,
      1
    ),
    
    any_explicit_human_role_n =
      human_grades_ai_supports_n +
      ai_drafts_human_reviews_n +
      selective_human_review_n,
    
    any_explicit_human_role_percent = round(
      100 *
        any_explicit_human_role_n /
        valid_n,
      1
    )
  )

print(
  preferred_model_summary,
  n = Inf,
  width = Inf
)

### Step 8C: Create reporting labels
preferred_model_reporting <-
  preferred_model_distribution |>
  filter(
    !is.na(preferred_model)
  ) |>
  mutate(
    n_percent = paste0(
      n,
      " (",
      sprintf("%.1f", percent),
      "%)"
    )
  )

print(
  preferred_model_reporting,
  n = Inf,
  width = Inf
)


### Step 8E: Save
preferred_model_file <- file.path(
  learner_descriptive_dir,
  "learner_preferred_feedback_model.csv"
)

preferred_model_summary_file <- file.path(
  learner_descriptive_dir,
  "learner_preferred_feedback_model_summary.csv"
)

if (!file.exists(preferred_model_file)) {
  
  readr::write_csv(
    preferred_model_reporting,
    preferred_model_file,
    na = ""
  )
  
  message(
    "Saved: ",
    preferred_model_file
  )
  
} else {
  
  message(
    "Existing file was not overwritten: ",
    preferred_model_file
  )
}

if (!file.exists(preferred_model_summary_file)) {
  
  readr::write_csv(
    preferred_model_summary,
    preferred_model_summary_file,
    na = ""
  )
  
  message(
    "Saved: ",
    preferred_model_summary_file
  )
  
} else {
  
  message(
    "Existing file was not overwritten: ",
    preferred_model_summary_file
  )
}


# ==============================================================================
# Phase 3, Step 9: Preferred feedback model figure
# ==============================================================================

preferred_model_plot_data <- preferred_model_reporting |>
  mutate(
    plot_label = case_when(
      preferred_model ==
        "Human grades; AI supports" ~
        "Human grades;\nAI supports",
      
      preferred_model ==
        "AI drafts; human reviews every submission" ~
        "AI drafts feedback;\nhuman reviews every submission",
      
      preferred_model ==
        "AI provides feedback; human reviews disputes" ~
        "AI provides feedback;\nhuman reviews disputes",
      
      preferred_model ==
        "No preference" ~
        "No preference",
      
      TRUE ~ as.character(preferred_model)
    ),
    
    plot_label = factor(
      plot_label,
      levels = rev(c(
        "Human grades;\nAI supports",
        "AI drafts feedback;\nhuman reviews every submission",
        "AI provides feedback;\nhuman reviews disputes",
        "No preference"
      ))
    ),
    
    uf_fill = factor(
      c("Blue", "Orange", "Blue", "Orange"),
      levels = c("Blue", "Orange")
    )
  )

### Plot
print(
  preferred_model_plot_data,
  n = Inf,
  width = Inf
)


### bar chart
preferred_model_plot <- ggplot(
  preferred_model_plot_data,
  aes(
    x = plot_label,
    y = percent,
    fill = uf_fill
  )
) +
  geom_col(
    width = 0.7
  ) +
  geom_text(
    aes(label = n_percent),
    hjust = -0.1,
    size = 3.8,
    color = "black"
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Blue" = "#0021A5",
      "Orange" = "#FA4616"
    ),
    guide = "none"
  ) +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, 10),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Learners’ Preferred Assessment and Feedback Model",
    subtitle = "Valid responses: n = 169",
    x = NULL,
    y = "Percentage of learners"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 30, 10, 10)
  )

print(preferred_model_plot)

### Save
preferred_model_pdf_file <- file.path(
  learner_descriptive_dir,
  "learner_preferred_feedback_model_plot.pdf"
)

if (!file.exists(preferred_model_pdf_file)) {
  
  ggsave(
    filename =
      preferred_model_pdf_file,
    
    plot =
      preferred_model_plot,
    
    width = 10,
    height = 6
  )
  
  message(
    "Saved: ",
    preferred_model_pdf_file
  )
  
} else {
  
  message(
    "Existing figure was not overwritten: ",
    preferred_model_pdf_file
  )
}

### Step 10: Feedback experience Likert plot
uf_likert_colors <- c(
  "Strongly disagree" = "#C44E1A",
  "Somewhat disagree" = "#FA4616",
  "Neither agree nor disagree" = "#B7B7B7",
  "Somewhat agree" = "#6C8AE4",
  "Strongly agree" = "#0021A5"
)


### Create unrounded plotting data
feedback_plot_data <- feedback_long |>
  count(
    item,
    response,
    name = "n",
    .drop = FALSE
  ) |>
  group_by(item) |>
  mutate(
    valid_n = sum(n),
    
    # Keep full precision for plotting
    percent_plot = 100 * n / valid_n
  ) |>
  ungroup() |>
  mutate(
    item = forcats::fct_rev(item)
  )


### 10B. Create the feedback plot
feedback_likert_plot_final <- ggplot(
  feedback_plot_data,
  aes(
    x = item,
    y = percent_plot,
    fill = response
  )
) +
  geom_col(
    width = 0.75,
    color = "white",
    linewidth = 0.2,
    position = position_stack(reverse = TRUE)
  ) +
  coord_flip() +
  scale_fill_manual(
    values = uf_likert_colors,
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.005))
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  labs(
    title = "Learner Ratings of the Assessment and Feedback Process",
    subtitle = "Valid responses: n = 170 per item",
    x = NULL,
    y = "Percentage of learners",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 20, 20, 10)
  )

print(feedback_likert_plot_final)


### Unrounded AI plotting data
ai_plot_data <- ai_long |>
  count(
    item,
    response,
    name = "n",
    .drop = FALSE
  ) |>
  group_by(item) |>
  mutate(
    valid_n = sum(n),
    
    # Keep full precision for plotting
    percent_plot = 100 * n / valid_n
  ) |>
  ungroup() |>
  mutate(
    item = forcats::fct_rev(item)
  )




### 11A, Create the AI plot
ai_likert_plot_final <- ggplot(
  ai_plot_data,
  aes(
    x = item,
    y = percent_plot,
    fill = response
  )
) +
  geom_col(
    width = 0.75,
    color = "white",
    linewidth = 0.2,
    position = position_stack(reverse = TRUE)
  ) +
  coord_flip() +
  scale_fill_manual(
    values = uf_likert_colors,
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.005))
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  labs(
    title = "Learner Perceptions of AI-Supported Feedback",
    subtitle = "Valid responses: n = 170 per item",
    x = NULL,
    y = "Percentage of learners",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 20, 20, 10)
  )

print(ai_likert_plot_final)


### Save
ggsave(
  filename = file.path(
    learner_descriptive_dir,
    "learner_feedback_likert_plot_corrected.png"
  ),
  plot = feedback_likert_plot_final,
  width = 12,
  height = 6.5,
  dpi = 300
)

ggsave(
  filename = file.path(
    learner_descriptive_dir,
    "learner_ai_likert_plot_corrected.png"
  ),
  plot = ai_likert_plot_final,
  width = 12,
  height = 5.5,
  dpi = 300
)
