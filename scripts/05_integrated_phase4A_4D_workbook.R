# ==============================================================================
# PHASE 5: INTEGRATE PHASES 4A-4D
# Learner Assessment and Feedback Survey
#
# BLOCK 1: LOCATE AND VERIFY SAVED ANALYSIS OUTPUTS
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. START CLEAN AND LOAD PACKAGES
# ------------------------------------------------------------------------------

rm(list = ls())
graphics.off()
options(scipen = 999)

library(tidyverse)
library(here)

here::i_am(
  "scripts/05_integrated_phase4A_4D_workbook.R"
)

cat(
  "\nProject root:\n",
  here(),
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 1. DEFINE EXPECTED PHASE OUTPUT DIRECTORIES
# ------------------------------------------------------------------------------

phase_output_directories <- tibble(
  phase = c(
    "Phase 4A",
    "Phase 4B",
    "Phase 4C",
    "Phase 4D"
  ),
  
  expected_directory = c(
    here(
      "output",
      "learner_scale_diagnostics"
    ),
    
    here(
      "output",
      "learner_ai_comfort_ordinal"
    ),
    
    here(
      "output",
      "learner_ai_usefulness_ordinal"
    ),
    
    here(
      "output",
      "learner_preferred_model_multinomial"
    )
  )
) |>
  mutate(
    directory_exists = dir.exists(
      expected_directory
    )
  )


# ------------------------------------------------------------------------------
# 2. PRINT DIRECTORY CHECK
# ------------------------------------------------------------------------------

cat(
  "PHASE OUTPUT DIRECTORY CHECK\n"
)

print(
  phase_output_directories,
  n = Inf,
  width = Inf
)

missing_directories <- phase_output_directories |>
  filter(
    !directory_exists
  )

if (nrow(missing_directories) > 0) {
  warning(
    paste0(
      "\nThe following expected phase directories were not found:\n",
      paste(
        missing_directories$expected_directory,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 3. LIST ALL RELEVANT SAVED FILES
# ------------------------------------------------------------------------------

phase_saved_files <- phase_output_directories |>
  filter(
    directory_exists
  ) |>
  select(
    phase,
    expected_directory
  ) |>
  purrr::pmap_dfr(
    function(
    phase,
    expected_directory
    ) {
      discovered_files <- list.files(
        path = expected_directory,
        recursive = TRUE,
        full.names = TRUE
      )
      
      if (length(discovered_files) == 0) {
        return(
          tibble(
            phase = phase,
            file_name = NA_character_,
            file_extension = NA_character_,
            full_path = NA_character_,
            file_size_kb = NA_real_
          )
        )
      }
      
      tibble(
        phase = phase,
        full_path = discovered_files
      ) |>
        mutate(
          file_name = basename(
            full_path
          ),
          
          file_extension = tools::file_ext(
            full_path
          ) |>
            str_to_lower(),
          
          file_size_kb = file.info(
            full_path
          )$size / 1024
        ) |>
        select(
          phase,
          file_name,
          file_extension,
          file_size_kb,
          full_path
        )
    }
  ) |>
  filter(
    file_extension %in% c(
      "rds",
      "csv",
      "xlsx",
      "txt",
      "png",
      "pdf"
    )
  ) |>
  arrange(
    phase,
    file_extension,
    file_name
  )


# ------------------------------------------------------------------------------
# 4. PRINT RDS CHECKPOINT CANDIDATES
# ------------------------------------------------------------------------------

phase_rds_candidates <- phase_saved_files |>
  filter(
    file_extension == "rds"
  )

cat(
  "\nRDS CHECKPOINT AND MODEL FILES\n"
)

print(
  phase_rds_candidates |>
    select(
      phase,
      file_name,
      file_size_kb,
      full_path
    ),
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 5. PRINT TABLE FILE CANDIDATES
# ------------------------------------------------------------------------------

phase_table_candidates <- phase_saved_files |>
  filter(
    file_extension %in% c(
      "csv",
      "xlsx"
    )
  )

cat(
  "\nCSV AND EXCEL TABLE FILES\n"
)

print(
  phase_table_candidates |>
    select(
      phase,
      file_name,
      file_size_kb,
      full_path
    ),
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 6. PRINT FIGURE CANDIDATES
# ------------------------------------------------------------------------------

phase_figure_candidates <- phase_saved_files |>
  filter(
    file_extension %in% c(
      "png",
      "pdf"
    )
  )

cat(
  "\nFIGURE FILES\n"
)

print(
  phase_figure_candidates |>
    select(
      phase,
      file_name,
      file_extension,
      file_size_kb,
      full_path
    ),
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 7. CREATE A COMPACT FILE COUNT SUMMARY
# ------------------------------------------------------------------------------

phase_file_count_summary <- phase_saved_files |>
  count(
    phase,
    file_extension,
    name = "number_of_files"
  ) |>
  arrange(
    phase,
    file_extension
  )

cat(
  "\nFILE COUNT SUMMARY\n"
)

print(
  phase_file_count_summary,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 8. IDENTIFY LIKELY ANALYSIS CHECKPOINTS
# ------------------------------------------------------------------------------

likely_checkpoint_files <- phase_rds_candidates |>
  filter(
    str_detect(
      file_name,
      regex(
        paste(
          c(
            "checkpoint",
            "analysis_objects",
            "final_analysis",
            "complete_cases",
            "with_composites"
          ),
          collapse = "|"
        ),
        ignore_case = TRUE
      )
    )
  )

cat(
  "\nLIKELY ANALYSIS CHECKPOINTS\n"
)

print(
  likely_checkpoint_files |>
    select(
      phase,
      file_name,
      full_path
    ),
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 9. COMPLETION MESSAGE
# ------------------------------------------------------------------------------

cat(
  "\n",
  "PHASE 5 BLOCK 1 COMPLETE\n",
  "Return the printed sections titled:\n",
  "  1. PHASE OUTPUT DIRECTORY CHECK\n",
  "  2. RDS CHECKPOINT AND MODEL FILES\n",
  "  3. LIKELY ANALYSIS CHECKPOINTS\n",
  "\n",
  "The next block will read the verified checkpoint files and inspect ",
  "the object names stored inside each one.\n",
  sep = ""
)


# ==============================================================================
# PHASE 5, BLOCK 2: READ AND INSPECT AUTHORITATIVE CHECKPOINTS
# ==============================================================================


# ------------------------------------------------------------------------------
# 10. DEFINE AUTHORITATIVE CHECKPOINT PATHS
# ------------------------------------------------------------------------------

phase5_source_paths <- tibble(
  source_name = c(
    "Phase 4A analysis objects",
    "Phase 4A analysis data",
    "Phase 4B final analysis objects",
    "Phase 4C final checkpoint",
    "Phase 4C complete-case data",
    "Phase 4D final checkpoint",
    "Phase 4D complete-case data"
  ),
  
  source_path = c(
    here(
      "output",
      "learner_scale_diagnostics",
      "phase4A_analysis_objects.rds"
    ),
    
    here(
      "output",
      "learner_scale_diagnostics",
      "learner_analysis_phase4A_with_composites.rds"
    ),
    
    here(
      "output",
      "learner_ai_comfort_ordinal",
      "final",
      "phase4B_final_analysis_objects.rds"
    ),
    
    here(
      "output",
      "learner_ai_usefulness_ordinal",
      "phase4C_final_analysis_checkpoint.rds"
    ),
    
    here(
      "output",
      "learner_ai_usefulness_ordinal",
      "learner_analysis_phase4C_complete_cases.rds"
    ),
    
    here(
      "output",
      "learner_preferred_model_multinomial",
      "phase4D_final_analysis_checkpoint.rds"
    ),
    
    here(
      "output",
      "learner_preferred_model_multinomial",
      "learner_analysis_phase4D_complete_cases.rds"
    )
  )
) |>
  mutate(
    file_exists = file.exists(
      source_path
    ),
    
    file_size_kb = if_else(
      file_exists,
      file.info(
        source_path
      )$size / 1024,
      NA_real_
    )
  )


# ------------------------------------------------------------------------------
# 11. VERIFY SOURCE FILES
# ------------------------------------------------------------------------------

cat(
  "\nAUTHORITATIVE SOURCE FILE CHECK\n"
)

print(
  phase5_source_paths,
  n = Inf,
  width = Inf
)

missing_source_files <- phase5_source_paths |>
  filter(
    !file_exists
  )

if (nrow(missing_source_files) > 0) {
  stop(
    paste0(
      "The following required Phase 5 source files were not found:\n",
      paste(
        missing_source_files$source_path,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 12. READ THE CHECKPOINTS AND SUPPORTING DATA
# ------------------------------------------------------------------------------

phase4A_objects <- readRDS(
  phase5_source_paths$source_path[
    phase5_source_paths$source_name ==
      "Phase 4A analysis objects"
  ]
)

phase4A_data <- readRDS(
  phase5_source_paths$source_path[
    phase5_source_paths$source_name ==
      "Phase 4A analysis data"
  ]
)

phase4B_objects <- readRDS(
  phase5_source_paths$source_path[
    phase5_source_paths$source_name ==
      "Phase 4B final analysis objects"
  ]
)

phase4C_objects <- readRDS(
  phase5_source_paths$source_path[
    phase5_source_paths$source_name ==
      "Phase 4C final checkpoint"
  ]
)

phase4C_data <- readRDS(
  phase5_source_paths$source_path[
    phase5_source_paths$source_name ==
      "Phase 4C complete-case data"
  ]
)

phase4D_objects <- readRDS(
  phase5_source_paths$source_path[
    phase5_source_paths$source_name ==
      "Phase 4D final checkpoint"
  ]
)

phase4D_data <- readRDS(
  phase5_source_paths$source_path[
    phase5_source_paths$source_name ==
      "Phase 4D complete-case data"
  ]
)


# ------------------------------------------------------------------------------
# 13. HELPER: SUMMARIZE TOP-LEVEL OBJECT CONTENTS
# ------------------------------------------------------------------------------

summarize_stored_object <- function(
    stored_object,
    stored_name
) {
  object_class <- paste(
    class(stored_object),
    collapse = ", "
  )
  
  object_rows <- if (
    is.data.frame(stored_object) ||
    is.matrix(stored_object)
  ) {
    nrow(stored_object)
  } else {
    NA_integer_
  }
  
  object_columns <- if (
    is.data.frame(stored_object) ||
    is.matrix(stored_object)
  ) {
    ncol(stored_object)
  } else {
    NA_integer_
  }
  
  object_length <- length(
    stored_object
  )
  
  tibble(
    object_name = stored_name,
    object_class = object_class,
    rows = object_rows,
    columns = object_columns,
    object_length = object_length
  )
}


inspect_checkpoint <- function(
    checkpoint_object,
    checkpoint_name
) {
  cat(
    "\n",
    checkpoint_name,
    "\n",
    strrep(
      "-",
      nchar(checkpoint_name)
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Top-level class: ",
    paste(
      class(checkpoint_object),
      collapse = ", "
    ),
    "\n",
    sep = ""
  )
  
  if (is.list(checkpoint_object)) {
    checkpoint_summary <- purrr::imap_dfr(
      checkpoint_object,
      ~ summarize_stored_object(
        stored_object = .x,
        stored_name = .y
      )
    )
    
    print(
      checkpoint_summary,
      n = Inf,
      width = Inf
    )
    
    return(
      checkpoint_summary
    )
  }
  
  checkpoint_summary <- summarize_stored_object(
    stored_object = checkpoint_object,
    stored_name = checkpoint_name
  )
  
  print(
    checkpoint_summary,
    n = Inf,
    width = Inf
  )
  
  checkpoint_summary
}


# ------------------------------------------------------------------------------
# 14. INSPECT THE FOUR ANALYSIS CHECKPOINTS
# ------------------------------------------------------------------------------

phase4A_object_inventory <- inspect_checkpoint(
  phase4A_objects,
  "PHASE 4A ANALYSIS OBJECTS"
)

phase4B_object_inventory <- inspect_checkpoint(
  phase4B_objects,
  "PHASE 4B FINAL ANALYSIS OBJECTS"
)

phase4C_object_inventory <- inspect_checkpoint(
  phase4C_objects,
  "PHASE 4C FINAL ANALYSIS CHECKPOINT"
)

phase4D_object_inventory <- inspect_checkpoint(
  phase4D_objects,
  "PHASE 4D FINAL ANALYSIS CHECKPOINT"
)


# ------------------------------------------------------------------------------
# 15. INSPECT SUPPORTING ANALYSIS DATASETS
# ------------------------------------------------------------------------------

phase5_dataset_inventory <- tibble(
  dataset = c(
    "Phase 4A analysis data",
    "Phase 4C complete-case data",
    "Phase 4D complete-case data"
  ),
  
  object_class = c(
    paste(
      class(phase4A_data),
      collapse = ", "
    ),
    
    paste(
      class(phase4C_data),
      collapse = ", "
    ),
    
    paste(
      class(phase4D_data),
      collapse = ", "
    )
  ),
  
  rows = c(
    nrow(phase4A_data),
    nrow(phase4C_data),
    nrow(phase4D_data)
  ),
  
  columns = c(
    ncol(phase4A_data),
    ncol(phase4C_data),
    ncol(phase4D_data)
  ),
  
  unique_case_ids = c(
    if (
      "case_id" %in% names(phase4A_data)
    ) {
      n_distinct(
        phase4A_data$case_id
      )
    } else {
      NA_integer_
    },
    
    if (
      "case_id" %in% names(phase4C_data)
    ) {
      n_distinct(
        phase4C_data$case_id
      )
    } else {
      NA_integer_
    },
    
    if (
      "case_id" %in% names(phase4D_data)
    ) {
      n_distinct(
        phase4D_data$case_id
      )
    } else {
      NA_integer_
    }
  )
)

cat(
  "\nSUPPORTING DATASET INVENTORY\n"
)

print(
  phase5_dataset_inventory,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 16. PRINT DATASET VARIABLE NAMES
# ------------------------------------------------------------------------------

cat(
  "\nPHASE 4A DATA VARIABLES\n"
)

print(
  names(
    phase4A_data
  )
)

cat(
  "\nPHASE 4C DATA VARIABLES\n"
)

print(
  names(
    phase4C_data
  )
)

cat(
  "\nPHASE 4D DATA VARIABLES\n"
)

print(
  names(
    phase4D_data
  )
)


# ------------------------------------------------------------------------------
# 17. CHECK FOR EXPECTED HIGH-PRIORITY OBJECTS
# ------------------------------------------------------------------------------

expected_checkpoint_objects <- tribble(
  ~phase, ~expected_object,
  
  "Phase 4A",
  "polychoric_matrix",
  
  "Phase 4A",
  "ordinal_item_diagnostics",
  
  "Phase 4A",
  "composite_summary",
  
  "Phase 4A",
  "measurement_decision",
  
  "Phase 4B",
  "final_model_coefficients",
  
  "Phase 4B",
  "final_model_selection",
  
  "Phase 4B",
  "adjusted_probability_by_awareness",
  
  "Phase 4C",
  "final_model",
  
  "Phase 4C",
  "outcome_distribution",
  
  "Phase 4C",
  "predicted_probabilities",
  
  "Phase 4D",
  "final_model",
  
  "Phase 4D",
  "final_model_selection",
  
  "Phase 4D",
  "final_coefficient_table",
  
  "Phase 4D",
  "preferred_model_distribution",
  
  "Phase 4D",
  "human_review_summary",
  
  "Phase 4D",
  "predicted_probabilities"
)

phase5_expected_object_check <- expected_checkpoint_objects |>
  mutate(
    object_available = case_when(
      phase == "Phase 4A" ~
        expected_object %in%
        names(
          phase4A_objects
        ),
      
      phase == "Phase 4B" ~
        expected_object %in%
        names(
          phase4B_objects
        ),
      
      phase == "Phase 4C" ~
        expected_object %in%
        names(
          phase4C_objects
        ),
      
      phase == "Phase 4D" ~
        expected_object %in%
        names(
          phase4D_objects
        ),
      
      TRUE ~
        FALSE
    )
  )

cat(
  "\nEXPECTED HIGH-PRIORITY OBJECT CHECK\n"
)

print(
  phase5_expected_object_check,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 18. SAVE THE BLOCK-2 INVENTORY
# ------------------------------------------------------------------------------

phase5_inventory_dir <- here(
  "output",
  "learner_integrated_phase4A_4D"
)

dir.create(
  phase5_inventory_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  list(
    source_paths =
      phase5_source_paths,
    
    phase4A_object_inventory =
      phase4A_object_inventory,
    
    phase4B_object_inventory =
      phase4B_object_inventory,
    
    phase4C_object_inventory =
      phase4C_object_inventory,
    
    phase4D_object_inventory =
      phase4D_object_inventory,
    
    dataset_inventory =
      phase5_dataset_inventory,
    
    expected_object_check =
      phase5_expected_object_check
  ),
  
  file.path(
    phase5_inventory_dir,
    "phase5_block2_source_inventory.rds"
  )
)


# ------------------------------------------------------------------------------
# 19. COMPLETION MESSAGE
# ------------------------------------------------------------------------------

cat(
  "\n",
  "PHASE 5 BLOCK 2 COMPLETE\n",
  "Return the printed sections titled:\n",
  "  1. PHASE 4A ANALYSIS OBJECTS\n",
  "  2. PHASE 4B FINAL ANALYSIS OBJECTS\n",
  "  3. PHASE 4C FINAL ANALYSIS CHECKPOINT\n",
  "  4. PHASE 4D FINAL ANALYSIS CHECKPOINT\n",
  "  5. SUPPORTING DATASET INVENTORY\n",
  "  6. EXPECTED HIGH-PRIORITY OBJECT CHECK\n",
  "\n",
  "The next block will extract and verify the headline statistics ",
  "that will appear in the integrated workbook.\n",
  sep = ""
)




# ==============================================================================
# PHASE 5, BLOCK 3: EXTRACT AND VERIFY HEADLINE RESULTS
# ==============================================================================


# ------------------------------------------------------------------------------
# 20. HELPER FUNCTIONS FOR REPORT-READY TABLES
# ------------------------------------------------------------------------------

as_report_table <- function(object) {
  if (is.matrix(object)) {
    return(
      as.data.frame(object) |>
        tibble::rownames_to_column(
          var = "row"
        ) |>
        as_tibble()
    )
  }
  
  if (is.data.frame(object)) {
    object_data <- as.data.frame(
      object
    )
    
    if (tibble::has_rownames(object_data)) {
      object_data <- object_data |>
        tibble::rownames_to_column(
          var = "row"
        )
    }
    
    return(
      as_tibble(object_data)
    )
  }
  
  tibble(
    value = object
  )
}


inspect_report_table <- function(
    object,
    table_title
) {
  report_table <- as_report_table(
    object
  )
  
  cat(
    "\n",
    table_title,
    "\n",
    strrep(
      "-",
      nchar(table_title)
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Columns: ",
    paste(
      names(report_table),
      collapse = " | "
    ),
    "\n",
    sep = ""
  )
  
  print(
    report_table,
    n = Inf,
    width = Inf
  )
  
  invisible(
    report_table
  )
}


# ------------------------------------------------------------------------------
# 21. PHASE 4A: SCALE QUALITY AND FEEDBACK EXPERIENCE
# ------------------------------------------------------------------------------

phase4A_polychoric_matrix <- as_report_table(
  phase4A_objects$polychoric_matrix
)

phase4A_eigenvalue_summary <- tibble(
  component = seq_along(
    phase4A_objects$raw_eigenvalues
  ),
  
  eigenvalue =
    phase4A_objects$raw_eigenvalues,
  
  proportion_of_variance =
    phase4A_objects$raw_eigenvalues /
    sum(
      phase4A_objects$raw_eigenvalues
    ),
  
  cumulative_proportion =
    cumsum(
      phase4A_objects$raw_eigenvalues
    ) /
    sum(
      phase4A_objects$raw_eigenvalues
    )
)


# ------------------------------------------------------------------------------
# 21A. CALCULATE CFA-BASED OMEGA
# ------------------------------------------------------------------------------

phase4A_standardized_solution <- lavaan::standardizedSolution(
  phase4A_objects$four_item_cfa
)

phase4A_standardized_loadings <- phase4A_standardized_solution |>
  filter(
    op == "=~"
  ) |>
  select(
    factor = lhs,
    item = rhs,
    standardized_loading = est.std
  )

phase4A_standardized_residuals <- phase4A_standardized_solution |>
  filter(
    op == "~~",
    lhs == rhs,
    lhs %in%
      phase4A_standardized_loadings$item
  ) |>
  select(
    item = lhs,
    standardized_residual_variance = est.std
  )

phase4A_omega <- (
  sum(
    phase4A_standardized_loadings$
      standardized_loading
  )^2
) /
  (
    sum(
      phase4A_standardized_loadings$
        standardized_loading
    )^2 +
      sum(
        phase4A_standardized_residuals$
          standardized_residual_variance
      )
  )

phase4A_cfa_fit <- lavaan::fitMeasures(
  phase4A_objects$four_item_cfa,
  fit.measures = c(
    "chisq",
    "df",
    "pvalue",
    "cfi",
    "tli",
    "rmsea",
    "srmr"
  )
) |>
  enframe(
    name = "fit_statistic",
    value = "estimate"
  )

phase4A_feedback_summary <- phase4A_data |>
  summarise(
    analytic_n = sum(
      !is.na(
        feedback_experience
      )
    ),
    
    composite_mean = mean(
      feedback_experience,
      na.rm = TRUE
    ),
    
    composite_sd = sd(
      feedback_experience,
      na.rm = TRUE
    ),
    
    composite_median = median(
      feedback_experience,
      na.rm = TRUE
    ),
    
    minimum = min(
      feedback_experience,
      na.rm = TRUE
    ),
    
    maximum = max(
      feedback_experience,
      na.rm = TRUE
    ),
    
    maximum_score_n = sum(
      feedback_experience == 5,
      na.rm = TRUE
    ),
    
    maximum_score_percent =
      100 * mean(
        feedback_experience == 5,
        na.rm = TRUE
      )
  )

phase4A_headline_metrics <- tibble(
  metric = c(
    "Analytic sample",
    "Ordinal alpha",
    "CFA-based ordinal omega",
    "First eigenvalue",
    "Variance represented by first component",
    "Feedback-experience composite mean",
    "Feedback-experience composite SD",
    "Maximum-score respondents"
  ),
  
  estimate = c(
    phase4A_feedback_summary$analytic_n,
    phase4A_objects$ordinal_alpha,
    phase4A_omega,
    phase4A_objects$raw_eigenvalues[1],
    phase4A_eigenvalue_summary$
      proportion_of_variance[1],
    phase4A_feedback_summary$composite_mean,
    phase4A_feedback_summary$composite_sd,
    phase4A_feedback_summary$
      maximum_score_percent
  ),
  
  unit = c(
    "n",
    "reliability coefficient",
    "reliability coefficient",
    "eigenvalue",
    "proportion",
    "1-to-5 scale",
    "1-to-5 scale",
    "percent"
  )
)

inspect_report_table(
  phase4A_headline_metrics,
  "PHASE 4A HEADLINE METRICS"
)

inspect_report_table(
  phase4A_objects$ordinal_item_diagnostics,
  "PHASE 4A ORDINAL ITEM DIAGNOSTICS"
)

inspect_report_table(
  phase4A_standardized_loadings,
  "PHASE 4A STANDARDIZED CFA LOADINGS"
)

inspect_report_table(
  phase4A_cfa_fit,
  "PHASE 4A CFA FIT"
)

inspect_report_table(
  phase4A_objects$composite_summary,
  "PHASE 4A COMPOSITE SUMMARY"
)

inspect_report_table(
  phase4A_objects$measurement_decision,
  "PHASE 4A MEASUREMENT DECISION"
)


# ------------------------------------------------------------------------------
# 22. PHASE 4B: AI COMFORT
# ------------------------------------------------------------------------------

phase4B_outcome_distribution <- phase4A_data |>
  count(
    ai_comfort,
    .drop = FALSE,
    name = "n"
  ) |>
  mutate(
    percent = 100 * n / sum(n),
    valid_n = sum(n)
  )

phase4B_model_fit <- tibble(
  analytic_n = nobs(
    phase4B_objects$final_model
  ),
  
  log_likelihood = as.numeric(
    logLik(
      phase4B_objects$final_model
    )
  ),
  
  AIC = AIC(
    phase4B_objects$final_model
  ),
  
  number_of_parameters = attr(
    logLik(
      phase4B_objects$final_model
    ),
    "df"
  ),
  
  convergence_code =
    phase4B_objects$final_model$convergence
)

inspect_report_table(
  phase4B_outcome_distribution,
  "PHASE 4B AI-COMFORT DISTRIBUTION"
)

inspect_report_table(
  phase4B_model_fit,
  "PHASE 4B FINAL MODEL FIT"
)

inspect_report_table(
  phase4B_objects$final_model_coefficients,
  "PHASE 4B FINAL MODEL COEFFICIENTS"
)

inspect_report_table(
  phase4B_objects$final_model_selection,
  "PHASE 4B FINAL MODEL SELECTION"
)

inspect_report_table(
  phase4B_objects$awareness_scale_interpretation,
  "PHASE 4B AWARENESS-SCALE INTERPRETATION"
)

inspect_report_table(
  phase4B_objects$adjusted_probability_by_awareness,
  "PHASE 4B ADJUSTED PROBABILITIES BY AI AWARENESS"
)

inspect_report_table(
  phase4B_objects$
    adjusted_probability_by_feedback_and_awareness,
  paste0(
    "PHASE 4B ADJUSTED PROBABILITIES BY ",
    "FEEDBACK EXPERIENCE AND AI AWARENESS"
  )
)

inspect_report_table(
  phase4B_objects$feedback_ceiling_summary,
  "PHASE 4B FEEDBACK-COMPOSITE CEILING SUMMARY"
)

inspect_report_table(
  phase4B_objects$final_analysis_decision,
  "PHASE 4B FINAL ANALYSIS DECISION"
)


# ------------------------------------------------------------------------------
# 23. PHASE 4C: PERCEIVED AI USEFULNESS
# ------------------------------------------------------------------------------

phase4C_model_coefficient_matrix <- coef(
  summary(
    phase4C_objects$final_model
  )
) |>
  as.data.frame() |>
  tibble::rownames_to_column(
    var = "parameter"
  ) |>
  as_tibble()

phase4C_model_fit <- tibble(
  analytic_n = nobs(
    phase4C_objects$final_model
  ),
  
  log_likelihood = as.numeric(
    logLik(
      phase4C_objects$final_model
    )
  ),
  
  AIC = AIC(
    phase4C_objects$final_model
  ),
  
  number_of_parameters = attr(
    logLik(
      phase4C_objects$final_model
    ),
    "df"
  ),
  
  convergence_code =
    phase4C_objects$final_model$convergence
)

phase4C_observed_agreement <- phase4C_data |>
  summarise(
    analytic_n = n(),
    
    agreement_n = sum(
      as.character(
        ai_usefulness
      ) %in% c(
        "Somewhat agree",
        "Strongly agree"
      )
    ),
    
    agreement_percent =
      100 * agreement_n / analytic_n
  )

inspect_report_table(
  phase4C_observed_agreement,
  "PHASE 4C OBSERVED AGREEMENT SUMMARY"
)

inspect_report_table(
  phase4C_objects$outcome_distribution,
  "PHASE 4C OUTCOME DISTRIBUTION"
)

inspect_report_table(
  phase4C_model_fit,
  "PHASE 4C FINAL MODEL FIT"
)

inspect_report_table(
  phase4C_model_coefficient_matrix,
  "PHASE 4C FINAL MODEL COEFFICIENTS"
)

inspect_report_table(
  phase4C_objects$final_model_fit_comparison,
  "PHASE 4C MODEL-FIT COMPARISON"
)

inspect_report_table(
  phase4C_objects$proportional_vs_scale_comparison,
  paste0(
    "PHASE 4C PROPORTIONAL-ODDS VERSUS ",
    "LOCATION-SCALE COMPARISON"
  )
)

inspect_report_table(
  phase4C_objects$feedback_addition_comparison,
  "PHASE 4C FEEDBACK-ADDITION COMPARISON"
)

inspect_report_table(
  phase4C_objects$predicted_probabilities,
  "PHASE 4C ADJUSTED AGREEMENT PROBABILITIES"
)


# ------------------------------------------------------------------------------
# 24. PHASE 4D: PREFERRED FEEDBACK MODEL
# ------------------------------------------------------------------------------

phase4D_model_fit <- tibble(
  analytic_n = nrow(
    phase4D_objects$final_model$model
  ),
  
  log_likelihood = as.numeric(
    logLik(
      phase4D_objects$final_model
    )
  ),
  
  AIC = AIC(
    phase4D_objects$final_model
  ),
  
  BIC = BIC(
    phase4D_objects$final_model
  ),
  
  number_of_parameters = attr(
    logLik(
      phase4D_objects$final_model
    ),
    "df"
  ),
  
  convergence_code =
    phase4D_objects$final_model$convergence
)

phase4D_report_ready_coefficients <- phase4D_objects$
  final_coefficient_table |>
  filter(
    term != "(Intercept)"
  )

inspect_report_table(
  phase4D_objects$preferred_model_distribution,
  "PHASE 4D PREFERRED-MODEL DISTRIBUTION"
)

inspect_report_table(
  phase4D_objects$human_review_summary,
  "PHASE 4D HUMAN-REVIEW SUMMARY"
)

inspect_report_table(
  phase4D_model_fit,
  "PHASE 4D FINAL MODEL FIT"
)

inspect_report_table(
  phase4D_objects$final_model_selection,
  "PHASE 4D FINAL MODEL SELECTION"
)

inspect_report_table(
  phase4D_report_ready_coefficients,
  "PHASE 4D REPORT-READY COEFFICIENTS"
)

inspect_report_table(
  phase4D_objects$reduced_model_comparisons,
  "PHASE 4D REDUCED-MODEL COMPARISONS"
)

inspect_report_table(
  phase4D_objects$estimator_comparison,
  "PHASE 4D MAXIMUM-LIKELIHOOD VERSUS BIAS-REDUCED ESTIMATES"
)

inspect_report_table(
  phase4D_objects$grouped_probabilities,
  "PHASE 4D GROUPED ADJUSTED PROBABILITIES"
)


# ------------------------------------------------------------------------------
# 25. SAVE BLOCK-3 VERIFIED RESULTS
# ------------------------------------------------------------------------------

phase5_output_dir <- here(
  "output",
  "learner_integrated_phase4A_4D"
)

dir.create(
  phase5_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  list(
    phase4A_headline_metrics =
      phase4A_headline_metrics,
    
    phase4A_eigenvalue_summary =
      phase4A_eigenvalue_summary,
    
    phase4A_standardized_loadings =
      phase4A_standardized_loadings,
    
    phase4A_cfa_fit =
      phase4A_cfa_fit,
    
    phase4A_feedback_summary =
      phase4A_feedback_summary,
    
    phase4B_outcome_distribution =
      phase4B_outcome_distribution,
    
    phase4B_model_fit =
      phase4B_model_fit,
    
    phase4C_observed_agreement =
      phase4C_observed_agreement,
    
    phase4C_model_fit =
      phase4C_model_fit,
    
    phase4C_model_coefficient_matrix =
      phase4C_model_coefficient_matrix,
    
    phase4D_model_fit =
      phase4D_model_fit,
    
    phase4D_report_ready_coefficients =
      phase4D_report_ready_coefficients
  ),
  
  file.path(
    phase5_output_dir,
    "phase5_block3_verified_headline_results.rds"
  )
)


# ------------------------------------------------------------------------------
# 26. COMPLETION MESSAGE
# ------------------------------------------------------------------------------

cat(
  "\n",
  "PHASE 5 BLOCK 3 COMPLETE\n",
  "Return the printed sections titled:\n",
  "  1. PHASE 4A HEADLINE METRICS\n",
  "  2. PHASE 4B FINAL MODEL COEFFICIENTS\n",
  "  3. PHASE 4B FINAL MODEL SELECTION\n",
  "  4. PHASE 4C OBSERVED AGREEMENT SUMMARY\n",
  "  5. PHASE 4C FINAL MODEL COEFFICIENTS\n",
  "  6. PHASE 4C ADJUSTED AGREEMENT PROBABILITIES\n",
  "  7. PHASE 4D PREFERRED-MODEL DISTRIBUTION\n",
  "  8. PHASE 4D HUMAN-REVIEW SUMMARY\n",
  "  9. PHASE 4D FINAL MODEL SELECTION\n",
  " 10. PHASE 4D REDUCED-MODEL COMPARISONS\n",
  "\n",
  "Block 4 will use the verified values to construct the executive summary, ",
  "cross-phase synthesis, and headline-metrics tables.\n",
  sep = ""
)