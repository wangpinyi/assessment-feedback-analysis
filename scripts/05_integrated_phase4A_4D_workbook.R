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
library(ordinal)
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

# ------------------------------------------------------------------------------
# PHASE 4B FINAL MODEL FIT
# ------------------------------------------------------------------------------

phase4B_log_likelihood <- logLik(
  phase4B_objects$final_model
)

phase4B_convergence_code <- if (
  !is.null(
    phase4B_objects$final_model$optRes$convergence
  )
) {
  as.integer(
    phase4B_objects$final_model$optRes$convergence
  )
} else if (
  !is.null(
    phase4B_objects$final_model$convergence
  )
) {
  as.integer(
    phase4B_objects$final_model$convergence
  )
} else {
  NA_integer_
}

phase4B_model_fit <- tibble(
  analytic_n = nobs(
    phase4B_objects$final_model
  ),
  
  log_likelihood = as.numeric(
    phase4B_log_likelihood
  ),
  
  AIC = AIC(
    phase4B_objects$final_model
  ),
  
  number_of_parameters = attr(
    phase4B_log_likelihood,
    "df"
  ),
  
  convergence_code =
    phase4B_convergence_code
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



# ==============================================================================
# PHASE 5, BLOCK 4: BUILD VERIFIED CROSS-PHASE SUMMARY TABLES
# ==============================================================================


# ------------------------------------------------------------------------------
# 27. HELPER FUNCTIONS
# ------------------------------------------------------------------------------

format_p_value <- function(
    p_value
) {
  case_when(
    is.na(p_value) ~
      NA_character_,
    
    p_value < 0.001 ~
      "< .001",
    
    TRUE ~
      paste0(
        "= ",
        sprintf(
          "%.3f",
          p_value
        )
      )
  )
}


format_percent_value <- function(
    value,
    digits = 1
) {
  paste0(
    sprintf(
      paste0(
        "%.",
        digits,
        "f"
      ),
      value
    ),
    "%"
  )
}


# ------------------------------------------------------------------------------
# 28. CORRECT PHASE 4A HEADLINE RESULTS
# ------------------------------------------------------------------------------

# The four-item CFA was inadmissible because of a negative residual variance
# and a standardized loading above 1.00. CFA-based omega and CFA fit indices
# are therefore excluded from the integrated headline results.

phase4A_average_polychoric_correlation <- mean(
  phase4A_objects$polychoric_matrix[
    upper.tri(
      phase4A_objects$polychoric_matrix
    )
  ]
)

phase4A_minimum_polychoric_correlation <- min(
  phase4A_objects$polychoric_matrix[
    upper.tri(
      phase4A_objects$polychoric_matrix
    )
  ]
)

phase4A_maximum_polychoric_correlation <- max(
  phase4A_objects$polychoric_matrix[
    upper.tri(
      phase4A_objects$polychoric_matrix
    )
  ]
)

phase4A_primary_sensitivity_correlation <-
  phase4A_objects$measurement_decision |>
  filter(
    feature ==
      "Correlation between primary and sensitivity scores"
  ) |>
  pull(
    result
  ) |>
  as.numeric()

phase4A_headline_metrics_verified <- tibble(
  result = c(
    "Analytic sample",
    "Ordinal alpha",
    "Average polychoric correlation",
    "Range of polychoric correlations",
    "Variance represented by first component",
    "Feedback-experience composite mean",
    "Feedback-experience composite SD",
    "Maximum-score respondents",
    "Primary-sensitivity composite correlation",
    "Measurement decision"
  ),
  
  estimate = c(
    as.character(
      phase4A_feedback_summary$analytic_n
    ),
    
    sprintf(
      "%.3f",
      phase4A_objects$ordinal_alpha
    ),
    
    sprintf(
      "%.3f",
      phase4A_average_polychoric_correlation
    ),
    
    paste0(
      sprintf(
        "%.3f",
        phase4A_minimum_polychoric_correlation
      ),
      " to ",
      sprintf(
        "%.3f",
        phase4A_maximum_polychoric_correlation
      )
    ),
    
    format_percent_value(
      100 *
        phase4A_eigenvalue_summary$
        proportion_of_variance[1]
    ),
    
    sprintf(
      "%.2f",
      phase4A_feedback_summary$composite_mean
    ),
    
    sprintf(
      "%.3f",
      phase4A_feedback_summary$composite_sd
    ),
    
    format_percent_value(
      phase4A_feedback_summary$
        maximum_score_percent
    ),
    
    sprintf(
      "%.3f",
      phase4A_primary_sensitivity_correlation
    ),
    
    paste0(
      "Retain the four-item unit-weighted index; ",
      "do not interpret the inadmissible CFA."
    )
  ),
  
  interpretation = c(
    "All 170 respondents had complete data for the four feedback items.",
    
    "The four-item index demonstrated strong ordinal reliability.",
    
    "The feedback items were strongly related at the latent-response level.",
    
    "All item pairs were positively associated, although the strength varied across pairs.",
    
    "A dominant first component supported summarizing the items with one overall feedback-experience score.",
    
    "Learners reported exceptionally favorable feedback experiences.",
    
    "The composite displayed limited variability.",
    
    "The pronounced ceiling effect limits differentiation among respondents with very positive feedback experiences.",
    
    "The four-item and three-item scores produced nearly identical respondent rankings.",
    
    "The four-item index is retained for substantive coverage, with the three-item version used as a sensitivity measure."
  ),
  
  reporting_caution = c(
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "Interpret regression estimates in light of the restricted score variation.",
    "Do not treat the composite as finely distinguishing highly satisfied respondents.",
    NA_character_,
    paste0(
      "Do not report CFA-based omega or global CFA fit because ",
      "the four-item CFA was inadmissible."
    )
  )
)


# ------------------------------------------------------------------------------
# 29. CORRECT PHASE 4B MODEL FIT
# ------------------------------------------------------------------------------

phase4B_model_fit_verified <- phase4B_objects$
  final_model_selection |>
  filter(
    model ==
      "Final linear awareness-scale model"
  ) |>
  transmute(
    selected_model = model,
    analytic_n = n,
    number_of_parameters,
    log_likelihood,
    AIC,
    BIC,
    maximum_absolute_gradient,
    hessian_condition_number,
    link
  )

stopifnot(
  nrow(
    phase4B_model_fit_verified
  ) == 1
)


# ------------------------------------------------------------------------------
# 30. CREATE PHASE 4B EFFECT TABLE
# ------------------------------------------------------------------------------

phase4B_effects_verified <- phase4B_objects$
  final_model_coefficients |>
  filter(
    parameter %in% c(
      "prior_ai_experienceExtensive.experience",
      "ai_awareness_score",
      "feedback_experience_half_point",
      "ai_awareness_score.1"
    )
  ) |>
  mutate(
    effect = case_when(
      parameter ==
        "prior_ai_experienceExtensive.experience" ~
        "Extensive prior AI experience versus no experience",
      
      parameter ==
        "ai_awareness_score" ~
        "AI awareness: location effect",
      
      parameter ==
        "feedback_experience_half_point" ~
        "Feedback experience: 0.5-point increase",
      
      parameter ==
        "ai_awareness_score.1" ~
        "AI awareness: log-scale effect",
      
      TRUE ~
        parameter
    ),
    
    effect_type = case_when(
      parameter ==
        "ai_awareness_score.1" ~
        "Scale effect",
      
      TRUE ~
        "Location effect"
    ),
    
    p_value_display = map_chr(
      p_value,
      format_p_value
    )
  ) |>
  select(
    effect,
    effect_type,
    estimate,
    standard_error,
    z_value,
    p_value,
    p_value_display,
    confidence_low,
    confidence_high
  )

phase4B_awareness_probabilities_verified <- phase4B_objects$
  adjusted_probability_by_awareness |>
  transmute(
    ai_awareness,
    predicted_agreement_probability =
      predicted_probability,
    
    predicted_percent =
      100 * predicted_probability,
    
    confidence_low_percent =
      100 * confidence_low,
    
    confidence_high_percent =
      100 * confidence_high
  )


# ------------------------------------------------------------------------------
# 31. CREATE PHASE 4B HEADLINE SUMMARY
# ------------------------------------------------------------------------------

phase4B_headline_summary <- tibble(
  finding = c(
    "Extensive prior AI experience",
    "AI awareness",
    "Feedback experience",
    "Adjusted comfort probability by awareness",
    "Model form"
  ),
  
  evidence = c(
    paste0(
      "Location coefficient = ",
      sprintf(
        "%.2f",
        phase4B_effects_verified |>
          filter(
            effect ==
              "Extensive prior AI experience versus no experience"
          ) |>
          pull(
            estimate
          )
      ),
      ", p ",
      phase4B_effects_verified |>
        filter(
          effect ==
            "Extensive prior AI experience versus no experience"
        ) |>
        pull(
          p_value_display
        )
    ),
    
    paste0(
      "Location coefficient = ",
      sprintf(
        "%.2f",
        phase4B_effects_verified |>
          filter(
            effect ==
              "AI awareness: location effect"
          ) |>
          pull(
            estimate
          )
      ),
      ", p ",
      phase4B_effects_verified |>
        filter(
          effect ==
            "AI awareness: location effect"
        ) |>
        pull(
          p_value_display
        ),
      "; log-scale coefficient = ",
      sprintf(
        "%.3f",
        phase4B_effects_verified |>
          filter(
            effect ==
              "AI awareness: log-scale effect"
          ) |>
          pull(
            estimate
          )
      ),
      ", p ",
      phase4B_effects_verified |>
        filter(
          effect ==
            "AI awareness: log-scale effect"
        ) |>
        pull(
          p_value_display
        )
    ),
    
    paste0(
      "Location coefficient per 0.5-point increase = ",
      sprintf(
        "%.3f",
        phase4B_effects_verified |>
          filter(
            effect ==
              "Feedback experience: 0.5-point increase"
          ) |>
          pull(
            estimate
          )
      ),
      ", p ",
      phase4B_effects_verified |>
        filter(
          effect ==
            "Feedback experience: 0.5-point increase"
        ) |>
        pull(
          p_value_display
        )
    ),
    
    paste0(
      "Probability of somewhat or strongly agreeing with AI comfort: ",
      "48.6% among respondents not aware of AI use, ",
      "71.8% among somewhat aware respondents, and ",
      "82.2% among fully aware respondents."
    ),
    
    paste0(
      "Cumulative-link location-scale model; AIC = ",
      sprintf(
        "%.2f",
        phase4B_model_fit_verified$AIC
      ),
      "."
    )
  ),
  
  interpretation = c(
    "Respondents with extensive prior AI experience reported greater comfort with AI-supported feedback.",
    
    "Greater awareness corresponded with higher comfort, but awareness also changed the dispersion of the latent comfort response.",
    
    "More favorable feedback experiences were associated with greater AI comfort.",
    
    "The adjusted probabilities show a substantial increase in comfort across awareness levels.",
    
    "Because awareness affected both location and scale, adjusted probabilities are preferable to a single proportional-odds ratio."
  )
)


# ------------------------------------------------------------------------------
# 32. CORRECT PHASE 4C MODEL FIT
# ------------------------------------------------------------------------------

phase4C_model_fit_verified <- phase4C_objects$
  final_model_fit_comparison |>
  filter(
    model ==
      "Comfort-added AI-comfort scale model"
  )

stopifnot(
  nrow(
    phase4C_model_fit_verified
  ) == 1
)


# ------------------------------------------------------------------------------
# 33. CREATE PHASE 4C EFFECT TABLE
# ------------------------------------------------------------------------------

phase4C_effects_verified <- phase4C_model_coefficient_matrix |>
  filter(
    parameter %in% c(
      "ai_comfort_score",
      "ai_comfort_score.1"
    )
  ) |>
  transmute(
    effect = case_when(
      parameter ==
        "ai_comfort_score" ~
        "AI comfort: location effect",
      
      parameter ==
        "ai_comfort_score.1" ~
        "AI comfort: log-scale effect"
    ),
    
    effect_type = case_when(
      parameter ==
        "ai_comfort_score" ~
        "Location effect",
      
      parameter ==
        "ai_comfort_score.1" ~
        "Scale effect"
    ),
    
    estimate = Estimate,
    standard_error = `Std. Error`,
    z_value = `z value`,
    p_value = `Pr(>|z|)`,
    
    p_value_display = map_chr(
      p_value,
      format_p_value
    )
  )

phase4C_feedback_addition_p <- phase4C_objects$
  feedback_addition_comparison |>
  filter(
    !is.na(
      `Pr(>Chisq)`
    )
  ) |>
  pull(
    `Pr(>Chisq)`
  )

phase4C_location_scale_p <- phase4C_objects$
  proportional_vs_scale_comparison |>
  filter(
    !is.na(
      `Pr(>Chisq)`
    )
  ) |>
  pull(
    `Pr(>Chisq)`
  )


# ------------------------------------------------------------------------------
# 34. CREATE PHASE 4C HEADLINE SUMMARY
# ------------------------------------------------------------------------------

phase4C_lowest_comfort_probability <- phase4C_objects$
  predicted_probabilities |>
  filter(
    ai_comfort_score == 0
  ) |>
  pull(
    predicted_probability
  )

phase4C_highest_comfort_probability <- phase4C_objects$
  predicted_probabilities |>
  filter(
    ai_comfort_score == 4
  ) |>
  pull(
    predicted_probability
  )

phase4C_headline_summary <- tibble(
  finding = c(
    "Observed perceived usefulness",
    "AI comfort",
    "Adjusted usefulness probability",
    "Location-scale model improvement",
    "Feedback-experience contribution"
  ),
  
  evidence = c(
    paste0(
      phase4C_observed_agreement$agreement_n,
      " of ",
      phase4C_observed_agreement$analytic_n,
      " respondents (",
      format_percent_value(
        phase4C_observed_agreement$
          agreement_percent
      ),
      ") somewhat or strongly agreed that AI-generated feedback ",
      "could be as useful as human feedback."
    ),
    
    paste0(
      "Location coefficient = ",
      sprintf(
        "%.2f",
        phase4C_effects_verified |>
          filter(
            effect ==
              "AI comfort: location effect"
          ) |>
          pull(
            estimate
          )
      ),
      ", p ",
      phase4C_effects_verified |>
        filter(
          effect ==
            "AI comfort: location effect"
        ) |>
        pull(
          p_value_display
        ),
      "; log-scale coefficient = ",
      sprintf(
        "%.3f",
        phase4C_effects_verified |>
          filter(
            effect ==
              "AI comfort: log-scale effect"
          ) |>
          pull(
            estimate
          )
      ),
      ", p ",
      phase4C_effects_verified |>
        filter(
          effect ==
            "AI comfort: log-scale effect"
        ) |>
        pull(
          p_value_display
        )
    ),
    
    paste0(
      "Adjusted agreement probability increased from ",
      scales::percent(
        phase4C_lowest_comfort_probability,
        accuracy = 0.1
      ),
      " at the lowest comfort level to ",
      scales::percent(
        phase4C_highest_comfort_probability,
        accuracy = 0.1
      ),
      " at the highest comfort level."
    ),
    
    paste0(
      "Likelihood-ratio test p ",
      format_p_value(
        phase4C_location_scale_p
      ),
      "; selected-model AIC = ",
      sprintf(
        "%.2f",
        phase4C_model_fit_verified$AIC
      ),
      "."
    ),
    
    paste0(
      "Adding feedback experience after AI comfort: ",
      "likelihood-ratio test p ",
      format_p_value(
        phase4C_feedback_addition_p
      ),
      "."
    )
  ),
  
  interpretation = c(
    "A majority of respondents viewed AI-generated feedback as potentially comparable in usefulness to human feedback.",
    
    "AI comfort was strongly associated with perceived AI usefulness and also affected latent-response dispersion.",
    
    "Comfort clearly distinguished respondents with low versus high adjusted probabilities of viewing AI feedback as useful.",
    
    "The location-scale model fit better than the proportional-odds benchmark.",
    
    "Feedback experience did not improve the usefulness model after AI comfort was included."
  )
)


# ------------------------------------------------------------------------------
# 35. CREATE PHASE 4D VERIFIED SUMMARY TABLES
# ------------------------------------------------------------------------------

phase4D_usefulness_addition_p <- phase4D_objects$
  reduced_model_comparisons |>
  filter(
    comparison ==
      "Feedback only versus feedback plus usefulness"
  ) |>
  pull(
    p_value
  )

phase4D_feedback_addition_p <- phase4D_objects$
  reduced_model_comparisons |>
  filter(
    comparison ==
      "Usefulness only versus usefulness plus feedback"
  ) |>
  pull(
    p_value
  )

phase4D_universal_review_percent <- phase4D_objects$
  human_review_summary |>
  filter(
    metric ==
      "Preferred universal human review"
  ) |>
  pull(
    percent
  )

phase4D_directional_universal_review_percent <-
  phase4D_objects$human_review_summary |>
  filter(
    metric ==
      "Universal review among directional preferences"
  ) |>
  pull(
    percent
  )

phase4D_ai_forward_low <- phase4D_objects$
  grouped_probabilities |>
  filter(
    ai_usefulness_score == 0,
    preference_group ==
      "AI-forward feedback model"
  ) |>
  pull(
    predicted_probability
  )

phase4D_ai_forward_high <- phase4D_objects$
  grouped_probabilities |>
  filter(
    ai_usefulness_score == 4,
    preference_group ==
      "AI-forward feedback model"
  ) |>
  pull(
    predicted_probability
  )

phase4D_headline_summary <- tibble(
  finding = c(
    "Largest preferred-model category",
    "Universal human review",
    "Perceived AI usefulness",
    "Feedback experience",
    "Adjusted AI-forward preference"
  ),
  
  evidence = c(
    paste0(
      "Human-led grading with AI support: 66 of 169 respondents (39.1%)."
    ),
    
    paste0(
      "111 of 169 respondents (",
      format_percent_value(
        phase4D_universal_review_percent
      ),
      ") preferred universal human review. Among respondents with ",
      "a directional preference, the percentage was ",
      format_percent_value(
        phase4D_directional_universal_review_percent
      ),
      "."
    ),
    
    paste0(
      "Adding perceived usefulness to a feedback-only model: ",
      "likelihood-ratio test p ",
      format_p_value(
        phase4D_usefulness_addition_p
      ),
      "."
    ),
    
    paste0(
      "Adding feedback experience to a usefulness-only model: ",
      "likelihood-ratio test p ",
      format_p_value(
        phase4D_feedback_addition_p
      ),
      "."
    ),
    
    paste0(
      "Adjusted probability of an AI-forward feedback model increased ",
      "from ",
      scales::percent(
        phase4D_ai_forward_low,
        accuracy = 0.1
      ),
      " among respondents who strongly disagreed that AI feedback was ",
      "as useful as human feedback to ",
      scales::percent(
        phase4D_ai_forward_high,
        accuracy = 0.1
      ),
      " among those who strongly agreed."
    )
  ),
  
  interpretation = c(
    "The most common single preference retained instructor control and used AI only as a support tool.",
    
    "Learners generally accepted AI assistance while retaining a strong preference for universal human accountability.",
    
    "Perceived AI usefulness substantially distinguished preferred feedback models.",
    
    paste0(
      "Feedback experience improved overall model fit even though its ",
      "individual category-specific Wald coefficients were not statistically ",
      "significant."
    ),
    
    "More favorable perceptions of AI usefulness corresponded with substantially more AI-forward implementation preferences."
  )
)


# ------------------------------------------------------------------------------
# 36. CREATE THE INTEGRATED EXECUTIVE SUMMARY
# ------------------------------------------------------------------------------

phase5_executive_summary <- tibble(
  phase = c(
    "Phase 4A",
    "Phase 4B",
    "Phase 4C",
    "Phase 4D"
  ),
  
  analytic_focus = c(
    "Feedback-experience measurement",
    "AI comfort",
    "Perceived AI usefulness",
    "Preferred feedback model"
  ),
  
  key_result = c(
    paste0(
      "The four feedback items formed a strongly reliable ordinal index ",
      "(ordinal alpha = ",
      sprintf(
        "%.3f",
        phase4A_objects$ordinal_alpha
      ),
      "). The composite mean was ",
      sprintf(
        "%.2f",
        phase4A_feedback_summary$composite_mean
      ),
      ", and ",
      format_percent_value(
        phase4A_feedback_summary$
          maximum_score_percent
      ),
      " received the maximum score."
    ),
    
    paste0(
      "Greater AI comfort was associated with extensive prior AI experience, ",
      "greater awareness of AI use, and more favorable feedback experience. ",
      "Adjusted comfort probability increased from 48.6% among respondents ",
      "not aware of AI use to 82.2% among fully aware respondents."
    ),
    
    paste0(
      format_percent_value(
        phase4C_observed_agreement$
          agreement_percent
      ),
      " agreed that AI-generated feedback could be as useful as human ",
      "feedback. Adjusted agreement increased from ",
      scales::percent(
        phase4C_lowest_comfort_probability,
        accuracy = 0.1
      ),
      " to ",
      scales::percent(
        phase4C_highest_comfort_probability,
        accuracy = 0.1
      ),
      " across the AI-comfort scale."
    ),
    
    paste0(
      format_percent_value(
        phase4D_universal_review_percent
      ),
      " preferred a model involving universal human review. The adjusted ",
      "probability of an AI-forward model increased from ",
      scales::percent(
        phase4D_ai_forward_low,
        accuracy = 0.1
      ),
      " to ",
      scales::percent(
        phase4D_ai_forward_high,
        accuracy = 0.1
      ),
      " across perceived AI-usefulness levels."
    )
  ),
  
  interpretation = c(
    "The current feedback process provides a strong experiential foundation, but the severe ceiling effect limits differentiation among highly positive experiences.",
    
    "AI readiness corresponds with prior exposure, transparency, and learners' experiences of the feedback process.",
    
    "AI comfort is the clearest bridge between general readiness and believing that AI-generated feedback is useful.",
    
    "Learners distinguish between viewing AI as useful and granting AI unrestricted responsibility for feedback or grading."
  ),
  
  reporting_caution = c(
    "The four-item CFA was inadmissible; the scale should be described as a reliable unit-weighted index rather than a confirmed latent factor.",
    
    "AI awareness affected both the location and scale components of comfort, so adjusted probabilities are more interpretable than one proportional-odds ratio.",
    
    "The selected location-scale model had an elevated Hessian condition number, so emphasis should remain on adjusted probabilities and the overall pattern.",
    
    "The multinomial analysis was associational, and sparse predictor-by-outcome cells warrant caution with individual category-specific estimates."
  )
)


# ------------------------------------------------------------------------------
# 37. CREATE THE CROSS-PHASE SYNTHESIS
# ------------------------------------------------------------------------------

phase5_cross_phase_synthesis <- tibble(
  evidence_stage = c(
    "1. Feedback foundation",
    "2. Readiness for AI-supported feedback",
    "3. Perceived usefulness of AI feedback",
    "4. Preferred implementation model"
  ),
  
  contributing_phases = c(
    "Phase 4A",
    "Phases 4A and 4B",
    "Phases 4B and 4C",
    "Phases 4C and 4D"
  ),
  
  main_evidence = c(
    paste0(
      "Ordinal alpha = ",
      sprintf(
        "%.3f",
        phase4A_objects$ordinal_alpha
      ),
      "; composite mean = ",
      sprintf(
        "%.2f",
        phase4A_feedback_summary$composite_mean
      ),
      "; maximum-score percentage = ",
      format_percent_value(
        phase4A_feedback_summary$
          maximum_score_percent
      ),
      "."
    ),
    
    paste0(
      "Extensive prior AI experience, AI awareness, and feedback experience ",
      "were associated with AI comfort."
    ),
    
    paste0(
      "Adjusted agreement that AI feedback could be as useful as human ",
      "feedback increased from ",
      scales::percent(
        phase4C_lowest_comfort_probability,
        accuracy = 0.1
      ),
      " to ",
      scales::percent(
        phase4C_highest_comfort_probability,
        accuracy = 0.1
      ),
      " across AI-comfort levels."
    ),
    
    paste0(
      format_percent_value(
        phase4D_universal_review_percent
      ),
      " preferred universal human review, while adjusted AI-forward ",
      "preference increased from ",
      scales::percent(
        phase4D_ai_forward_low,
        accuracy = 0.1
      ),
      " to ",
      scales::percent(
        phase4D_ai_forward_high,
        accuracy = 0.1
      ),
      " across perceived usefulness levels."
    )
  ),
  
  integrated_interpretation = c(
    "Respondents evaluated the existing feedback process exceptionally positively, creating a favorable but compressed foundation for considering AI-supported feedback.",
    
    "Comfort with AI appears connected to familiarity, transparency, and experience rather than functioning only as a fixed personal attitude.",
    
    "Comfort is more proximally connected to perceived AI usefulness than the general feedback-experience index.",
    
    "Greater perceived usefulness corresponds with more AI-forward preferences, but learners continue to place substantial value on universal human review."
  ),
  
  inference_boundary = c(
    "The scale findings describe measurement quality and response distribution; they do not establish causal effects.",
    
    "The Phase 4B associations do not demonstrate that awareness or feedback experience causes comfort.",
    
    "The sequence from comfort to usefulness is an analytic synthesis, not a tested mediation model.",
    
    "The preference results show associations and adjusted probabilities, not causal effects of usefulness on model choice."
  )
)


# ------------------------------------------------------------------------------
# 38. CREATE ONE LONG-FORM HEADLINE-METRICS TABLE
# ------------------------------------------------------------------------------

phase5_headline_metrics <- bind_rows(
  phase4A_headline_metrics_verified |>
    transmute(
      phase = "Phase 4A",
      domain =
        "Feedback-experience measurement",
      metric = result,
      estimate = estimate,
      statistical_evidence = NA_character_,
      interpretation,
      reporting_caution
    ),
  
  phase4B_headline_summary |>
    transmute(
      phase = "Phase 4B",
      domain = "AI comfort",
      metric = finding,
      estimate = NA_character_,
      statistical_evidence = evidence,
      interpretation,
      reporting_caution = case_when(
        finding == "AI awareness" ~
          paste0(
            "Awareness affected both location and scale; ",
            "use adjusted probabilities for interpretation."
          ),
        
        finding == "Model form" ~
          paste0(
            "The Hessian condition number was elevated, so large ",
            "individual coefficients should be interpreted cautiously."
          ),
        
        TRUE ~
          NA_character_
      )
    ),
  
  phase4C_headline_summary |>
    transmute(
      phase = "Phase 4C",
      domain = "Perceived AI usefulness",
      metric = finding,
      estimate = NA_character_,
      statistical_evidence = evidence,
      interpretation,
      reporting_caution = case_when(
        finding == "AI comfort" ~
          paste0(
            "Comfort affected both location and scale; adjusted ",
            "probabilities are the primary effect-size presentation."
          ),
        
        finding == "Location-scale model improvement" ~
          paste0(
            "The selected model had an elevated Hessian condition number."
          ),
        
        TRUE ~
          NA_character_
      )
    ),
  
  phase4D_headline_summary |>
    transmute(
      phase = "Phase 4D",
      domain = "Preferred feedback model",
      metric = finding,
      estimate = NA_character_,
      statistical_evidence = evidence,
      interpretation,
      reporting_caution = case_when(
        finding %in% c(
          "Perceived AI usefulness",
          "Feedback experience"
        ) ~
          paste0(
            "Interpret omnibus likelihood-ratio tests alongside ",
            "category-specific coefficients."
          ),
        
        finding ==
          "Adjusted AI-forward preference" ~
          paste0(
            "The results are associational, and some predictor-by-outcome ",
            "cells were sparse."
          ),
        
        TRUE ~
          NA_character_
      )
    )
)


# ------------------------------------------------------------------------------
# 39. PRINT TABLES FOR VERIFICATION
# ------------------------------------------------------------------------------

inspect_report_table(
  phase4A_headline_metrics_verified,
  "VERIFIED PHASE 4A HEADLINE RESULTS"
)

inspect_report_table(
  phase4B_model_fit_verified,
  "VERIFIED PHASE 4B MODEL FIT"
)

inspect_report_table(
  phase4B_effects_verified,
  "VERIFIED PHASE 4B EFFECTS"
)

inspect_report_table(
  phase4B_headline_summary,
  "PHASE 4B HEADLINE SUMMARY"
)

inspect_report_table(
  phase4C_model_fit_verified,
  "VERIFIED PHASE 4C MODEL FIT"
)

inspect_report_table(
  phase4C_effects_verified,
  "VERIFIED PHASE 4C EFFECTS"
)

inspect_report_table(
  phase4C_headline_summary,
  "PHASE 4C HEADLINE SUMMARY"
)

inspect_report_table(
  phase4D_headline_summary,
  "PHASE 4D HEADLINE SUMMARY"
)

inspect_report_table(
  phase5_executive_summary,
  "PHASE 5 EXECUTIVE SUMMARY"
)

inspect_report_table(
  phase5_cross_phase_synthesis,
  "PHASE 5 CROSS-PHASE SYNTHESIS"
)

inspect_report_table(
  phase5_headline_metrics,
  "PHASE 5 INTEGRATED HEADLINE METRICS"
)


# ------------------------------------------------------------------------------
# 40. SAVE THE VERIFIED INTEGRATION TABLES
# ------------------------------------------------------------------------------

saveRDS(
  list(
    phase4A_headline_metrics =
      phase4A_headline_metrics_verified,
    
    phase4B_model_fit =
      phase4B_model_fit_verified,
    
    phase4B_effects =
      phase4B_effects_verified,
    
    phase4B_awareness_probabilities =
      phase4B_awareness_probabilities_verified,
    
    phase4B_headline_summary =
      phase4B_headline_summary,
    
    phase4C_model_fit =
      phase4C_model_fit_verified,
    
    phase4C_effects =
      phase4C_effects_verified,
    
    phase4C_headline_summary =
      phase4C_headline_summary,
    
    phase4D_headline_summary =
      phase4D_headline_summary,
    
    executive_summary =
      phase5_executive_summary,
    
    cross_phase_synthesis =
      phase5_cross_phase_synthesis,
    
    integrated_headline_metrics =
      phase5_headline_metrics
  ),
  
  file.path(
    phase5_output_dir,
    "phase5_block4_verified_integration_tables.rds"
  )
)


# ------------------------------------------------------------------------------
# 41. COMPLETION MESSAGE
# ------------------------------------------------------------------------------

cat(
  "\n",
  "PHASE 5 BLOCK 4 COMPLETE\n",
  "Return the printed sections titled:\n",
  "  1. VERIFIED PHASE 4A HEADLINE RESULTS\n",
  "  2. VERIFIED PHASE 4B EFFECTS\n",
  "  3. PHASE 4B HEADLINE SUMMARY\n",
  "  4. VERIFIED PHASE 4C EFFECTS\n",
  "  5. PHASE 4C HEADLINE SUMMARY\n",
  "  6. PHASE 4D HEADLINE SUMMARY\n",
  "  7. PHASE 5 EXECUTIVE SUMMARY\n",
  "  8. PHASE 5 CROSS-PHASE SYNTHESIS\n",
  "\n",
  "After verification, Block 5 will create the single styled Excel workbook.\n",
  sep = ""
)


# ==============================================================================
# PHASE 5: SIMPLE FINAL INTEGRATED WORKBOOK
# Run after Phase 5 Block 4
# ==============================================================================
library(openxlsx)


# ------------------------------------------------------------------------------
# 1. OUTPUT SETTINGS
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

phase5_final_workbook_path <- file.path(
  phase5_output_dir,
  "learner_phase5_integrated_phase4A_4D_results_FINAL.xlsx"
)

# Figures are analytical content rather than workbook decoration.
# Change to FALSE to omit the Figures worksheet.
include_figures <- TRUE


# ------------------------------------------------------------------------------
# 2. PREPARE SIMPLE REPORT TABLES
# ------------------------------------------------------------------------------

phase4A_polychoric_excel <- as.data.frame(
  phase4A_objects$polychoric_matrix
) |>
  tibble::rownames_to_column(
    var = "item"
  ) |>
  as_tibble()

phase4D_report_ready_coefficients <- phase4D_objects$
  final_coefficient_table |>
  filter(
    term != "(Intercept)"
  )


# ------------------------------------------------------------------------------
# 3. CONVERT OBJECTS TO EXCEL-SAFE DATA FRAMES
# ------------------------------------------------------------------------------

simple_excel_ready <- function(
    object
) {
  if (is.null(object)) {
    return(
      tibble(
        note = "Object unavailable."
      )
    )
  }
  
  if (is.matrix(object)) {
    object <- as.data.frame(
      object
    ) |>
      tibble::rownames_to_column(
        var = "row"
      )
  }
  
  if (!is.data.frame(object)) {
    object <- as.data.frame(
      object
    )
  }
  
  object <- as_tibble(
    object
  ) |>
    mutate(
      across(
        where(is.factor),
        as.character
      ),
      
      across(
        where(is.ordered),
        as.character
      )
    )
  
  list_columns <- names(
    object
  )[
    purrr::map_lgl(
      object,
      is.list
    )
  ]
  
  if (length(list_columns) > 0) {
    object <- object |>
      mutate(
        across(
          all_of(
            list_columns
          ),
          ~ purrr::map_chr(
            .x,
            function(value) {
              paste(
                unlist(
                  value
                ),
                collapse = "; "
              )
            }
          )
        )
      )
  }
  
  object
}


# ------------------------------------------------------------------------------
# 4. DEFINE MINIMAL STYLES
# ------------------------------------------------------------------------------

sheet_title_style <- createStyle(
  fontSize = 14,
  textDecoration = "bold",
  valign = "top"
)

section_title_style <- createStyle(
  fontSize = 11,
  textDecoration = "bold",
  border = "bottom",
  borderStyle = "thin",
  valign = "top"
)

column_header_style <- createStyle(
  textDecoration = "bold",
  border = "bottom",
  borderStyle = "thin",
  wrapText = TRUE,
  valign = "top"
)

body_style <- createStyle(
  wrapText = TRUE,
  valign = "top"
)

note_style <- createStyle(
  textDecoration = "italic",
  wrapText = TRUE,
  valign = "top"
)


# ------------------------------------------------------------------------------
# 5. SIMPLE WRITING FUNCTIONS
# ------------------------------------------------------------------------------

write_sheet_title <- function(
    workbook,
    sheet,
    title,
    note = NULL
) {
  writeData(
    workbook,
    sheet,
    title,
    startRow = 1,
    startCol = 1,
    colNames = FALSE
  )
  
  addStyle(
    workbook,
    sheet,
    sheet_title_style,
    rows = 1,
    cols = 1,
    stack = TRUE
  )
  
  if (!is.null(note)) {
    writeData(
      workbook,
      sheet,
      note,
      startRow = 2,
      startCol = 1,
      colNames = FALSE
    )
    
    addStyle(
      workbook,
      sheet,
      note_style,
      rows = 2,
      cols = 1,
      stack = TRUE
    )
    
    setRowHeights(
      workbook,
      sheet,
      rows = 2,
      heights = 35
    )
  }
}


write_simple_section <- function(
    workbook,
    sheet,
    section_title,
    data,
    start_row,
    body_row_height = 30
) {
  data <- simple_excel_ready(
    data
  )
  
  writeData(
    workbook,
    sheet,
    section_title,
    startRow = start_row,
    startCol = 1,
    colNames = FALSE
  )
  
  addStyle(
    workbook,
    sheet,
    section_title_style,
    rows = start_row,
    cols = 1,
    stack = TRUE
  )
  
  table_start_row <- start_row + 1
  
  writeData(
    workbook,
    sheet,
    data,
    startRow = table_start_row,
    startCol = 1,
    colNames = TRUE,
    headerStyle = column_header_style,
    borders = "none"
  )
  
  if (nrow(data) > 0) {
    body_start_row <- table_start_row + 1
    body_end_row <- table_start_row + nrow(
      data
    )
    
    addStyle(
      workbook,
      sheet,
      body_style,
      rows = body_start_row:body_end_row,
      cols = seq_len(
        ncol(data)
      ),
      gridExpand = TRUE,
      stack = TRUE
    )
    
    setRowHeights(
      workbook,
      sheet,
      rows = body_start_row:body_end_row,
      heights = body_row_height
    )
  }
  
  table_start_row +
    nrow(
      data
    ) +
    3
}


# ------------------------------------------------------------------------------
# 6. CREATE WORKBOOK
# ------------------------------------------------------------------------------

phase5_workbook <- createWorkbook(
  creator = "Pinyi Wang",
  title = "Learner Survey Integrated Phase 4A-4D Results",
  subject = "Phase 5 quantitative integration"
)


# ------------------------------------------------------------------------------
# 7. SUMMARY SHEET
# ------------------------------------------------------------------------------

addWorksheet(
  phase5_workbook,
  "Summary",
  gridLines = TRUE
)

write_sheet_title(
  phase5_workbook,
  "Summary",
  "Learner Survey: Integrated Phase 4A-4D Results",
  paste0(
    "The sequence from feedback experience to AI comfort, perceived ",
    "usefulness, and preferred feedback model is an interpretive and ",
    "associational synthesis, not a causal or mediation model."
  )
)

summary_row <- 4

summary_row <- write_simple_section(
  phase5_workbook,
  "Summary",
  "Executive Summary",
  phase5_executive_summary,
  summary_row,
  body_row_height = 75
)

summary_row <- write_simple_section(
  phase5_workbook,
  "Summary",
  "Cross-Phase Synthesis",
  phase5_cross_phase_synthesis,
  summary_row,
  body_row_height = 80
)

setColWidths(
  phase5_workbook,
  "Summary",
  cols = 1:5,
  widths = c(
    30,
    24,
    52,
    52,
    50
  )
)

freezePane(
  phase5_workbook,
  "Summary",
  firstActiveRow = 4
)


# ------------------------------------------------------------------------------
# 8. HEADLINE METRICS SHEET
# ------------------------------------------------------------------------------

addWorksheet(
  phase5_workbook,
  "Headline Metrics",
  gridLines = TRUE
)

write_sheet_title(
  phase5_workbook,
  "Headline Metrics",
  "Integrated Headline Metrics"
)

writeData(
  phase5_workbook,
  "Headline Metrics",
  simple_excel_ready(
    phase5_headline_metrics
  ),
  startRow = 3,
  startCol = 1,
  colNames = TRUE,
  headerStyle = column_header_style,
  borders = "none"
)

addStyle(
  phase5_workbook,
  "Headline Metrics",
  body_style,
  rows = 4:(
    3 +
      nrow(
        phase5_headline_metrics
      )
  ),
  cols = 1:7,
  gridExpand = TRUE,
  stack = TRUE
)

setRowHeights(
  phase5_workbook,
  "Headline Metrics",
  rows = 4:(
    3 +
      nrow(
        phase5_headline_metrics
      )
  ),
  heights = 48
)

setColWidths(
  phase5_workbook,
  "Headline Metrics",
  cols = 1:7,
  widths = c(
    12,
    29,
    38,
    24,
    50,
    50,
    46
  )
)

freezePane(
  phase5_workbook,
  "Headline Metrics",
  firstActiveRow = 4
)


# ------------------------------------------------------------------------------
# 9. PHASE 4A SHEET
# ------------------------------------------------------------------------------

addWorksheet(
  phase5_workbook,
  "Phase 4A",
  gridLines = TRUE
)

write_sheet_title(
  phase5_workbook,
  "Phase 4A",
  "Phase 4A: Feedback-Experience Scale Diagnostics"
)

phase4A_row <- 3

phase4A_row <- write_simple_section(
  phase5_workbook,
  "Phase 4A",
  "Headline Results",
  phase4A_headline_metrics_verified,
  phase4A_row,
  body_row_height = 42
)

phase4A_row <- write_simple_section(
  phase5_workbook,
  "Phase 4A",
  "Polychoric Correlation Matrix",
  phase4A_polychoric_excel,
  phase4A_row
)

phase4A_row <- write_simple_section(
  phase5_workbook,
  "Phase 4A",
  "Ordinal Item Diagnostics",
  phase4A_objects$ordinal_item_diagnostics,
  phase4A_row
)

phase4A_row <- write_simple_section(
  phase5_workbook,
  "Phase 4A",
  "Composite Summary",
  phase4A_objects$composite_summary,
  phase4A_row
)

phase4A_row <- write_simple_section(
  phase5_workbook,
  "Phase 4A",
  "Measurement Decision",
  phase4A_objects$measurement_decision,
  phase4A_row
)

setColWidths(
  phase5_workbook,
  "Phase 4A",
  cols = 1:10,
  widths = c(
    42,
    28,
    34,
    34,
    22,
    22,
    22,
    22,
    22,
    22
  )
)

freezePane(
  phase5_workbook,
  "Phase 4A",
  firstActiveRow = 3
)


# ------------------------------------------------------------------------------
# 10. PHASE 4B SHEET
# ------------------------------------------------------------------------------

addWorksheet(
  phase5_workbook,
  "Phase 4B",
  gridLines = TRUE
)

write_sheet_title(
  phase5_workbook,
  "Phase 4B",
  "Phase 4B: AI Comfort"
)

phase4B_row <- 3

phase4B_row <- write_simple_section(
  phase5_workbook,
  "Phase 4B",
  "Headline Summary",
  phase4B_headline_summary,
  phase4B_row,
  body_row_height = 46
)

phase4B_row <- write_simple_section(
  phase5_workbook,
  "Phase 4B",
  "Final Model Fit",
  phase4B_model_fit_verified,
  phase4B_row
)

phase4B_row <- write_simple_section(
  phase5_workbook,
  "Phase 4B",
  "Verified Model Effects",
  phase4B_effects_verified,
  phase4B_row
)

phase4B_row <- write_simple_section(
  phase5_workbook,
  "Phase 4B",
  "Adjusted Probabilities by AI Awareness",
  phase4B_objects$adjusted_probability_by_awareness,
  phase4B_row
)

phase4B_row <- write_simple_section(
  phase5_workbook,
  "Phase 4B",
  "Adjusted Probabilities by Feedback Experience and AI Awareness",
  phase4B_objects$
    adjusted_probability_by_feedback_and_awareness,
  phase4B_row
)

phase4B_row <- write_simple_section(
  phase5_workbook,
  "Phase 4B",
  "Final Analysis Decision",
  phase4B_objects$final_analysis_decision,
  phase4B_row
)

setColWidths(
  phase5_workbook,
  "Phase 4B",
  cols = 1:11,
  widths = c(
    46,
    34,
    25,
    22,
    22,
    22,
    22,
    22,
    22,
    22,
    22
  )
)

freezePane(
  phase5_workbook,
  "Phase 4B",
  firstActiveRow = 3
)


# ------------------------------------------------------------------------------
# 11. PHASE 4C SHEET
# ------------------------------------------------------------------------------

addWorksheet(
  phase5_workbook,
  "Phase 4C",
  gridLines = TRUE
)

write_sheet_title(
  phase5_workbook,
  "Phase 4C",
  "Phase 4C: Perceived AI Usefulness"
)

phase4C_row <- 3

phase4C_row <- write_simple_section(
  phase5_workbook,
  "Phase 4C",
  "Headline Summary",
  phase4C_headline_summary,
  phase4C_row,
  body_row_height = 46
)

phase4C_row <- write_simple_section(
  phase5_workbook,
  "Phase 4C",
  "Outcome Distribution",
  phase4C_objects$outcome_distribution,
  phase4C_row
)

phase4C_row <- write_simple_section(
  phase5_workbook,
  "Phase 4C",
  "Final Model Fit",
  phase4C_model_fit_verified,
  phase4C_row
)

phase4C_row <- write_simple_section(
  phase5_workbook,
  "Phase 4C",
  "Verified AI-Comfort Effects",
  phase4C_effects_verified,
  phase4C_row
)

phase4C_row <- write_simple_section(
  phase5_workbook,
  "Phase 4C",
  "Adjusted Agreement Probabilities by AI Comfort",
  phase4C_objects$predicted_probabilities,
  phase4C_row
)

phase4C_row <- write_simple_section(
  phase5_workbook,
  "Phase 4C",
  "Proportional-Odds Versus Location-Scale Comparison",
  phase4C_objects$proportional_vs_scale_comparison,
  phase4C_row
)

phase4C_row <- write_simple_section(
  phase5_workbook,
  "Phase 4C",
  "Feedback-Experience Addition Comparison",
  phase4C_objects$feedback_addition_comparison,
  phase4C_row
)

setColWidths(
  phase5_workbook,
  "Phase 4C",
  cols = 1:11,
  widths = c(
    44,
    34,
    24,
    22,
    22,
    22,
    22,
    22,
    22,
    22,
    22
  )
)

freezePane(
  phase5_workbook,
  "Phase 4C",
  firstActiveRow = 3
)


# ------------------------------------------------------------------------------
# 12. PHASE 4D SHEET
# ------------------------------------------------------------------------------

addWorksheet(
  phase5_workbook,
  "Phase 4D",
  gridLines = TRUE
)

write_sheet_title(
  phase5_workbook,
  "Phase 4D",
  "Phase 4D: Preferred Feedback Model"
)

phase4D_row <- 3

phase4D_row <- write_simple_section(
  phase5_workbook,
  "Phase 4D",
  "Headline Summary",
  phase4D_headline_summary,
  phase4D_row,
  body_row_height = 46
)

phase4D_row <- write_simple_section(
  phase5_workbook,
  "Phase 4D",
  "Preferred-Model Distribution",
  phase4D_objects$preferred_model_distribution,
  phase4D_row
)

phase4D_row <- write_simple_section(
  phase5_workbook,
  "Phase 4D",
  "Human-Review Summary",
  phase4D_objects$human_review_summary,
  phase4D_row
)

phase4D_row <- write_simple_section(
  phase5_workbook,
  "Phase 4D",
  "Final Model Selection",
  phase4D_objects$final_model_selection,
  phase4D_row
)

phase4D_row <- write_simple_section(
  phase5_workbook,
  "Phase 4D",
  "Report-Ready Multinomial Coefficients",
  phase4D_report_ready_coefficients,
  phase4D_row
)

phase4D_row <- write_simple_section(
  phase5_workbook,
  "Phase 4D",
  "Reduced-Model Likelihood-Ratio Comparisons",
  phase4D_objects$reduced_model_comparisons,
  phase4D_row
)

phase4D_row <- write_simple_section(
  phase5_workbook,
  "Phase 4D",
  "Grouped Adjusted Probabilities",
  phase4D_objects$grouped_probabilities,
  phase4D_row
)

phase4D_row <- write_simple_section(
  phase5_workbook,
  "Phase 4D",
  "Maximum-Likelihood Versus Bias-Reduced Estimates",
  phase4D_objects$estimator_comparison,
  phase4D_row
)

setColWidths(
  phase5_workbook,
  "Phase 4D",
  cols = 1:12,
  widths = c(
    48,
    39,
    36,
    24,
    24,
    24,
    24,
    24,
    24,
    24,
    24,
    24
  )
)

freezePane(
  phase5_workbook,
  "Phase 4D",
  firstActiveRow = 3
)


# ------------------------------------------------------------------------------
# 13. OPTIONAL FIGURES SHEET
# ------------------------------------------------------------------------------

phase5_figure_manifest <- tibble(
  phase = c(
    "Phase 4A",
    "Phase 4B",
    "Phase 4C",
    "Phase 4D"
  ),
  
  title = c(
    "Feedback-Experience Reliability Diagnostics",
    "Adjusted AI-Comfort Probability by AI Awareness",
    "Adjusted Perceived-Usefulness Probability by AI Comfort",
    "Adjusted Probabilities of Preferred Feedback Models"
  ),
  
  path = c(
    here(
      "output",
      "learner_scale_diagnostics",
      "learner_feedback_reliability_plot.png"
    ),
    
    here(
      "output",
      "learner_ai_comfort_ordinal",
      "final",
      "phase4B_adjusted_probability_by_awareness.png"
    ),
    
    here(
      "output",
      "learner_ai_usefulness_ordinal",
      "phase4C_adjusted_usefulness_probability.png"
    ),
    
    here(
      "output",
      "learner_preferred_model_multinomial",
      "phase4D_adjusted_preferred_model_probabilities.png"
    )
  )
) |>
  mutate(
    exists = file.exists(
      path
    )
  )

if (include_figures) {
  addWorksheet(
    phase5_workbook,
    "Figures",
    gridLines = TRUE
  )
  
  write_sheet_title(
    phase5_workbook,
    "Figures",
    "Report-Ready Figures"
  )
  
  figure_title_rows <- c(
    3,
    31,
    59,
    87
  )
  
  for (
    figure_index in seq_len(
      nrow(
        phase5_figure_manifest
      )
    )
  ) {
    current_row <- figure_title_rows[
      figure_index
    ]
    
    writeData(
      phase5_workbook,
      "Figures",
      paste0(
        phase5_figure_manifest$phase[
          figure_index
        ],
        ": ",
        phase5_figure_manifest$title[
          figure_index
        ]
      ),
      startRow = current_row,
      startCol = 1,
      colNames = FALSE
    )
    
    addStyle(
      phase5_workbook,
      "Figures",
      section_title_style,
      rows = current_row,
      cols = 1,
      stack = TRUE
    )
    
    if (
      phase5_figure_manifest$exists[
        figure_index
      ]
    ) {
      insertImage(
        phase5_workbook,
        "Figures",
        phase5_figure_manifest$path[
          figure_index
        ],
        startRow = current_row + 1,
        startCol = 1,
        width = 10.5,
        height = 6.4,
        units = "in",
        dpi = 300
      )
    } else {
      writeData(
        phase5_workbook,
        "Figures",
        "Figure file not found.",
        startRow = current_row + 1,
        startCol = 1,
        colNames = FALSE
      )
    }
  }
  
  setColWidths(
    phase5_workbook,
    "Figures",
    cols = 1:9,
    widths = 13
  )
}


# ------------------------------------------------------------------------------
# 14. SOURCE MANIFEST SHEET
# ------------------------------------------------------------------------------

phase5_source_manifest_simple <- phase5_source_paths |>
  transmute(
    source = source_name,
    type = "RDS analysis source",
    path = source_path,
    exists = file_exists,
    size_kb = round(
      file_size_kb,
      2
    )
  )

if (include_figures) {
  phase5_source_manifest_simple <- bind_rows(
    phase5_source_manifest_simple,
    
    phase5_figure_manifest |>
      transmute(
        source = paste0(
          phase,
          ": ",
          title
        ),
        type = "PNG figure",
        path,
        exists,
        size_kb = if_else(
          exists,
          round(
            file.info(
              path
            )$size / 1024,
            2
          ),
          NA_real_
        )
      )
  )
}

addWorksheet(
  phase5_workbook,
  "Sources",
  gridLines = TRUE
)

write_sheet_title(
  phase5_workbook,
  "Sources",
  "Analysis Source Manifest"
)

writeData(
  phase5_workbook,
  "Sources",
  phase5_source_manifest_simple,
  startRow = 3,
  startCol = 1,
  colNames = TRUE,
  headerStyle = column_header_style,
  borders = "none"
)

addStyle(
  phase5_workbook,
  "Sources",
  body_style,
  rows = 4:(
    3 +
      nrow(
        phase5_source_manifest_simple
      )
  ),
  cols = 1:5,
  gridExpand = TRUE,
  stack = TRUE
)

setRowHeights(
  phase5_workbook,
  "Sources",
  rows = 4:(
    3 +
      nrow(
        phase5_source_manifest_simple
      )
  ),
  heights = 30
)

setColWidths(
  phase5_workbook,
  "Sources",
  cols = 1:5,
  widths = c(
    45,
    22,
    95,
    12,
    14
  )
)

freezePane(
  phase5_workbook,
  "Sources",
  firstActiveRow = 4
)


# ------------------------------------------------------------------------------
# 15. SAVE FINAL WORKBOOK ONCE
# ------------------------------------------------------------------------------

saveWorkbook(
  phase5_workbook,
  phase5_final_workbook_path,
  overwrite = TRUE
)


# ------------------------------------------------------------------------------
# 16. VERIFY WITHOUT REWRITING THE WORKBOOK
# ------------------------------------------------------------------------------

expected_sheets <- c(
  "Summary",
  "Headline Metrics",
  "Phase 4A",
  "Phase 4B",
  "Phase 4C",
  "Phase 4D",
  if (include_figures) "Figures",
  "Sources"
)

actual_sheets <- getSheetNames(
  phase5_final_workbook_path
)

xlsx_contents <- unzip(
  phase5_final_workbook_path,
  list = TRUE
)

required_xlsx_parts <- c(
  "[Content_Types].xml",
  "xl/workbook.xml",
  "xl/_rels/workbook.xml.rels"
)

validation <- tibble(
  check = c(
    "Workbook exists",
    "Workbook size is greater than zero",
    "Worksheet names and order are correct",
    "Required XLSX components are present"
  ),
  
  passed = c(
    file.exists(
      phase5_final_workbook_path
    ),
    
    file.info(
      phase5_final_workbook_path
    )$size > 0,
    
    identical(
      actual_sheets,
      expected_sheets
    ),
    
    all(
      required_xlsx_parts %in%
        xlsx_contents$Name
    )
  )
)

print(
  validation,
  n = Inf
)

if (!all(validation$passed)) {
  stop(
    "At least one workbook validation check failed."
  )
}

cat(
  "\nPHASE 5 SIMPLE WORKBOOK COMPLETE\n",
  "File:\n",
  phase5_final_workbook_path,
  "\n\n",
  "Worksheets: ",
  length(
    actual_sheets
  ),
  "\n",
  "File size: ",
  round(
    file.info(
      phase5_final_workbook_path
    )$size / 1024 / 1024,
    2
  ),
  " MB\n",
  sep = ""
)


