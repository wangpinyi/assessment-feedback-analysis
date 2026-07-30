# ==============================================================================
# INSTRUCTOR OUTPUTS: COMBINE INTO ONE PLAIN, REPORT-READY EXCEL WORKBOOK
# ==============================================================================


library(openxlsx)
library(dplyr)
library(tibble)
library(here)

# ------------------------------------------------------------------------------
# 1. FILE PATHS
# ------------------------------------------------------------------------------

output_dir <- here("output", "instructor", "tables")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

files <- c(
  phase1 = file.path(output_dir, "instructor_phase1_quality_checks.xlsx"),
  phase3 = file.path(output_dir, "instructor_phase3_descriptive_tables.xlsx"),
  phase4A = file.path(output_dir, "instructor_phase4A_evaluation_diagnostics.xlsx"),
  phase4BC = file.path(output_dir, "instructor_phase4B_4C_results.xlsx"),
  phase6 = file.path(output_dir, "instructor_phase6_joint_display_template.xlsx")
)

missing_files <- files[!file.exists(files)]

if (length(missing_files) > 0) {
  stop(
    "These source files were not found:\n",
    paste(missing_files, collapse = "\n"),
    call. = FALSE
  )
}

final_file <- file.path(
  output_dir,
  "instructor_integrated_analysis_results_FINAL.xlsx"
)

# ------------------------------------------------------------------------------
# 2. READ SOURCE SHEETS AS PLAIN DATA
# ------------------------------------------------------------------------------

read_sheet <- function(file, sheet) {
  read.xlsx(
    file,
    sheet = sheet,
    check.names = FALSE,
    skipEmptyRows = TRUE,
    skipEmptyCols = TRUE,
    na.strings = c("", "NA")
  ) |>
    as_tibble()
}

p1_quality <- read_sheet(files[["phase1"]], "Quality_Summary")
p1_missing <- read_sheet(files[["phase1"]], "Missingness")
p1_dictionary <- read_sheet(files[["phase1"]], "Question_Dictionary")

p3_open_text <- read_sheet(files[["phase3"]], "Open_Text_Counts")
p3_frequencies <- read_sheet(files[["phase3"]], "All_Frequencies")

p4a_reliability <- read_sheet(files[["phase4A"]], "Reliability_Summary")
p4a_item_stats <- read_sheet(files[["phase4A"]], "Item_Statistics")
p4a_alpha_deleted <- read_sheet(files[["phase4A"]], "Alpha_If_Deleted")
p4a_spearman_matrix <- read_sheet(files[["phase4A"]], "Spearman_Matrix")
p4a_pairwise <- read_sheet(files[["phase4A"]], "Spearman_Pairwise_Tests")
p4a_polychoric <- read_sheet(files[["phase4A"]], "Polychoric_Sensitivity")
p4a_notes <- read_sheet(files[["phase4A"]], "Method_Note") |>
  rename(topic = issue) |>
  mutate(phase = "Phase 4A", .before = 1)

p4b_spearman <- read_sheet(files[["phase4BC"]], "Phase4B_Spearman")
p4b_fisher <- read_sheet(files[["phase4BC"]], "Phase4B_Fisher_Sensitivity")
p4c_models <- read_sheet(files[["phase4BC"]], "Phase4C_Bias_Reduced_Models")
p4bc_notes <- read_sheet(files[["phase4BC"]], "Method_Notes") |>
  rename(topic = analysis) |>
  mutate(phase = "Phases 4B-4C", .before = 1)

joint_display <- read_sheet(files[["phase6"]], "Joint_Display")

# Combine item statistics and alpha-if-deleted information.
p4a_item_diagnostics <- p4a_item_stats |>
  left_join(
    p4a_alpha_deleted |>
      transmute(
        item,
        raw_alpha_if_deleted = raw_alpha,
        standardized_alpha_if_deleted = std.alpha,
        average_r_if_deleted = average_r
      ),
    by = "item"
  )

method_notes <- bind_rows(p4a_notes, p4bc_notes)

# ------------------------------------------------------------------------------
# 3. CREATE REPORT-LEVEL SYNTHESIS TABLES
# ------------------------------------------------------------------------------

analytic_n <- p1_quality |>
  filter(quality_indicator == "Primary quantitative analytic sample") |>
  pull(n) |>
  as.numeric()

freq_stat <- function(variable_name, responses) {
  p3_frequencies |>
    filter(
      variable == variable_name,
      response %in% responses
    ) |>
    summarise(
      n = sum(as.numeric(n), na.rm = TRUE),
      percent = sum(as.numeric(percent_valid), na.rm = TRUE)
    )
}

stat <- list(
  cohorts = freq_stat("instructor_cohorts", "6 or more cohorts"),
  experience = freq_stat("educator_experience", "20+"),
  prior_ai_grading = freq_stat("prior_ai_grading_tool", "No"),
  short_review = freq_stat("review_time", c("3-5 minutes", "6-10 minutes")),
  time_saved = freq_stat("time_impact", c("Saved some time", "Saved significant time")),
  workflow = freq_stat("workflow_fit", c("Somewhat agree", "Strongly agree")),
  alignment = freq_stat("rubric_alignment", "Agree"),
  clarity = freq_stat("feedback_clarity", "Agree"),
  accuracy = freq_stat("score_accuracy", c("Good", "Excellent")),
  trust = freq_stat("score_trust", c("Somewhat agree", "Strongly agree")),
  appropriate = freq_stat("current_model_appropriate", c("Somewhat agree", "Strongly agree")),
  continue = freq_stat(
    "continuation_recommendation",
    c("Continue as-is", "Continue with modifications (please describe)")
  ),
  universal_review = freq_stat("preferred_model_group", "Universal human review"),
  human_role = freq_stat(
    "preferred_model_group",
    c("Human-led grading", "Universal human review", "Limited human review")
  )
)

headline_metrics <- tribble(
  ~metric, ~n, ~percent, ~interpretation,
  "Primary quantitative analytic sample", analytic_n, NA_real_,
  "Instructor responses included in the primary quantitative analyses.",
  "Taught six or more cohorts", stat$cohorts$n, stat$cohorts$percent,
  "The sample primarily represented highly experienced program instructors.",
  "Twenty or more years of educator experience", stat$experience$n, stat$experience$percent,
  "Most respondents had extensive educator or instructional experience.",
  "No prior AI grading-tool experience", stat$prior_ai_grading$n, stat$prior_ai_grading$percent,
  "Most respondents entered the implementation without prior AI grading-tool use.",
  "Reviewed submissions in 10 minutes or less", stat$short_review$n, stat$short_review$percent,
  "About half reported relatively short review times.",
  "Reported some or significant time savings", stat$time_saved$n, stat$time_saved$percent,
  "Perceived efficiency was one of the strongest favorable implementation indicators.",
  "Reported positive workflow fit", stat$workflow$n, stat$workflow$percent,
  "Workflow fit was favorable for a slim majority, with substantial neutral or negative responses remaining.",
  "Reported positive rubric alignment", stat$alignment$n, stat$alignment$percent,
  "Most instructors viewed the AI feedback as aligned with the rubric.",
  "Reported clear, specific, constructive feedback", stat$clarity$n, stat$clarity$percent,
  "Feedback clarity was generally favorable but not uniformly positive.",
  "Rated score accuracy good or excellent", stat$accuracy$n, stat$accuracy$percent,
  "Most instructors rated AI-generated score accuracy favorably.",
  "Reported positive trust in AI scores", stat$trust$n, stat$trust$percent,
  "Trust in AI scores as a review starting point was high.",
  "Agreed that the current model was appropriate", stat$appropriate$n, stat$appropriate$percent,
  "Most instructors viewed the current implementation model favorably.",
  "Recommended continuing the model", stat$continue$n, stat$continue$percent,
  "This combines continuation as-is and continuation with modifications.",
  "Preferred universal human review", stat$universal_review$n, stat$universal_review$percent,
  "Universal human review was the single most common future-model preference.",
  "Preferred a model with an explicit human role", stat$human_role$n, stat$human_role$percent,
  "Most instructors retained an explicit human oversight role in their preferred model."
)

p_text <- function(p) {
  ifelse(
    as.numeric(p) < .001,
    "< .001",
    paste0("= ", sub("^0", "", sprintf("%.3f", as.numeric(p))))
  )
}

assoc_text <- function(label) {
  row <- p4b_spearman |>
    filter(interpretation == label) |>
    slice(1)

  paste0(
    "rho = ", sprintf("%.3f", row$spearman_rho),
    "; BH-adjusted p ", p_text(row$p_value_bh)
  )
}

model_text <- function(predictor_name) {
  row <- p4c_models |>
    filter(predictor == predictor_name) |>
    slice(1)

  paste0(
    "OR = ", sprintf("%.2f", row$odds_ratio),
    ", 95% CI [", sprintf("%.2f", row$confidence_low),
    ", ", sprintf("%.2f", row$confidence_high),
    "]; BH-adjusted p ", p_text(row$p_value_bh)
  )
}

executive_summary <- tribble(
  ~domain, ~key_evidence, ~interpretation, ~source,
  "Sample context",
  paste0(
    analytic_n, " instructors were included; ",
    sprintf("%.1f%%", stat$cohorts$percent), " had taught six or more cohorts and ",
    sprintf("%.1f%%", stat$experience$percent), " had 20 or more years of educator experience."
  ),
  "The results primarily reflect experienced instructors with substantial program and professional experience.",
  "Phases 1 and 3",
  "Efficiency and review time",
  paste0(
    sprintf("%.1f%%", stat$time_saved$percent),
    " reported some or significant time savings; ",
    assoc_text("Time saving versus model appropriateness"), "."
  ),
  "Perceived efficiency was closely connected to judgments about whether the current model was appropriate.",
  "Phases 3, 4B, and 4C",
  "Workflow integration",
  paste0(
    sprintf("%.1f%%", stat$workflow$percent),
    " reported positive workflow fit; ",
    assoc_text("Time saving versus workflow fit"), "."
  ),
  "Workflow integration was favorable for a slim majority, but meaningful implementation friction remained.",
  "Phases 3 and 4B",
  "Feedback evaluation",
  paste0(
    sprintf("%.1f%%", stat$alignment$percent), " reported positive rubric alignment; ",
    sprintf("%.1f%%", stat$clarity$percent), " reported positive feedback clarity; and ",
    sprintf("%.1f%%", stat$accuracy$percent), " rated score accuracy good or excellent."
  ),
  "The AI-supported feedback was generally evaluated favorably, although clarity and accuracy were less uniformly positive than alignment.",
  "Phases 3 and 4A",
  "Score trust",
  paste0(
    sprintf("%.1f%%", stat$trust$percent),
    " reported positive trust; ",
    assoc_text("Trust versus model appropriateness"), "."
  ),
  "Trust was high descriptively and was positively associated with perceived model appropriateness.",
  "Phases 3 and 4B",
  "Continuation",
  paste0(
    sprintf("%.1f%%", stat$continue$percent),
    " recommended continuing the model; appropriateness model: ",
    model_text("current_model_appropriate_score"), "."
  ),
  "Continuation support was substantial but not unconditional because some respondents requested modifications and one quarter recommended discontinuation.",
  "Phases 3 and 4C",
  "Human oversight preference",
  paste0(
    sprintf("%.1f%%", stat$human_role$percent),
    " preferred an explicit human role; universal human review was the most common single preference at ",
    sprintf("%.1f%%", stat$universal_review$percent), "."
  ),
  "The preferred future direction retained meaningful human oversight.",
  "Phase 3",
  "Measurement caution",
  paste0(
    "The four-item evaluation set had standardized alpha = ",
    sprintf("%.3f", p4a_reliability$standardized_alpha[[1]]), "."
  ),
  "Report the evaluation indicators as related but distinct dimensions; use the composite only as an exploratory sensitivity measure.",
  "Phase 4A"
)

cross_phase_synthesis <- tribble(
  ~analytic_domain, ~descriptive_evidence, ~analytic_evidence, ~provisional_interpretation,
  "Efficiency and review time",
  paste0(sprintf("%.1f%%", stat$time_saved$percent), " reported time savings."),
  paste0(
    assoc_text("Time saving versus model appropriateness"),
    "; continuation model: ", model_text("time_impact_score"), "."
  ),
  "Efficiency appears central to acceptance of the implementation model.",
  "Workflow integration",
  paste0(sprintf("%.1f%%", stat$workflow$percent), " reported positive workflow fit."),
  paste0(
    assoc_text("Time saving versus workflow fit"),
    "; continuation model: ", model_text("workflow_fit_score"), "."
  ),
  "Workflow fit was mixed and closely connected to perceived time savings.",
  "Feedback alignment and clarity",
  paste0(
    sprintf("%.1f%%", stat$alignment$percent), " reported positive alignment and ",
    sprintf("%.1f%%", stat$clarity$percent), " reported positive clarity."
  ),
  "Alignment and clarity were positively correlated in Phase 4A.",
  "Alignment and clarity should be reported as related but distinct outcomes.",
  "Score accuracy and trust",
  paste0(
    sprintf("%.1f%%", stat$accuracy$percent), " rated accuracy good or excellent and ",
    sprintf("%.1f%%", stat$trust$percent), " reported positive trust."
  ),
  paste0(
    assoc_text("Trust versus model appropriateness"),
    "; continuation model: ", model_text("score_trust_score"), "."
  ),
  "Trust was high even though a meaningful subgroup rated accuracy fair or poor.",
  "Appropriateness and continuation",
  paste0(
    sprintf("%.1f%%", stat$appropriate$percent), " viewed the model as appropriate and ",
    sprintf("%.1f%%", stat$continue$percent), " recommended continuation."
  ),
  paste0(
    "Appropriateness model: ", model_text("current_model_appropriate_score"),
    "; evaluation-index sensitivity model: ", model_text("ai_evaluation_index_10"), "."
  ),
  "Support for continuation was substantial but not unconditional.",
  "Preferred human oversight model",
  paste0(
    sprintf("%.1f%%", stat$human_role$percent), " preferred an explicit human role; ",
    sprintf("%.1f%%", stat$universal_review$percent), " selected universal human review."
  ),
  assoc_text("Model appropriateness versus preferred human oversight"),
  "Human oversight remained a central future-model preference.",
  "Accessibility, responsiveness, and implementation safeguards",
  "No direct quantitative scale was available for this domain.",
  "No quantitative inferential result was available.",
  "Complete this domain after Phase 5 qualitative coding."
)

# Populate the quantitative side of the Phase 6 joint display.
joint_display <- joint_display |>
  left_join(cross_phase_synthesis, by = "analytic_domain") |>
  mutate(
    quantitative_evidence = paste(descriptive_evidence, analytic_evidence),
    qualitative_evidence = "To be completed from Phase 5 open-ended coding.",
    integrated_interpretation = paste0(
      "Provisional quantitative interpretation: ",
      provisional_interpretation
    ),
    design_or_policy_recommendation = case_when(
      analytic_domain == "Efficiency and review time" ~
        "Preserve efficiency gains while identifying tasks that add review time.",
      analytic_domain == "Workflow integration" ~
        "Refine workflow steps for instructors reporting neutral or negative fit.",
      analytic_domain == "Feedback alignment and clarity" ~
        "Strengthen clarity and specificity while preserving rubric alignment.",
      analytic_domain == "Score accuracy and trust" ~
        "Retain human review and clarify that AI scores are a starting point.",
      analytic_domain == "Appropriateness and continuation" ~
        "Continue with targeted modifications rather than treating support as unconditional.",
      analytic_domain == "Preferred human oversight model" ~
        "Retain an explicit and visible human oversight role.",
      TRUE ~
        "Complete after Phase 5 qualitative coding."
    ),
    strength_of_evidence = case_when(
      analytic_domain == "Accessibility, responsiveness, and implementation safeguards" ~
        "Not assessed quantitatively",
      analytic_domain == "Preferred human oversight model" ~
        "Descriptive evidence; small sample",
      TRUE ~
        "Descriptive and small-sample analytic evidence"
    )
  ) |>
  select(
    analytic_domain,
    quantitative_evidence,
    qualitative_evidence,
    integrated_interpretation,
    design_or_policy_recommendation,
    strength_of_evidence
  )

workbook_guide <- tribble(
  ~section, ~details,
  "Purpose",
  "Consolidates instructor survey outputs from Phases 1, 3, 4A, 4B, 4C, and the Phase 6 joint-display template.",
  "Design",
  "Uses plain worksheets rather than themed Excel tables. Formatting is limited to bold headers, thin borders, filters, frozen header rows, wrapped text, and practical number formats.",
  "Phase 3 consolidation",
  "The complete All_Frequencies sheet is retained. The 15 separate one-variable frequency sheets are not duplicated because they contain the same results.",
  "Phase 4A caution",
  "The four evaluation indicators are related but distinct; the composite index is exploratory because internal consistency was modest.",
  "Phase 4C caution",
  "Continuation results are separate bias-reduced logistic models, not one multivariable causal model.",
  "Phase 6 status",
  "Quantitative evidence is populated. Qualitative evidence remains pending Phase 5 open-ended coding."
)

source_manifest <- tribble(
  ~phase, ~source_workbook, ~source_content, ~integrated_destination,
  "Phase 1", basename(files[["phase1"]]),
  "Quality summary, missingness, and question dictionary",
  "Phase 1 Quality; Phase 1 Missingness; Phase 1 Dictionary",
  "Phase 3", basename(files[["phase3"]]),
  "Quality summary, open-text counts, All_Frequencies, and 15 redundant variable-specific frequency sheets",
  "Phase 3 Frequencies; Phase 3 Open Text",
  "Phase 4A", basename(files[["phase4A"]]),
  "Reliability, item diagnostics, Spearman results, polychoric sensitivity results, and method notes",
  "Phase 4A sheets; Method Notes",
  "Phases 4B-4C", basename(files[["phase4BC"]]),
  "Operational associations, Fisher sensitivity tests, continuation models, and method notes",
  "Phase 4B sheets; Phase 4C Models; Method Notes",
  "Phase 6", basename(files[["phase6"]]),
  "Joint-display template",
  "Phase 6 Joint Display"
)

# ------------------------------------------------------------------------------
# 4. CREATE A SIMPLE, NON-THEMED WORKBOOK
# ------------------------------------------------------------------------------

wb <- createWorkbook(creator = "Instructor survey analysis")

style_title <- createStyle(fontSize = 14, textDecoration = "bold")
style_note <- createStyle(fontSize = 10, textDecoration = "italic", wrapText = TRUE)
style_header <- createStyle(
  textDecoration = "bold",
  border = "Bottom",
  borderStyle = "thin",
  wrapText = TRUE
)
style_wrap <- createStyle(wrapText = TRUE, valign = "top")
style_integer <- createStyle(numFmt = "0")
style_percent <- createStyle(numFmt = "0.0")
style_decimal <- createStyle(numFmt = "0.000")
style_two_decimal <- createStyle(numFmt = "0.00")
style_p <- createStyle(numFmt = '[<0.001]"< .001";0.000')

write_plain_sheet <- function(sheet, data, title, note = NULL) {
  addWorksheet(wb, sheet, gridLines = TRUE)

  writeData(wb, sheet, title, startRow = 1, colNames = FALSE)
  addStyle(wb, sheet, style_title, rows = 1, cols = 1)

  if (!is.null(note)) {
    writeData(wb, sheet, note, startRow = 2, colNames = FALSE)
    addStyle(wb, sheet, style_note, rows = 2, cols = 1)
  }

  header_row <- 4
  first_data_row <- 5

  writeData(
    wb,
    sheet,
    data,
    startRow = header_row,
    headerStyle = style_header,
    keepNA = FALSE
  )

  if (ncol(data) > 0) {
    setColWidths(wb, sheet, cols = seq_len(ncol(data)), widths = "auto")
    freezePane(wb, sheet, firstActiveRow = first_data_row)

    if (nrow(data) > 0) {
      addFilter(wb, sheet, row = header_row, cols = seq_len(ncol(data)))

      text_cols <- which(vapply(data, is.character, logical(1)))
      if (length(text_cols) > 0) {
        setColWidths(wb, sheet, cols = text_cols, widths = 35)
        addStyle(
          wb,
          sheet,
          style_wrap,
          rows = first_data_row:(first_data_row + nrow(data) - 1),
          cols = text_cols,
          gridExpand = TRUE,
          stack = TRUE
        )
      }

      data_rows <- first_data_row:(first_data_row + nrow(data) - 1)
      nm <- names(data)

      integer_cols <- which(grepl("(^n$|_n$|analytic_n|number_of_items)", nm))
      percent_cols <- which(grepl("percent", nm, ignore.case = TRUE))
      p_cols <- which(grepl("^p_value", nm, ignore.case = TRUE))
      two_cols <- which(grepl("odds_ratio|confidence_|aic$", nm, ignore.case = TRUE))
      numeric_cols <- which(vapply(data, is.numeric, logical(1)))
      decimal_cols <- setdiff(
        numeric_cols,
        unique(c(integer_cols, percent_cols, p_cols, two_cols))
      )

      if (length(integer_cols) > 0) {
        addStyle(wb, sheet, style_integer, data_rows, integer_cols,
                 gridExpand = TRUE, stack = TRUE)
      }
      if (length(percent_cols) > 0) {
        addStyle(wb, sheet, style_percent, data_rows, percent_cols,
                 gridExpand = TRUE, stack = TRUE)
      }
      if (length(p_cols) > 0) {
        addStyle(wb, sheet, style_p, data_rows, p_cols,
                 gridExpand = TRUE, stack = TRUE)
      }
      if (length(two_cols) > 0) {
        addStyle(wb, sheet, style_two_decimal, data_rows, two_cols,
                 gridExpand = TRUE, stack = TRUE)
      }
      if (length(decimal_cols) > 0) {
        addStyle(wb, sheet, style_decimal, data_rows, decimal_cols,
                 gridExpand = TRUE, stack = TRUE)
      }
    }
  }
}

# ------------------------------------------------------------------------------
# 5. WRITE THE FINAL WORKBOOK
# ------------------------------------------------------------------------------

write_plain_sheet(
  "Workbook Guide", workbook_guide,
  "Instructor Survey Integrated Analysis Workbook",
  "No themed tables, colored fills, charts, or decorative formatting are used."
)

write_plain_sheet(
  "Executive Summary", executive_summary,
  "Executive Summary",
  "Results are descriptive or associational and should not be interpreted as causal."
)

write_plain_sheet(
  "Cross-Phase Synthesis", cross_phase_synthesis,
  "Cross-Phase Quantitative Synthesis"
)

write_plain_sheet(
  "Headline Metrics", headline_metrics,
  "Headline Instructor Metrics",
  "Percentages are valid percentages shown on a 0-100 scale."
)

write_plain_sheet("Phase 1 Quality", p1_quality, "Phase 1: Data Quality Summary")
write_plain_sheet("Phase 1 Missingness", p1_missing, "Phase 1: Variable Missingness")
write_plain_sheet("Phase 1 Dictionary", p1_dictionary, "Phase 1: Question Dictionary")

write_plain_sheet(
  "Phase 3 Frequencies", p3_frequencies,
  "Phase 3: Consolidated Descriptive Frequencies",
  "This sheet replaces the redundant one-variable frequency sheets."
)
write_plain_sheet("Phase 3 Open Text", p3_open_text, "Phase 3: Open-Ended Response Counts")

write_plain_sheet("Phase 4A Reliability", p4a_reliability, "Phase 4A: Reliability Summary")
write_plain_sheet("Phase 4A Item Stats", p4a_item_diagnostics, "Phase 4A: Item Diagnostics")
write_plain_sheet("Phase 4A Spearman Matrix", p4a_spearman_matrix, "Phase 4A: Spearman Matrix")
write_plain_sheet("Phase 4A Correlations", p4a_pairwise, "Phase 4A: Pairwise Spearman Tests")
write_plain_sheet(
  "Phase 4A Polychoric", p4a_polychoric,
  "Phase 4A: Polychoric Sensitivity Analysis",
  "Retained as a sensitivity analysis because of sparse cells and unequal response categories."
)

write_plain_sheet("Phase 4B Associations", p4b_spearman, "Phase 4B: Operational Associations")
write_plain_sheet(
  "Phase 4B Fisher", p4b_fisher,
  "Phase 4B: Fisher Exact-Test Sensitivity Results",
  "Dichotomized predictors were used only for sensitivity analyses."
)
write_plain_sheet(
  "Phase 4C Models", p4c_models,
  "Phase 4C: Separate Bias-Reduced Continuation Models",
  "Each row is a separate model; no multivariable continuation model was estimated."
)

write_plain_sheet(
  "Phase 6 Joint Display", joint_display,
  "Phase 6: Joint Display",
  "Quantitative evidence is populated; qualitative evidence remains pending Phase 5 coding."
)

write_plain_sheet("Method Notes", method_notes, "Methodological and Interpretation Notes")
write_plain_sheet("Source Manifest", source_manifest, "Source Workbook Manifest")

# ------------------------------------------------------------------------------
# 6. SAVE AND VERIFY
# ------------------------------------------------------------------------------

saveWorkbook(wb, final_file, overwrite = TRUE)

expected_sheets <- c(
  "Workbook Guide", "Executive Summary", "Cross-Phase Synthesis",
  "Headline Metrics", "Phase 1 Quality", "Phase 1 Missingness",
  "Phase 1 Dictionary", "Phase 3 Frequencies", "Phase 3 Open Text",
  "Phase 4A Reliability", "Phase 4A Item Stats",
  "Phase 4A Spearman Matrix", "Phase 4A Correlations",
  "Phase 4A Polychoric", "Phase 4B Associations", "Phase 4B Fisher",
  "Phase 4C Models", "Phase 6 Joint Display", "Method Notes",
  "Source Manifest"
)

stopifnot(identical(getSheetNames(final_file), expected_sheets))

message("Saved successfully to: ", normalizePath(final_file, winslash = "/"))
