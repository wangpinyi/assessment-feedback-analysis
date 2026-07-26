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