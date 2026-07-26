# ==============================================================================
# Learner Experience Survey: Phase 4A
# Feedback-experience scale diagnostics and visualizations
# ==============================================================================

rm(list = ls())
graphics.off()
options(scipen = 999)

library(tidyverse)
library(here)

here::i_am(
  "scripts/04A_scale_diagnostics_learner.R"
)

learner <- readRDS(
  here::here(
    "data_processed",
    "learner_analysis_phase3_descriptive_ready.rds"
  )
)
cat("Rows:", nrow(learner), "\n")
cat("Columns:", ncol(learner), "\n")

stopifnot(nrow(learner) == 170)


### Create an output directory
scale_diagnostics_dir <- here::here(
  "output",
  "learner_scale_diagnostics"
)

dir.create(
  scale_diagnostics_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.exists(scale_diagnostics_dir)


### Select the four feedback items
feedback_items <- c(
  "rubric_clear_aligned",
  "feedback_specific_relevant",
  "feedback_helped_improve",
  "feedback_timely"
)

feedback_item_labels <- c(
  rubric_clear_aligned =
    "Rubric clarity",
  
  feedback_specific_relevant =
    "Specificity and relevance",
  
  feedback_helped_improve =
    "Improvement guidance",
  
  feedback_timely =
    "Timeliness"
)

feedback_scale_data <- learner |>
  select(
    all_of(feedback_items)
  )


### Install and load the package
library(psych)


### Convert ordered responses to integer scores
feedback_numeric <- feedback_scale_data |>
  mutate(
    across(
      everything(),
      ~ as.integer(.x)
    )
  )


### Calculate the polychoric correlation matrix
polychoric_result <- psych::polychoric(
  as.data.frame(feedback_numeric)
)

polychoric_matrix <- polychoric_result$rho

print(
  round(
    polychoric_matrix,
    3
  )
)


### Convert the matrix into heatmap data
library(tidyverse)
library(stringr)
library(scales)

label_order <- c(
  "Rubric clarity",
  "Specificity and relevance",
  "Improvement guidance",
  "Timeliness"
)

polychoric_heatmap_data <- as.data.frame(polychoric_matrix) |>
  tibble::rownames_to_column(var = "item_row") |>
  pivot_longer(
    cols = -item_row,
    names_to = "item_column",
    values_to = "correlation"
  ) |>
  mutate(
    item_row = recode(
      item_row,
      rubric_clear_aligned = "Rubric clarity",
      feedback_specific_relevant = "Specificity and relevance",
      feedback_helped_improve = "Improvement guidance",
      feedback_timely = "Timeliness"
    ),
    item_column = recode(
      item_column,
      rubric_clear_aligned = "Rubric clarity",
      feedback_specific_relevant = "Specificity and relevance",
      feedback_helped_improve = "Improvement guidance",
      feedback_timely = "Timeliness"
    )
  ) |>
  mutate(
    row_num = match(item_row, label_order),
    col_num = match(item_column, label_order)
  ) |>
  filter(row_num > col_num) |>
  mutate(
    item_row = factor(item_row, levels = rev(label_order)),
    item_column = factor(item_column, levels = label_order)
  )

polychoric_heatmap_uf <- ggplot(
  polychoric_heatmap_data,
  aes(
    x = item_column,
    y = item_row,
    fill = correlation
  )
) +
  geom_tile(
    color = "white",
    linewidth = 1
  ) +
  geom_text(
    aes(label = sprintf("%.2f", correlation)),
    size = 5,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_gradient2(
    low = "orange",     # s  UF orange  
    mid = "pink",     # light neutral
    high = "#0021A5",    # UF blue
    midpoint = 0.75,
    limits = c(0.50, 1.00),
    breaks = seq(0.5, 1.0, 0.1),
    labels = function(x) sprintf("%.1f", x),
    name = "Correlation"
  ) +
  coord_fixed() +
  labs(
    title = "Polychoric Correlations Among Feedback-Experience Items",
    subtitle = "Unique off-diagonal correlations among the four ordinal feedback items",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 20,
      hjust = 1,
      size = 11,
      color = "black"
    ),
    axis.text.y = element_text(
      size = 11,
      color = "black"
    ),
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0
    ),
    plot.subtitle = element_text(
      size = 11,
      hjust = 0
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  )

print(polychoric_heatmap_uf)