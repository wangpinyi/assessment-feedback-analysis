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

### Reliability analysis
four_item_alpha_object <- psych::alpha(
  as.data.frame(feedback_numeric),
  check.keys = FALSE,
  warnings = FALSE
)

### inspect the overall results
four_item_alpha_object <- psych::alpha(
  as.data.frame(feedback_numeric),
  check.keys = FALSE,
  warnings = FALSE
)


### Inspect the item-level results
four_item_alpha_object$item.stats
four_item_alpha_object$alpha.drop


### Build a clean diagnostic table
reliability_diagnostics <- tibble(
  item = feedback_items,
  item_label = unname(feedback_item_labels[feedback_items]),
  
  corrected_item_total_correlation =
    four_item_alpha_object$item.stats$r.drop,
  
  alpha_if_item_deleted =
    four_item_alpha_object$alpha.drop$raw_alpha
) |>
  mutate(
    overall_alpha =
      four_item_alpha_object$total$raw_alpha
  )

print(
  reliability_diagnostics,
  n = Inf,
  width = Inf
)


### Save
readr::write_csv(
  reliability_diagnostics,
  file.path(
    scale_diagnostics_dir,
    "learner_feedback_reliability_diagnostics.csv"
  ),
  na = ""
)



#-------------------------------
### Reshape for plotting
#-------------------------------
reliability_plot_data <- reliability_diagnostics |>
  select(
    item_label,
    corrected_item_total_correlation,
    alpha_if_item_deleted,
    overall_alpha
  ) |>
  pivot_longer(
    cols = c(
      corrected_item_total_correlation,
      alpha_if_item_deleted
    ),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    metric = recode(
      metric,
      corrected_item_total_correlation =
        "Corrected item–total correlation",
      alpha_if_item_deleted =
        "Alpha if item deleted"
    ),
    
    item_label = factor(
      item_label,
      levels = rev(c(
        "Rubric clarity",
        "Specificity and relevance",
        "Improvement guidance",
        "Timeliness"
      ))
    )
  )

print(
  reliability_plot_data,
  n = Inf,
  width = Inf
)

### Creat the UF themed plot
uf_blue <- "#0021A5"
uf_orange <- "#FA4616"

overall_alpha_value <- four_item_alpha_object$total$raw_alpha

reliability_plot <- ggplot(
  reliability_plot_data,
  aes(
    x = value,
    y = item_label,
    color = metric
  )
) +
  geom_segment(
    aes(
      x = 0,
      xend = value,
      y = item_label,
      yend = item_label
    ),
    linewidth = 1
  ) +
  geom_point(
    size = 4
  ) +
  facet_wrap(
    ~ metric,
    ncol = 1,
    scales = "free_x"
  ) +
  geom_vline(
    data = tibble(
      metric = c(
        "Corrected item–total correlation",
        "Alpha if item deleted"
      ),
      reference_value = c(
        0.30,
        overall_alpha_value
      )
    ),
    aes(xintercept = reference_value),
    inherit.aes = FALSE,
    linetype = "dashed",
    linewidth = 0.8,
    color = "gray40"
  ) +
  geom_text(
    aes(
      label = sprintf("%.2f", value)
    ),
    hjust = -0.15,
    size = 4,
    color = "black"
  ) +
  scale_color_manual(
    values = c(
      "Corrected item–total correlation" = uf_blue,
      "Alpha if item deleted" = uf_orange
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Reliability Diagnostics for the Feedback-Experience Items",
    subtitle = paste0(
      "Overall Cronbach’s alpha = ",
      sprintf("%.2f", overall_alpha_value),
      "; dashed lines show the reference threshold (r = .30) and the overall alpha"
    ),
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      size = 11
    ),
    plot.margin = margin(10, 20, 10, 10)
  )

print(reliability_plot)


### Save the plot
ggsave(
  filename = file.path(
    scale_diagnostics_dir,
    "learner_feedback_reliability_plot.png"
  ),
  plot = reliability_plot,
  width = 9,
  height = 7,
  dpi = 300
)

ggsave(
  filename = file.path(
    scale_diagnostics_dir,
    "learner_feedback_reliability_plot.pdf"
  ),
  plot = reliability_plot,
  width = 9,
  height = 7
)


# ==============================================================================
# Calculate ordinal alpha and item diagnostics from the polychoric matrix
# ==============================================================================

feedback_items <- c(
  "rubric_clear_aligned",
  "feedback_specific_relevant",
  "feedback_helped_improve",
  "feedback_timely"
)

feedback_item_labels <- c(
  rubric_clear_aligned = "Rubric clarity",
  feedback_specific_relevant = "Specificity and relevance",
  feedback_helped_improve = "Improvement guidance",
  feedback_timely = "Timeliness"
)

# Confirm that matrix rows and columns match the item order
polychoric_matrix <- polychoric_matrix[
  feedback_items,
  feedback_items
]

number_of_items <- ncol(polychoric_matrix)

# ------------------------------------------------------------------------------
# Overall ordinal alpha
# ------------------------------------------------------------------------------

average_polychoric_correlation <- mean(
  polychoric_matrix[
    lower.tri(polychoric_matrix)
  ],
  na.rm = TRUE
)

ordinal_alpha <- (
  number_of_items *
    average_polychoric_correlation
) / (
  1 +
    (number_of_items - 1) *
    average_polychoric_correlation
)

# ------------------------------------------------------------------------------
# Ordinal item-rest correlations
# ------------------------------------------------------------------------------

ordinal_item_rest <- purrr::map_dbl(
  seq_len(number_of_items),
  function(item_number) {
    
    remaining_items <- setdiff(
      seq_len(number_of_items),
      item_number
    )
    
    covariance_with_rest <- sum(
      polychoric_matrix[
        item_number,
        remaining_items
      ],
      na.rm = TRUE
    )
    
    variance_of_rest <- sum(
      polychoric_matrix[
        remaining_items,
        remaining_items
      ],
      na.rm = TRUE
    )
    
    covariance_with_rest /
      sqrt(variance_of_rest)
  }
)

names(ordinal_item_rest) <- feedback_items

# ------------------------------------------------------------------------------
# Ordinal alpha if each item is deleted
# ------------------------------------------------------------------------------

ordinal_alpha_if_deleted <- purrr::map_dbl(
  seq_len(number_of_items),
  function(item_number) {
    
    reduced_matrix <- polychoric_matrix[
      -item_number,
      -item_number,
      drop = FALSE
    ]
    
    reduced_item_count <- ncol(
      reduced_matrix
    )
    
    reduced_average_correlation <- mean(
      reduced_matrix[
        lower.tri(reduced_matrix)
      ],
      na.rm = TRUE
    )
    
    (
      reduced_item_count *
        reduced_average_correlation
    ) / (
      1 +
        (reduced_item_count - 1) *
        reduced_average_correlation
    )
  }
)

names(ordinal_alpha_if_deleted) <-
  feedback_items


### verify the calculation
cat(
  "Average polychoric correlation:",
  round(
    average_polychoric_correlation,
    3
  ),
  "\n"
)

cat(
  "Overall ordinal alpha:",
  round(
    ordinal_alpha,
    3
  ),
  "\n"
)

print(
  round(
    ordinal_item_rest,
    3
  )
)

print(
  round(
    ordinal_alpha_if_deleted,
    3
  )
)


### Diagnostic table
ordinal_reliability_diagnostics <- tibble(
  item = feedback_items,
  
  item_label = unname(
    feedback_item_labels[
      feedback_items
    ]
  ),
  
  ordinal_item_rest_correlation =
    unname(
      ordinal_item_rest[
        feedback_items
      ]
    ),
  
  ordinal_alpha_if_deleted =
    unname(
      ordinal_alpha_if_deleted[
        feedback_items
      ]
    )
)

print(
  ordinal_reliability_diagnostics,
  n = Inf,
  width = Inf
)