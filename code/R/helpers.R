# helpers.R
# Purpose: Shared utility functions for formatting, workbook naming, model-error
#          messages, support-table placeholders, dated output names, and y-axis scaling.
# Used by: All notebooks (00–06) via _load_all.R or direct source.
# Main functions provided: format_n(), format_mean_sd_range(), format_age_range(),
#          make_pooled_sheet_name(), condition_message_or_na(), check_support_cell(),
#          filter_support_sheet_pooled(), make_output_name(), make_curve_y_axis()
# Expected inputs: Standard R objects (character, numeric, tibble).
# Main outputs: Formatted strings, filtered tibbles, y-axis break/limit lists.
# Notes for release: No file I/O; pure helper functions.


# Format sample sizes for workbook headers, e.g., "n=1,234".
# Used in: workbook_writers.R (02.Rmd)
format_n <- function(x) {
  paste0(
    "n=",
    format(
      x,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  )
}


# Format descriptive statistics as:
# mean (SD) [min, max]
# Used in: workbook_writers.R (02.Rmd)
format_mean_sd_range <- function(mean_x, sd_x, min_x, max_x) {
  paste0(
    round(mean_x, 2),
    " (", round(sd_x, 2), ") [",
    round(min_x, 2),
    ", ",
    round(max_x, 2),
    "]"
  )
}


# Format age ranges for workbook headers, e.g., "[45.2, 89.7]".
# Used in: workbook_writers.R (02.Rmd)
format_age_range <- function(min_x, max_x) {
  paste0(
    "[",
    round(min_x, 1),
    ", ",
    round(max_x, 1),
    "]"
  )
}


# Clean and truncate test names so they are valid Excel sheet names.
# Used in: workbook_writers.R (02.Rmd)
make_pooled_sheet_name <- function(test_x) {
  substr(
    janitor::make_clean_names(test_x),
    1,
    31
  )
}


# Return an error message if an error object exists; otherwise return NA.
# Used in model-fitting summaries so failed fits can be documented cleanly.
# Used in: model_fitting.R (01.Rmd)
condition_message_or_na <- function(error_x) {
  if (is.null(error_x)) {
    return(NA_character_)
  }
  conditionMessage(error_x)
}


# Placeholder for workbook cell highlighting.
# The pooled output currently does not apply cell-level support highlighting,
# so this always returns FALSE while keeping the workbook writer interface stable.
# Used in: workbook_writers.R (02.Rmd)
check_support_cell <- function(
    support_x,
    sex_x,
    age_x,
    years_x = NA_real_
) {
  FALSE
}


# Filter an optional support table to one test.
# Returns NULL when no support table is provided.
# Used in: workbook_writers.R (02.Rmd)
filter_support_sheet_pooled <- function(support_x, test_x) {
  if (is.null(support_x)) {
    return(NULL)
  }
  support_x |>
    dplyr::filter(test == test_x)
}


# Create dated output filenames, e.g., "table4_class2_06232026.xlsx".
# Used in: 00.Rmd, 01.Rmd, 02.Rmd, 05.Rmd, 06.Rmd
make_output_name <- function(file_name, ext) {
  paste0(file_name, "_", format(Sys.Date(), "%m%d%Y"), ".", ext)
}


# Compute y-axis limits and pretty breaks from a prediction curve.
# Used by Class 1 and Class 2 life-course plots so plots can share
# consistent scaling based on the predicted score range.
# Used in: 03.Rmd, 04.Rmd
make_curve_y_axis <- function(
    curve_x,
    y_var = "predicted_score",
    padding_prop = 0.10,
    n_breaks = 5
) {
  y <- curve_x[[y_var]]
  y <- y[is.finite(y)]
  
  y_range <- range(y, na.rm = TRUE)
  y_span <- diff(y_range)
  
  # Avoid zero-width y-axis limits when the curve is flat.
  if (y_span == 0) {
    y_span <- max(abs(y_range[1]) * 0.05, 1)
  }
  
  y_limits_raw <- c(
    y_range[1] - padding_prop * y_span,
    y_range[2] + padding_prop * y_span
  )
  
  y_breaks <- scales::breaks_pretty(n = n_breaks)(y_limits_raw)
  
  list(
    limits = range(y_breaks),
    breaks = y_breaks
  )
}