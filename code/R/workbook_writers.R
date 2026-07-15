# workbook_writers.R
# Purpose: Write formatted Excel workbooks (openxlsx) for the pooled 3-cohort
#          analysis: test dictionary, summary sheets, per-test Table 1/2/3
#          blocks, and Table 4 (spontaneous reversal).
# Used by: Notebooks 02 (Tables 1–3 workbook), 05 and 06 (Table 4 workbook),
#          07 (make_pooled_test_dictionary).
# Main functions provided: write_all_pooled_test_workbook_with_summary(),
#          write_table4_workbook(), make_pooled_test_dictionary(),
#          make_pooled_workbook_summary_sheets(),
#          append_summary_sheets_to_workbook(), plus per-test/block helpers.
# Expected inputs: Table tibbles from table_class1.R / table_class2.R, analysis data.
# Main outputs: Formatted .xlsx workbooks saved to ../output/tables/.
# Notes for release: Table 4 workbooks include a footnote explaining the reversal algorithm.



# --- Test dictionary ---

make_pooled_test_dictionary <- function(dat) {

  tibble::tribble(
    ~test,             ~test_label,                                      ~xprize_subdomain,                  ~range,
    "lmi",             "Logical Memory, immediate recall",                "Memory",                           "0-23",
    "lmd",             "Logical Memory, delayed recall",                  "Memory",                           "0-23",
    "lmr",             "Logical Memory, recognition",                     "Memory",                           "0-11",
    "vri",             "Visual Reproductions, immediate recall",          "Visual Learning",                  "0-14",
    "vrd",             "Visual Reproductions, delayed recall",            "Visual Learning",                  "0-14",
    "vrr",             "Visual Reproductions, recognition",               "Visual Learning",                  "0-14",
    "pasi",            "Paired Associates, immediate recall",             "Memory",                           "0-21",
    "pasd",            "Paired Associates, delayed recall",               "Memory",                           "0-10",
    "pasr",            "Paired Associates, recognition",                  "Memory",                           "0-10",
    "dsf",             "Digit Span Forward",                              "Attention",                        "0-9",
    "dsb",             "Digit Span Backward",                             "Executive Function & Processing",  "0-8",
    "trailsa",         "Trail Making Test Part A",                        "Psychomotor",                      "0.32-10.00 minutes; maximum time set at 10 minutes",
    "trailsb",         "Trail Making Test Part B",                        "Executive Function & Processing",  "0.32-10.00 minutes; maximum time set at 10 minutes",
    "coding_correct",  "WAIS-IV Coding, total correct",                   "Psychomotor",                      "0-119"
  ) |>
    dplyr::filter(
      test %in% unique(dat$test)
    ) |>
    dplyr::arrange(
      xprize_subdomain,
      test
    )
}


# --- Summary sheets ---

make_pooled_workbook_summary_sheets <- function(
    dat,
    test_dictionary_sheet,
    model_required_vars
) {

  data_summary_base <- dat |>
    dplyr::mutate(
      complete_for_model = complete.cases(
        dplyr::across(
          dplyr::all_of(model_required_vars)
        )
      )
    )

  subject_any_test_denominator <- data_summary_base |>
    dplyr::group_by(
      sex_label
    ) |>
    dplyr::summarise(
      n_subjects_any_test = dplyr::n_distinct(dbgap_subject_id),
      .groups = "drop"
    )

  subject_model_support <- data_summary_base |>
    dplyr::filter(
      complete_for_model
    ) |>
    dplyr::group_by(
      test,
      sex_label,
      dbgap_subject_id
    ) |>
    dplyr::summarise(
      n_model_visits = dplyr::n(),
      .groups = "drop"
    )

  data_summary_sheet <- subject_model_support |>
    dplyr::group_by(
      test,
      sex_label
    ) |>
    dplyr::summarise(
      sample_size = dplyr::n_distinct(dbgap_subject_id),
      datapoints = sum(n_model_visits),
      subjects_2plus_visit = sum(n_model_visits >= 2),
      pct_subjects_2plus = subjects_2plus_visit / sample_size,
      .groups = "drop"
    ) |>
    dplyr::left_join(
      subject_any_test_denominator,
      by = "sex_label"
    ) |>
    dplyr::mutate(
      sample_size_missing = n_subjects_any_test - sample_size,
      pct_sample_size_missing = sample_size_missing / n_subjects_any_test
    ) |>
    dplyr::left_join(
      test_dictionary_sheet |>
        dplyr::select(
          test,
          xprize_subdomain
        ),
      by = "test"
    ) |>
    dplyr::transmute(
      `XPrize subdomain` = xprize_subdomain,
      test,
      sex_label,
      `# sample size` = sample_size,
      `# datapoints` = datapoints,
      `# sample_size_missing` = sample_size_missing,
      `% sample_size_missing` = pct_sample_size_missing,
      `# subjects_2plus_visit` = subjects_2plus_visit,
      `% subjects_2plus` = pct_subjects_2plus
    ) |>
    dplyr::arrange(
      `XPrize subdomain`,
      test,
      sex_label
    )

  subject_followup_pooled <- data_summary_base |>
    dplyr::filter(
      complete_for_model
    ) |>
    dplyr::group_by(
      test,
      sex_label,
      dbgap_subject_id
    ) |>
    dplyr::summarise(
      n_visits = dplyr::n(),
      max_followup_year = max(t_followup, na.rm = TRUE),
      .groups = "drop"
    )

  followup_summary_sheet <- subject_followup_pooled |>
    dplyr::group_by(
      test,
      sex_label
    ) |>
    dplyr::summarise(
      `# of subject` = dplyr::n(),
      `# of subject with 1 visit` = sum(n_visits == 1),
      `# of subject with 2+ visit` = sum(n_visits >= 2),

      `Follow up time (Median [IQR, year])` = paste0(
        round(median(max_followup_year, na.rm = TRUE), 1),
        " [",
        round(quantile(max_followup_year, 0.25, na.rm = TRUE), 1),
        ", ",
        round(quantile(max_followup_year, 0.75, na.rm = TRUE), 1),
        "]"
      ),

      `Follow up time (Max, year)` = round(
        max(max_followup_year, na.rm = TRUE),
        1
      ),

      `# of subject with 5y+ followup` = sum(max_followup_year >= 5),
      `# of subject with 10y+ followup` = sum(max_followup_year >= 10),
      `# of subject with 15y+ followup` = sum(max_followup_year >= 15),
      `# of subject with 20y+ followup` = sum(max_followup_year >= 20),

      .groups = "drop"
    ) |>
    dplyr::left_join(
      test_dictionary_sheet |>
        dplyr::select(
          test,
          `XPrize subdomain` = xprize_subdomain
        ),
      by = "test"
    ) |>
    dplyr::transmute(
      `XPrize subdomain`,
      test,
      Sex = sex_label,
      `# of subject`,
      `# of subject with 1 visit`,
      `# of subject with 2+ visit`,
      `Follow up time (Median [IQR, year])`,
      `Follow up time (Max, year)`,
      `# of subject with 5y+ followup`,
      `# of subject with 10y+ followup`,
      `# of subject with 15y+ followup`,
      `# of subject with 20y+ followup`
    ) |>
    dplyr::arrange(
      `XPrize subdomain`,
      test,
      Sex
    )

  list(
    data_summary_sheet = data_summary_sheet,
    followup_summary_sheet = followup_summary_sheet
  )
}

# --- Header info ---

get_pooled_header_info <- function(dat, test_x) {

  dat |>
    dplyr::filter(
      test == test_x,
      complete_for_model
    ) |>
    dplyr::group_by(
      sex_label
    ) |>
    dplyr::summarise(
      n_subj = dplyr::n_distinct(dbgap_subject_id),
      mean_score = mean(score, na.rm = TRUE),
      sd_score = sd(score, na.rm = TRUE),
      min_score = min(score, na.rm = TRUE),
      max_score = max(score, na.rm = TRUE),
      min_age_at_measure = min(age_at_measure, na.rm = TRUE),
      max_age_at_measure = max(age_at_measure, na.rm = TRUE),
      min_baseline_age = min(baseline_age, na.rm = TRUE),
      max_baseline_age = max(baseline_age, na.rm = TRUE),
      .groups = "drop"
    )
}


# --- Table block writers (write into an existing workbook/sheet) ---

write_table1_block <- function(
    wb,
    sheet,
    start_col,
    start_row,
    block_title,
    table_x,
    header_x,
    age_col,
    women_m_col,
    women_se_col,
    men_m_col,
    men_se_col,
    support_x = NULL
) {

  title_style <- openxlsx::createStyle(textDecoration = "bold")

  center_style <- openxlsx::createStyle(
    halign = "center",
    valign = "center"
  )

  top_style <- openxlsx::createStyle(border = "Top")

  line_style <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    border = "TopBottom",
    textDecoration = "bold"
  )

  bottom_style <- openxlsx::createStyle(border = "Bottom")
  num_style <- openxlsx::createStyle(numFmt = "0.00")

  note_style <- openxlsx::createStyle(
    fontSize = 9,
    textDecoration = "italic",
    halign = "left",
    valign = "top",
    wrapText = TRUE
  )

  openxlsx::writeData(
    wb, sheet,
    block_title,
    startCol = start_col,
    startRow = start_row
  )

  openxlsx::addStyle(
    wb, sheet,
    title_style,
    rows = start_row,
    cols = start_col
  )

  openxlsx::writeData(
    wb, sheet,
    "Table 1. Means",
    startCol = start_col,
    startRow = start_row + 2
  )

  openxlsx::addStyle(
    wb, sheet,
    title_style,
    rows = start_row + 2,
    cols = start_col
  )

  openxlsx::writeData(
    wb, sheet,
    "Women",
    startCol = start_col + 1,
    startRow = start_row + 4
  )

  openxlsx::mergeCells(
    wb, sheet,
    cols = (start_col + 1):(start_col + 2),
    rows = start_row + 4
  )

  openxlsx::writeData(
    wb, sheet,
    "Men",
    startCol = start_col + 4,
    startRow = start_row + 4
  )

  openxlsx::mergeCells(
    wb, sheet,
    cols = (start_col + 4):(start_col + 5),
    rows = start_row + 4
  )

  openxlsx::addStyle(
    wb, sheet,
    top_style,
    rows = start_row + 4,
    cols = start_col:(start_col + 5),
    gridExpand = TRUE,
    stack = TRUE
  )

  women_header <- header_x |>
    dplyr::filter(sex_label == "Women")

  men_header <- header_x |>
    dplyr::filter(sex_label == "Men")

  if (age_col == "baseline_age") {

    women_age_range <- format_age_range(
      women_header$min_baseline_age,
      women_header$max_baseline_age
    )

    men_age_range <- format_age_range(
      men_header$min_baseline_age,
      men_header$max_baseline_age
    )

  } else {

    women_age_range <- format_age_range(
      women_header$min_age_at_measure,
      women_header$max_age_at_measure
    )

    men_age_range <- format_age_range(
      men_header$min_age_at_measure,
      men_header$max_age_at_measure
    )
  }

  openxlsx::writeData(
    wb, sheet,
    format_n(women_header$n_subj),
    startCol = start_col + 1,
    startRow = start_row + 5
  )

  openxlsx::mergeCells(
    wb, sheet,
    cols = (start_col + 1):(start_col + 2),
    rows = start_row + 5
  )

  openxlsx::writeData(
    wb, sheet,
    format_n(men_header$n_subj),
    startCol = start_col + 4,
    startRow = start_row + 5
  )

  openxlsx::mergeCells(
    wb, sheet,
    cols = (start_col + 4):(start_col + 5),
    rows = start_row + 5
  )

  openxlsx::writeData(
    wb, sheet,
    paste0("age range ", women_age_range),
    startCol = start_col + 1,
    startRow = start_row + 6
  )

  openxlsx::mergeCells(
    wb, sheet,
    cols = (start_col + 1):(start_col + 2),
    rows = start_row + 6
  )

  openxlsx::writeData(
    wb, sheet,
    paste0("age range ", men_age_range),
    startCol = start_col + 4,
    startRow = start_row + 6
  )

  openxlsx::mergeCells(
    wb, sheet,
    cols = (start_col + 4):(start_col + 5),
    rows = start_row + 6
  )

  openxlsx::writeData(
    wb, sheet,
    format_mean_sd_range(
      women_header$mean_score,
      women_header$sd_score,
      women_header$min_score,
      women_header$max_score
    ),
    startCol = start_col + 1,
    startRow = start_row + 7
  )

  openxlsx::mergeCells(
    wb, sheet,
    cols = (start_col + 1):(start_col + 2),
    rows = start_row + 7
  )

  openxlsx::writeData(
    wb, sheet,
    format_mean_sd_range(
      men_header$mean_score,
      men_header$sd_score,
      men_header$min_score,
      men_header$max_score
    ),
    startCol = start_col + 4,
    startRow = start_row + 7
  )

  openxlsx::mergeCells(
    wb, sheet,
    cols = (start_col + 4):(start_col + 5),
    rows = start_row + 7
  )

  openxlsx::addStyle(
    wb, sheet,
    center_style,
    rows = (start_row + 4):(start_row + 7),
    cols = start_col:(start_col + 5),
    gridExpand = TRUE,
    stack = TRUE
  )

  table_header <- c("Age", "M", "SE", "", "M", "SE")

  openxlsx::writeData(
    wb, sheet,
    t(table_header),
    startCol = start_col,
    startRow = start_row + 8,
    colNames = FALSE
  )

  openxlsx::addStyle(
    wb, sheet,
    line_style,
    rows = start_row + 8,
    cols = start_col:(start_col + 5),
    gridExpand = TRUE,
    stack = TRUE
  )

  table_out <- table_x |>
    dplyr::transmute(
      age = .data[[age_col]],
      women_m = .data[[women_m_col]],
      women_se = .data[[women_se_col]],
      blank = NA_real_,
      men_m = .data[[men_m_col]],
      men_se = .data[[men_se_col]]
    )

  openxlsx::writeData(
    wb, sheet,
    table_out,
    startCol = start_col,
    startRow = start_row + 9,
    colNames = FALSE
  )

  data_rows <- (start_row + 9):(start_row + 9 + nrow(table_out) - 1)
  last_row <- max(data_rows)

  openxlsx::addStyle(
    wb, sheet,
    num_style,
    rows = data_rows,
    cols = (start_col + 1):(start_col + 5),
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    wb, sheet,
    center_style,
    rows = data_rows,
    cols = start_col:(start_col + 5),
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    wb, sheet,
    bottom_style,
    rows = last_row,
    cols = start_col:(start_col + 5),
    gridExpand = TRUE,
    stack = TRUE
  )

  note_row <- last_row + 1

  openxlsx::writeData(
    wb, sheet,
    paste(
      "Header statistics are shown as mean (SD) [range] for the observed",
      "test scores within each sex-specific analytic sample."
    ),
    startCol = start_col,
    startRow = note_row
  )

  openxlsx::mergeCells(
    wb, sheet,
    cols = start_col:(start_col + 5),
    rows = note_row
  )

  openxlsx::addStyle(
    wb, sheet,
    note_style,
    rows = note_row,
    cols = start_col:(start_col + 5),
    gridExpand = TRUE
  )

  openxlsx::setRowHeights(wb, sheet, rows = note_row, heights = 30)
}


write_table2_block <- function(
    wb,
    sheet,
    start_col,
    start_row,
    table_x,
    support_x = NULL
) {

  title_style <- openxlsx::createStyle(textDecoration = "bold")

  center_style <- openxlsx::createStyle(
    halign = "center",
    valign = "center"
  )

  header_center_style <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    textDecoration = "bold"
  )

  header_top_style <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    border = "Top",
    textDecoration = "bold"
  )

  header_bottom_style <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    border = "Bottom",
    textDecoration = "bold"
  )

  sex_style <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    textDecoration = "bold",
    border = "TopBottom"
  )

  bottom_style <- openxlsx::createStyle(border = "Bottom")
  gray_style <- openxlsx::createStyle(fgFill = "#D9D9D9")
  num_style <- openxlsx::createStyle(numFmt = "0.00")

  ages <- index_ages_table2
  yrs <- change_years

  openxlsx::writeData(
    wb, sheet,
    "Table 2. Estimated marginal changes per 5, 10, 15, and 20y of follow-up",
    startCol = start_col,
    startRow = start_row
  )

  openxlsx::addStyle(
    wb, sheet,
    title_style,
    rows = start_row,
    cols = start_col
  )

  openxlsx::writeData(
    wb, sheet,
    "Years of Change",
    startCol = start_col + 1,
    startRow = start_row + 1
  )

  openxlsx::mergeCells(
    wb, sheet,
    cols = (start_col + 1):(start_col + 8),
    rows = start_row + 1
  )

  openxlsx::writeData(
    wb, sheet,
    t(c("", "5", "", "10", "", "15", "", "20", "")),
    startCol = start_col,
    startRow = start_row + 2,
    colNames = FALSE
  )

  openxlsx::mergeCells(wb, sheet, cols = (start_col + 1):(start_col + 2), rows = start_row + 2)
  openxlsx::mergeCells(wb, sheet, cols = (start_col + 3):(start_col + 4), rows = start_row + 2)
  openxlsx::mergeCells(wb, sheet, cols = (start_col + 5):(start_col + 6), rows = start_row + 2)
  openxlsx::mergeCells(wb, sheet, cols = (start_col + 7):(start_col + 8), rows = start_row + 2)

  openxlsx::writeData(
    wb, sheet,
    t(c("Age", "Est.", "SE", "Est.", "SE", "Est.", "SE", "Est.", "SE")),
    startCol = start_col,
    startRow = start_row + 3,
    colNames = FALSE
  )

  openxlsx::addStyle(
    wb, sheet,
    header_center_style,
    rows = (start_row + 1):(start_row + 3),
    cols = start_col:(start_col + 8),
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    wb, sheet,
    header_top_style,
    rows = start_row + 1,
    cols = start_col:(start_col + 8),
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    wb, sheet,
    header_bottom_style,
    rows = start_row + 3,
    cols = start_col:(start_col + 8),
    gridExpand = TRUE,
    stack = TRUE
  )

  write_one_sex <- function(sex_x, sex_row) {

    openxlsx::writeData(
      wb, sheet,
      toupper(sex_x),
      startCol = start_col,
      startRow = sex_row,
      colNames = FALSE
    )

    openxlsx::mergeCells(
      wb, sheet,
      cols = start_col:(start_col + 8),
      rows = sex_row
    )

    openxlsx::addStyle(
      wb, sheet,
      sex_style,
      rows = sex_row,
      cols = start_col:(start_col + 8),
      gridExpand = TRUE,
      stack = TRUE
    )

    data_x <- table_x |>
      dplyr::filter(sex_label == sex_x) |>
      dplyr::select(
        age,
        est_5, se_5,
        est_10, se_10,
        est_15, se_15,
        est_20, se_20
      )

    out_x <- tibble::tibble(age = ages) |>
      dplyr::left_join(data_x, by = "age")

    for (i in seq_along(ages)) {

      age_i <- ages[i]
      row_i <- sex_row + i

      openxlsx::writeData(
        wb, sheet,
        age_i,
        startCol = start_col,
        startRow = row_i,
        colNames = FALSE
      )

      for (j in seq_along(yrs)) {

        yr_j <- yrs[j]
        end_age_j <- age_i + yr_j
        est_col <- paste0("est_", yr_j)
        se_col <- paste0("se_", yr_j)
        cell_col <- start_col + 1 + (j - 1) * 2

        if (end_age_j < min(index_ages_table1) | end_age_j > max(index_ages_table1)) {

          openxlsx::addStyle(
            wb, sheet,
            gray_style,
            rows = row_i,
            cols = cell_col:(cell_col + 1),
            gridExpand = TRUE,
            stack = TRUE
          )

        } else {

          openxlsx::writeData(
            wb, sheet,
            out_x[[est_col]][i],
            startCol = cell_col,
            startRow = row_i,
            colNames = FALSE
          )

          openxlsx::writeData(
            wb, sheet,
            out_x[[se_col]][i],
            startCol = cell_col + 1,
            startRow = row_i,
            colNames = FALSE
          )
        }
      }
    }

    data_rows <- (sex_row + 1):(sex_row + length(ages))
    last_row <- max(data_rows)

    openxlsx::addStyle(
      wb, sheet,
      num_style,
      rows = data_rows,
      cols = (start_col + 1):(start_col + 8),
      gridExpand = TRUE,
      stack = TRUE
    )

    openxlsx::addStyle(
      wb, sheet,
      center_style,
      rows = data_rows,
      cols = start_col:(start_col + 8),
      gridExpand = TRUE,
      stack = TRUE
    )

    openxlsx::addStyle(
      wb, sheet,
      bottom_style,
      rows = last_row,
      cols = start_col:(start_col + 8),
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  women_row <- start_row + 4
  men_row <- women_row + length(ages) + 1

  write_one_sex("Women", women_row)
  write_one_sex("Men", men_row)

  footer_row <- men_row + length(ages) + 1

  openxlsx::writeData(
    wb, sheet,
    "No estimates are requested in the grayed-out regions of the table (these are outside of the consideration of XPrize).",
    startCol = start_col,
    startRow = footer_row
  )
}


write_table3_block <- function(
    wb,
    sheet,
    start_col,
    start_row,
    table1_x,
    table2_x,
    table1_age_col,
    women_m_col,
    men_m_col,
    support_x = NULL
) {

  title_style <- openxlsx::createStyle(textDecoration = "bold")

  center_style <- openxlsx::createStyle(
    halign = "center",
    valign = "center"
  )

  header_center_style <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    textDecoration = "bold"
  )

  header_top_style <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    border = "Top",
    textDecoration = "bold"
  )

  header_bottom_style <- openxlsx::createStyle(
    halign = "center",
    valign = "center",
    border = "Bottom",
    textDecoration = "bold"
  )

  bottom_style <- openxlsx::createStyle(border = "Bottom")
  gray_style <- openxlsx::createStyle(fgFill = "#D9D9D9")
  pct_style <- openxlsx::createStyle(numFmt = "0%")

  ages <- index_ages_table2
  yrs <- change_years

  mean_lookup <- table1_x |>
    dplyr::transmute(
      end_age = .data[[table1_age_col]],
      women_m = .data[[women_m_col]],
      men_m = .data[[men_m_col]]
    )

  openxlsx::writeData(
    wb, sheet,
    "Table 3. Percentage Decline Relative to Baseline at 5, 10, 15, and 20y of follow-up (calculated based on the estimated changes in Table 2 and the estimated means in Table 1)",
    startCol = start_col,
    startRow = start_row
  )

  openxlsx::addStyle(
    wb, sheet,
    title_style,
    rows = start_row,
    cols = start_col
  )

  openxlsx::writeData(
    wb, sheet,
    "Years of Change",
    startCol = start_col + 1,
    startRow = start_row + 1
  )

  openxlsx::mergeCells(
    wb, sheet,
    cols = (start_col + 1):(start_col + 8),
    rows = start_row + 1
  )

  openxlsx::writeData(
    wb, sheet,
    t(c("", "5", "", "10", "", "15", "", "20", "")),
    startCol = start_col,
    startRow = start_row + 2,
    colNames = FALSE
  )

  openxlsx::mergeCells(wb, sheet, cols = (start_col + 1):(start_col + 2), rows = start_row + 2)
  openxlsx::mergeCells(wb, sheet, cols = (start_col + 3):(start_col + 4), rows = start_row + 2)
  openxlsx::mergeCells(wb, sheet, cols = (start_col + 5):(start_col + 6), rows = start_row + 2)
  openxlsx::mergeCells(wb, sheet, cols = (start_col + 7):(start_col + 8), rows = start_row + 2)

  openxlsx::writeData(
    wb, sheet,
    t(c("Age", "WOMEN", "MEN", "WOMEN", "MEN", "WOMEN", "MEN", "WOMEN", "MEN")),
    startCol = start_col,
    startRow = start_row + 3,
    colNames = FALSE
  )

  openxlsx::addStyle(
    wb, sheet,
    header_center_style,
    rows = (start_row + 1):(start_row + 3),
    cols = start_col:(start_col + 8),
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    wb, sheet,
    header_top_style,
    rows = start_row + 1,
    cols = start_col:(start_col + 8),
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    wb, sheet,
    header_bottom_style,
    rows = start_row + 3,
    cols = start_col:(start_col + 8),
    gridExpand = TRUE,
    stack = TRUE
  )

  data_start_row <- start_row + 4

  for (i in seq_along(ages)) {

    age_i <- ages[i]
    row_i <- data_start_row + i - 1

    openxlsx::writeData(
      wb, sheet,
      age_i,
      startCol = start_col,
      startRow = row_i,
      colNames = FALSE
    )

    for (j in seq_along(yrs)) {

      yr_j <- yrs[j]
      end_age_j <- age_i + yr_j
      est_col <- paste0("est_", yr_j)
      cell_col <- start_col + 1 + (j - 1) * 2

      if (end_age_j < min(index_ages_table1) | end_age_j > max(index_ages_table1)) {

        openxlsx::addStyle(
          wb, sheet,
          gray_style,
          rows = row_i,
          cols = cell_col:(cell_col + 1),
          gridExpand = TRUE,
          stack = TRUE
        )

      } else {

        mean_x <- mean_lookup |>
          dplyr::filter(end_age == end_age_j)

        women_est <- table2_x |>
          dplyr::filter(
            sex_label == "Women",
            age == age_i
          ) |>
          dplyr::pull(dplyr::all_of(est_col))

        men_est <- table2_x |>
          dplyr::filter(
            sex_label == "Men",
            age == age_i
          ) |>
          dplyr::pull(dplyr::all_of(est_col))

        women_pct <- -1 * women_est / mean_x$women_m
        men_pct <- -1 * men_est / mean_x$men_m

        openxlsx::writeData(
          wb, sheet,
          women_pct,
          startCol = cell_col,
          startRow = row_i,
          colNames = FALSE
        )

        openxlsx::writeData(
          wb, sheet,
          men_pct,
          startCol = cell_col + 1,
          startRow = row_i,
          colNames = FALSE
        )
      }
    }
  }

  data_rows <- data_start_row:(data_start_row + length(ages) - 1)
  last_row <- max(data_rows)

  openxlsx::addStyle(
    wb, sheet,
    pct_style,
    rows = data_rows,
    cols = (start_col + 1):(start_col + 8),
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    wb, sheet,
    center_style,
    rows = data_rows,
    cols = start_col:(start_col + 8),
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    wb, sheet,
    bottom_style,
    rows = last_row,
    cols = start_col:(start_col + 8),
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::writeData(
    wb, sheet,
    "No estimates are requested in the grayed-out regions of the table (these are outside of the consideration of XPrize). Note negative percentage changes correspond to increases in cognitive functioning.",
    startCol = start_col,
    startRow = last_row + 1
  )
}


# --- Per-test sheet writers ---

write_one_pooled_test_sheet <- function(
    wb,
    test_x,
    dat,
    class1_t1,
    class1_t2,
    class2_t1,
    class2_t2,
    class1_t1_support = NULL,
    class1_t2_support = NULL,
    class2_t1_support = NULL,
    class2_t2_support = NULL
) {

  sheet_x <- make_pooled_sheet_name(test_x)

  openxlsx::addWorksheet(wb, sheet_x)

  title_style <- openxlsx::createStyle(textDecoration = "bold")

  openxlsx::writeData(wb, sheet_x, "Test", startCol = 1, startRow = 1)
  openxlsx::writeData(wb, sheet_x, test_x, startCol = 2, startRow = 1)
  openxlsx::writeData(wb, sheet_x, "Pooled cohorts: Original, Offspring, Gen3", startCol = 4, startRow = 1)

  openxlsx::addStyle(
    wb, sheet_x,
    title_style,
    rows = 1,
    cols = c(1, 4),
    gridExpand = TRUE
  )

  header_x <- get_pooled_header_info(
    dat = dat,
    test_x = test_x
  )

  c1_t1_x <- class1_t1 |>
    dplyr::filter(test == test_x)

  c1_t2_x <- class1_t2 |>
    dplyr::filter(test == test_x)

  c2_t1_x <- class2_t1 |>
    dplyr::filter(test == test_x)

  c2_t2_x <- class2_t2 |>
    dplyr::filter(test == test_x) |>
    dplyr::rename(age = baseline_age)

  c1_t1_support_x <- filter_support_sheet_pooled(
    class1_t1_support,
    test_x
  )

  c1_t2_support_x <- filter_support_sheet_pooled(
    class1_t2_support,
    test_x
  )

  c2_t1_support_x <- filter_support_sheet_pooled(
    class2_t1_support,
    test_x
  )

  c2_t2_support_x <- filter_support_sheet_pooled(
    class2_t2_support,
    test_x
  )

  write_table1_block(
    wb = wb,
    sheet = sheet_x,
    start_col = 1,
    start_row = 3,
    block_title = "Class 1. Age as Time",
    table_x = c1_t1_x,
    header_x = header_x,
    age_col = "age",
    women_m_col = "emmean_women",
    women_se_col = "se_women",
    men_m_col = "emmean_men",
    men_se_col = "se_men",
    support_x = c1_t1_support_x
  )

  write_table2_block(
    wb = wb,
    sheet = sheet_x,
    start_col = 1,
    start_row = 24,
    table_x = c1_t2_x,
    support_x = c1_t2_support_x
  )

  write_table3_block(
    wb = wb,
    sheet = sheet_x,
    start_col = 1,
    start_row = 58,
    table1_x = c1_t1_x,
    table2_x = c1_t2_x,
    table1_age_col = "age",
    women_m_col = "emmean_women",
    men_m_col = "emmean_men",
    support_x = c1_t2_support_x
  )

  write_table1_block(
    wb = wb,
    sheet = sheet_x,
    start_col = 14,
    start_row = 3,
    block_title = "Class 2. Follow-up as Time",
    table_x = c2_t1_x,
    header_x = header_x,
    age_col = "baseline_age",
    women_m_col = "m_women",
    women_se_col = "se_women",
    men_m_col = "m_men",
    men_se_col = "se_men",
    support_x = c2_t1_support_x
  )

  write_table2_block(
    wb = wb,
    sheet = sheet_x,
    start_col = 14,
    start_row = 24,
    table_x = c2_t2_x,
    support_x = c2_t2_support_x
  )

  write_table3_block(
    wb = wb,
    sheet = sheet_x,
    start_col = 14,
    start_row = 58,
    table1_x = c2_t1_x,
    table2_x = c2_t2_x,
    table1_age_col = "baseline_age",
    women_m_col = "m_women",
    men_m_col = "m_men",
    support_x = c2_t2_support_x
  )

  openxlsx::setColWidths(wb, sheet_x, cols = 1:25, widths = 11)
  openxlsx::setColWidths(wb, sheet_x, cols = c(1, 14), widths = 8)
  openxlsx::setColWidths(wb, sheet_x, cols = c(3, 16), widths = 4)

  invisible(wb)
}


# --- Whole-workbook writers ---

write_all_pooled_test_workbook <- function(
    dat,
    class1_t1,
    class1_t2,
    class2_t1,
    class2_t2,
    out_file,
    class1_t1_support = NULL,
    class1_t2_support = NULL,
    class2_t1_support = NULL,
    class2_t2_support = NULL
) {

  wb <- openxlsx::createWorkbook()

  sheet_grid <- dat |>
    dplyr::distinct(test) |>
    dplyr::arrange(test)

  purrr::walk(
    sheet_grid$test,
    \(test_x) {

      message(
        "Writing sheet: ",
        test_x
      )

      write_one_pooled_test_sheet(
        wb = wb,
        test_x = test_x,
        dat = dat,
        class1_t1 = class1_t1,
        class1_t2 = class1_t2,
        class2_t1 = class2_t1,
        class2_t2 = class2_t2,
        class1_t1_support = class1_t1_support,
        class1_t2_support = class1_t2_support,
        class2_t1_support = class2_t1_support,
        class2_t2_support = class2_t2_support
      )
    }
  )

  openxlsx::saveWorkbook(
    wb,
    out_file,
    overwrite = TRUE
  )

  out_file
}


# Deduplicated helper: append test_dictionary, data_summary, and followup_summary
# sheets to an existing workbook file. Previously this logic was copy-pasted
# in three separate _with_summary() wrappers.
append_summary_sheets_to_workbook <- function(
    out_file,
    test_dictionary_sheet,
    data_summary_sheet,
    followup_summary_sheet
) {

  wb <- openxlsx::loadWorkbook(out_file)

  openxlsx::addWorksheet(wb, "test_dictionary")
  openxlsx::writeDataTable(
    wb,
    sheet = "test_dictionary",
    x = test_dictionary_sheet
  )

  openxlsx::addWorksheet(wb, "data_summary_pooled")
  openxlsx::writeDataTable(
    wb,
    sheet = "data_summary_pooled",
    x = data_summary_sheet
  )

  openxlsx::addWorksheet(wb, "followup_summary_pooled")
  openxlsx::writeDataTable(
    wb,
    sheet = "followup_summary_pooled",
    x = followup_summary_sheet
  )

  percent_style <- openxlsx::createStyle(numFmt = "0.0%")

  data_summary_percent_cols <- which(
    names(data_summary_sheet) %in% c(
      "% sample_size_missing",
      "% subjects_2plus"
    )
  )

  if (
    length(data_summary_percent_cols) > 0 &&
    nrow(data_summary_sheet) > 0
  ) {
    openxlsx::addStyle(
      wb,
      sheet = "data_summary_pooled",
      style = percent_style,
      rows = 2:(nrow(data_summary_sheet) + 1),
      cols = data_summary_percent_cols,
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  openxlsx::setColWidths(
    wb,
    sheet = "test_dictionary",
    cols = 1:ncol(test_dictionary_sheet),
    widths = "auto"
  )

  openxlsx::setColWidths(
    wb,
    sheet = "data_summary_pooled",
    cols = 1:ncol(data_summary_sheet),
    widths = "auto"
  )

  openxlsx::setColWidths(
    wb,
    sheet = "followup_summary_pooled",
    cols = 1:ncol(followup_summary_sheet),
    widths = "auto"
  )

  openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)

  out_file
}


# Main workbook call flow:
# 1. write_all_pooled_test_workbook()
#    - creates one worksheet per test
#    - each test sheet is written by write_one_pooled_test_sheet()
#    - each test sheet contains Class 1 and Class 2 Table 1/2/3 blocks
# 2. append_summary_sheets_to_workbook()
#    - appends test_dictionary, data_summary_pooled, and followup_summary_pooled sheets

write_all_pooled_test_workbook_with_summary <- function(
    dat,
    class1_t1,
    class1_t2,
    class2_t1,
    class2_t2,
    test_dictionary_sheet,
    data_summary_sheet,
    followup_summary_sheet,
    out_file
) {

  write_all_pooled_test_workbook(
    dat = dat,
    class1_t1 = class1_t1,
    class1_t2 = class1_t2,
    class2_t1 = class2_t1,
    class2_t2 = class2_t2,
    out_file = out_file
  )

  append_summary_sheets_to_workbook(
    out_file = out_file,
    test_dictionary_sheet = test_dictionary_sheet,
    data_summary_sheet = data_summary_sheet,
    followup_summary_sheet = followup_summary_sheet
  )
}

# --- Table 4: Spontaneous reversal counts ---

# `form`:
#   "exclusive"      - mutually exclusive counts (integer).
#   "cumulative"     - cumulative counts (integer).
#   "cumulative_pct" - cumulative reversal rates (proportions in [0, 1]); the
#                      reversal cells are rendered with a percent number format.
# The percentage form expects table4_wide from make_table4(form = "cumulative_pct").
write_table4_workbook <- function(table4_wide, out_file,
                                  form = c("exclusive", "cumulative",
                                           "cumulative_pct")) {

  age_bins   <- seq(50, 85, by = 5)
  age_labels <- c("50-54", "55-59", "60-64", "65-69",
                  "70-74", "75-79", "80-84", "85-90")

  form <- match.arg(form)

  # Shared description of how the model-derived threshold is defined (used in every form).
  threshold_note <- paste(
    "Thresholds were evaluated from fixed-effect predictions at each participant's",
    "exact index age using the backward-age contrast from A-h to A.",
    "Observed baseline-to-first-follow-up change was annualized.",
    "For Trails tests, signs were reversed so that positive values indicate improvement."
  )

  if (form == "exclusive") {
    footnote <- paste(
      "N is the number of participant-test records with baseline age 50-90.",
      "Reversal columns are mutually exclusive and report the maximum qualifying horizon.",
      threshold_note
    )
  } else if (form == "cumulative") {
    footnote <- paste(
      "N is the number of participant-test records with baseline age 50-90.",
      "Columns report cumulative counts by maximum qualifying horizon.",
      threshold_note
    )
  } else {
    footnote <- paste(
      "N is the number of participant-test records with baseline age 50-90.",
      "Columns report cumulative reversal counts divided by N.",
      threshold_note
    )
  }

  f <- "Aptos Narrow"; s <- 12
  st_left    <- openxlsx::createStyle(fontName = f, fontSize = s, halign = "left")
  st_center  <- openxlsx::createStyle(fontName = f, fontSize = s, halign = "center")
  st_right   <- openxlsx::createStyle(fontName = f, fontSize = s, halign = "right")
  st_header  <- openxlsx::createStyle(fontName = f, fontSize = s, halign = "center",
                                      border = "top", borderStyle = "thin")
  st_section <- openxlsx::createStyle(fontName = f, fontSize = s, halign = "center",
                                      border = "TopBottom", borderStyle = "thin")
  st_note    <- openxlsx::createStyle(fontName = f, fontSize = s, halign = "left",
                                      valign = "top", wrapText = TRUE,
                                      border = "top", borderStyle = "thin")
  # Percent number format for the reversal-rate cells (cumulative_pct form only).
  st_pct     <- openxlsx::createStyle(fontName = f, fontSize = s, halign = "right",
                                      numFmt = "0.0%")

  is_pct <- (form == "cumulative_pct")

  make_block <- function(sex_x, test_x) {
    dplyr::tibble(age_bin = age_bins, Age = age_labels) |>
      dplyr::left_join(
        dplyr::filter(table4_wide, test == test_x, sex_label == sex_x),
        by = "age_bin"
      ) |>
      dplyr::arrange(age_bin) |>
      dplyr::transmute(
        Age, N,
        `5`  = `N reversal 5yr`,
        `10` = `N reversal 10yr`,
        `15` = `N reversal 15yr`,
        `20` = `N reversal 20yr`
      )
  }

  wb <- openxlsx::createWorkbook()

  for (test_x in unique(table4_wide$test)) {

    openxlsx::addWorksheet(wb, test_x)
    openxlsx::setColWidths(wb, test_x, cols = 2:7, widths = 11)

    form_tag <- switch(
      form,
      exclusive      = "Exclusive count",
      cumulative     = "Cumulative count",
      cumulative_pct = "Cumulative percentage"
    )
    title_lead <- if (is_pct) {
      'Percentage of men and women '
    } else {
      'Counts of men and women '
    }
    title <- paste0('Table 4 (', form_tag, '). ', title_lead,
                    'naturally exhibiting apparent "reversal" of cognitive ',
                    'aging (baseline to first follow-up)')
    openxlsx::writeData(wb, test_x, title, startRow = 3, startCol = 2)
    openxlsx::addStyle(wb, test_x, st_left, rows = 3, cols = 2)

    reversal_header <- if (is_pct) "% with Reversal" else "N with Reversal"
    openxlsx::writeData(wb, test_x, "N",              startRow = 4, startCol = 3)
    openxlsx::writeData(wb, test_x, reversal_header,  startRow = 4, startCol = 4)
    openxlsx::mergeCells(wb, test_x, cols = 4:7, rows = 4)
    openxlsx::addStyle(wb, test_x, st_header, rows = 4, cols = 2:7, gridExpand = TRUE)

    openxlsx::writeData(wb, test_x, "Age", startRow = 5, startCol = 2)
    if (form == "exclusive") {
      col_labels <- c("Max 5", "Max 10", "Max 15", "Max 20")
    } else {
      col_labels <- c("5+", "10+", "15+", "20+")
    }
    openxlsx::writeData(wb, test_x, matrix(col_labels, nrow = 1),
              startRow = 5, startCol = 4, colNames = FALSE)
    openxlsx::addStyle(wb, test_x, st_left,   rows = 5, cols = 2)
    openxlsx::addStyle(wb, test_x, st_center, rows = 5, cols = 4:7, gridExpand = TRUE)

    openxlsx::writeData(wb, test_x, "WOMEN", startRow = 6, startCol = 2)
    openxlsx::mergeCells(wb, test_x, cols = 2:7, rows = 6)
    openxlsx::addStyle(wb, test_x, st_section, rows = 6, cols = 2:7, gridExpand = TRUE)
    openxlsx::writeData(wb, test_x, make_block("Women", test_x),
              startRow = 7, startCol = 2, colNames = FALSE)
    openxlsx::addStyle(wb, test_x, st_right, rows = 7:14, cols = 2:7, gridExpand = TRUE)
    if (is_pct) {
      openxlsx::addStyle(wb, test_x, st_pct, rows = 7:14, cols = 4:7,
                         gridExpand = TRUE, stack = TRUE)
    }

    openxlsx::writeData(wb, test_x, "MEN", startRow = 15, startCol = 2)
    openxlsx::mergeCells(wb, test_x, cols = 2:7, rows = 15)
    openxlsx::addStyle(wb, test_x, st_section, rows = 15, cols = 2:7, gridExpand = TRUE)
    openxlsx::writeData(wb, test_x, make_block("Men", test_x),
              startRow = 16, startCol = 2, colNames = FALSE)
    openxlsx::addStyle(wb, test_x, st_right, rows = 16:23, cols = 2:7, gridExpand = TRUE)
    if (is_pct) {
      openxlsx::addStyle(wb, test_x, st_pct, rows = 16:23, cols = 4:7,
                         gridExpand = TRUE, stack = TRUE)
    }

    openxlsx::writeData(wb, test_x, footnote, startRow = 24, startCol = 2)
    openxlsx::mergeCells(wb, test_x, cols = 2:7, rows = 24)
    openxlsx::addStyle(wb, test_x, st_note, rows = 24, cols = 2:7, gridExpand = TRUE)
    openxlsx::setRowHeights(wb, test_x, rows = 24, heights = 120)
  }

  openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)
  message("Saved ", out_file, " -- ", length(unique(table4_wide$test)), " sheets")
}
