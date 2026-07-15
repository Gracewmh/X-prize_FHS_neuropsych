# table_class1.R
# Purpose: Build emmeans-based Tables 1 (predicted means) and 2 (expected change)
#          for Class 1 (age-as-time) pooled models.
# Used by: Notebook 02 via _load_all.R.
# Main functions provided: make_pooled_class1_table1(), format_pooled_class1_table1_wide(),
#          make_contrast_list(), make_pooled_class1_table2(), format_pooled_class1_table2_wide()
# Expected inputs: Fitted lme4 model objects, analysis data, test/sex labels.
# Main outputs: Tibbles of emmeans-based Table 1 and Table 2 in long and wide format.
# Notes for release: Index ages for Table 1 = seq(50,90,5); Table 2 baselines = seq(30,85,5).

make_pooled_class1_table1 <- function(
    model_x,
    data_x,
    test_x,
    sex_x
) {

  emm_table1 <- emmeans::emmeans(
    model_x,
    specs = ~ cage_65,
    at = list(
      cage_65 = index_ages_table1 - 65,
      birth_year_c = 0
    ),
    data = data_x,
    lmer.df = "asymptotic"
  )

  table1_x <- summary(emm_table1) |>
    janitor::clean_names() |>
    dplyr::mutate(
      test = test_x,
      sex_label = sex_x,
      age = cage_65 + 65
    ) |>
    dplyr::select(
      test,
      sex_label,
      age,
      emmean,
      se,
      dplyr::everything()
    )

  table1_x
}


format_pooled_class1_table1_wide <- function(table1_x) {

  table1_x |>
    janitor::clean_names() |>
    dplyr::select(
      test,
      age,
      sex_label,
      emmean,
      se
    ) |>
    tidyr::pivot_wider(
      id_cols = c(
        test,
        age
      ),
      names_from = sex_label,
      values_from = c(
        emmean,
        se
      ),
      values_fn = list(
        emmean = dplyr::first,
        se = dplyr::first
      )
    ) |>
    janitor::clean_names() |>
    dplyr::arrange(
      test,
      age
    ) |>
    dplyr::select(
      test,
      age,
      emmean_women,
      se_women,
      emmean_men,
      se_men
    )
}


# Build contrast vectors that compare scores at two ages within the
# same emmeans grid (used for Table 2 age-based change estimates).

make_contrast_list <- function(grid, emm_levels) {

  contrast_positions <- grid |>
    dplyr::mutate(
      start_position = match(start_cage_65, emm_levels),
      end_position = match(end_cage_65, emm_levels)
    )

  contrast_list <- purrr::map2(
    contrast_positions$start_position,
    contrast_positions$end_position,
    \(start_position, end_position) {

      contrast_vector <- rep(0, length(emm_levels))
      contrast_vector[start_position] <- -1
      contrast_vector[end_position] <- 1

      contrast_vector
    }
  )

  names(contrast_list) <- paste0(
    "age_",
    grid$age,
    "_",
    grid$years_of_change,
    "y"
  )

  contrast_list
}


make_pooled_class1_table2 <- function(
    model_x,
    data_x,
    test_x,
    sex_x
) {

  table2_grid <- tidyr::expand_grid(
    age = index_ages_table2,
    years_of_change = change_years
  ) |>
    dplyr::mutate(
      end_age = age + years_of_change,
      start_cage_65 = age - 65,
      end_cage_65 = end_age - 65
    )

  cage_65_values_table2 <- sort(unique(c(
    table2_grid$start_cage_65,
    table2_grid$end_cage_65
  )))

  emm_table2 <- emmeans::emmeans(
    model_x,
    specs = ~ cage_65,
    at = list(
      cage_65 = cage_65_values_table2,
      birth_year_c = 0
    ),
    data = data_x,
    lmer.df = "asymptotic"
  )

  emm_levels <- summary(emm_table2) |>
    tibble::as_tibble() |>
    dplyr::pull(cage_65)

  contrast_list <- make_contrast_list(
    grid = table2_grid,
    emm_levels = emm_levels
  )

  contrast_table2 <- emmeans::contrast(
    emm_table2,
    method = contrast_list
  )

  table2_raw <- summary(
    contrast_table2,
    infer = c(TRUE, TRUE)
  ) |>
    tibble::as_tibble()

  stopifnot(nrow(table2_grid) == nrow(table2_raw))

  age_range_x <- data_x |>
    dplyr::summarise(
      min_age = min(age_at_measure, na.rm = TRUE),
      max_age = max(age_at_measure, na.rm = TRUE)
    )

  table2_x <- dplyr::bind_cols(
    table2_grid,
    table2_raw
  ) |>
    dplyr::mutate(
      test = test_x,
      sex_label = sex_x,
      supported_by_observed_age_range =
        age >= age_range_x$min_age &
        end_age <= age_range_x$max_age
    ) |>
    dplyr::select(
      test,
      sex_label,
      age,
      years_of_change,
      end_age,
      Est = estimate,
      SE,
      supported_by_observed_age_range,
      dplyr::everything()
    )

  table2_x
}


format_pooled_class1_table2_wide <- function(table2_x) {

  table2_x |>
    janitor::clean_names() |>
    dplyr::select(
      test,
      sex_label,
      age,
      years_of_change,
      est,
      se
    ) |>
    tidyr::pivot_wider(
      id_cols = c(
        test,
        sex_label,
        age
      ),
      names_from = years_of_change,
      values_from = c(
        est,
        se
      ),
      names_glue = "{.value}_{years_of_change}"
    ) |>
    dplyr::arrange(
      test,
      factor(sex_label, levels = c("Women", "Men")),
      age
    ) |>
    dplyr::select(
      test,
      sex_label,
      age,
      est_5,
      se_5,
      est_10,
      se_10,
      est_15,
      se_15,
      est_20,
      se_20
    )
}
