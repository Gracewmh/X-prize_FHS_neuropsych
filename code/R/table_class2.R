# table_class2.R
# Purpose: Build emmeans-based Tables 1–3 for Class 2 (follow-up-as-time) pooled
#          models, including education-stratified variants (by maxeducg_label).
# Used by: Notebook 02 via _load_all.R.
# Main functions provided: make_pooled_class2_table1(), format_pooled_class2_table1_wide(),
#          make_pooled_class2_table2(), format_pooled_class2_table2_wide(),
#          make_pooled_class2_education_table_objects() (convenience wrapper),
#          plus education-stratified table1/table2/table3 builders and formatters.
# Expected inputs: Fitted lme4 model objects, analysis data, test/sex labels.
# Expected globals: index_ages_table1, index_ages_table2, change_years (defined in 02.Rmd).
# Main outputs: Tibbles of Table 1/2/3 in long and wide format; education-stratified list.
# Notes for release: Table 3 (% decline) is computed from Table 1 means and Table 2 changes.

make_pooled_class2_table1 <- function(
    model_x,
    data_x,
    test_x,
    sex_x,
    class2_model_type_x = "quadratic"
) {

  emm_table1 <- emmeans::emmeans(
    model_x,
    specs = ~ baseline_age,
    at = list(
      baseline_age = index_ages_table1,
      t_followup = 0,
      birth_year_c = 0
    ),
    data = data_x,
    lmer.df = "asymptotic"
  )

  table1_x <- summary(emm_table1) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      test = test_x,
      sex_label = sex_x,
      class2_model_type = class2_model_type_x
    ) |>
    dplyr::rename(
      M = emmean
    ) |>
    dplyr::relocate(
      test,
      sex_label,
      class2_model_type,
      baseline_age,
      M,
      SE
    )

  table1_x
}


format_pooled_class2_table1_wide <- function(table1_x) {

  table1_x |>
    janitor::clean_names() |>
    dplyr::select(
      test,
      baseline_age,
      sex_label,
      m,
      se
    ) |>
    tidyr::pivot_wider(
      id_cols = c(
        test,
        baseline_age
      ),
      names_from = sex_label,
      values_from = c(
        m,
        se
      ),
      values_fn = list(
        m = dplyr::first,
        se = dplyr::first
      )
    ) |>
    janitor::clean_names() |>
    dplyr::arrange(
      test,
      baseline_age
    ) |>
    dplyr::select(
      test,
      baseline_age,
      m_women,
      se_women,
      m_men,
      se_men
    )
}


make_pooled_class2_table2 <- function(
    model_x,
    data_x,
    test_x,
    sex_x,
    class2_model_type_x = "quadratic"
) {

  emm_table2 <- emmeans::emmeans(
    model_x,
    specs = ~ t_followup | baseline_age,
    at = list(
      baseline_age = index_ages_table2,
      t_followup = c(0, change_years),
      birth_year_c = 0
    ),
    data = data_x,
    lmer.df = "asymptotic"
  )

  contrast_table2 <- emmeans::contrast(
    emm_table2,
    method = list(
      "5" = c(-1, 1, 0, 0, 0),
      "10" = c(-1, 0, 1, 0, 0),
      "15" = c(-1, 0, 0, 1, 0),
      "20" = c(-1, 0, 0, 0, 1)
    ),
    by = "baseline_age"
  )

  table2_x <- summary(
    contrast_table2,
    infer = c(TRUE, TRUE)
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      test = test_x,
      sex_label = sex_x,
      class2_model_type = class2_model_type_x,
      years_of_change = readr::parse_number(as.character(contrast))
    ) |>
    dplyr::select(
      test,
      sex_label,
      class2_model_type,
      baseline_age,
      years_of_change,
      Est = estimate,
      SE,
      dplyr::everything()
    )

  table2_x
}


format_pooled_class2_table2_wide <- function(table2_x) {

  table2_x |>
    janitor::clean_names() |>
    dplyr::select(
      test,
      class2_model_type,
      sex_label,
      baseline_age,
      years_of_change,
      est,
      se
    ) |>
    tidyr::pivot_wider(
      id_cols = c(
        test,
        class2_model_type,
        sex_label,
        baseline_age
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
      baseline_age
    ) |>
    dplyr::select(
      test,
      class2_model_type,
      sex_label,
      baseline_age,
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


# --- Education-stratified variants ---

make_pooled_class2_table1_by_education <- function(
    model_x,
    data_x,
    test_x,
    sex_x,
    education_var = "maxeducg_label",
    class2_model_type_x = "quadratic"
) {

  specs_formula <- stats::as.formula(
    paste("~ baseline_age |", education_var)
  )

  emm_table1 <- emmeans::emmeans(
    model_x,
    specs = specs_formula,
    at = list(
      baseline_age = index_ages_table1,
      t_followup = 0,
      birth_year_c = 0
    ),
    data = data_x,
    lmer.df = "asymptotic"
  )

  table1_x <- summary(emm_table1) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      test = test_x,
      sex_label = sex_x,
      class2_model_type = class2_model_type_x
    ) |>
    dplyr::rename(
      M = emmean
    ) |>
    dplyr::relocate(
      test,
      sex_label,
      class2_model_type,
      dplyr::all_of(education_var),
      baseline_age,
      M,
      SE
    )

  table1_x
}


make_pooled_class2_table2_by_education <- function(
    model_x,
    data_x,
    test_x,
    sex_x,
    education_var = "maxeducg_label",
    class2_model_type_x = "quadratic"
) {

  specs_formula <- stats::as.formula(
    paste("~ t_followup | baseline_age *", education_var)
  )

  emm_table2 <- emmeans::emmeans(
    model_x,
    specs = specs_formula,
    at = list(
      baseline_age = index_ages_table2,
      t_followup = c(0, change_years),
      birth_year_c = 0
    ),
    data = data_x,
    lmer.df = "asymptotic"
  )

  contrast_table2 <- emmeans::contrast(
    emm_table2,
    method = list(
      "5" = c(-1, 1, 0, 0, 0),
      "10" = c(-1, 0, 1, 0, 0),
      "15" = c(-1, 0, 0, 1, 0),
      "20" = c(-1, 0, 0, 0, 1)
    ),
    by = c(
      "baseline_age",
      education_var
    )
  )

  table2_x <- summary(
    contrast_table2,
    infer = c(TRUE, TRUE)
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      test = test_x,
      sex_label = sex_x,
      class2_model_type = class2_model_type_x,
      years_of_change = readr::parse_number(
        as.character(contrast)
      )
    ) |>
    dplyr::select(
      test,
      sex_label,
      class2_model_type,
      dplyr::all_of(education_var),
      baseline_age,
      years_of_change,
      Est = estimate,
      SE,
      dplyr::everything()
    )

  table2_x
}


format_pooled_class2_table2_education_wide <- function(
    table2_x,
    education_var = "maxeducg_label"
) {

  table2_x |>
    janitor::clean_names() |>
    dplyr::select(
      test,
      sex_label,
      dplyr::all_of(education_var),
      baseline_age,
      years_of_change,
      est,
      se
    ) |>
    tidyr::pivot_wider(
      id_cols = c(
        test,
        sex_label,
        dplyr::all_of(education_var),
        baseline_age
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
      .data[[education_var]],
      sex_label,
      baseline_age
    ) |>
    dplyr::select(
      test,
      dplyr::all_of(education_var),
      sex_label,
      baseline_age,
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

make_pooled_class2_table3_education_wide <- function(
    table1_x,
    table2_wide_x,
    education_var = "maxeducg_label"
) {

  mean_lookup <- table1_x |>
    janitor::clean_names() |>
    dplyr::transmute(
      test,
      sex_label,
      education_level = .data[[education_var]],
      end_age = baseline_age,
      mean_at_end_age = m
    )

  table2_long <- table2_wide_x |>
    janitor::clean_names() |>
    dplyr::select(
      test,
      education_level = dplyr::all_of(education_var),
      sex_label,
      baseline_age,
      dplyr::starts_with("est_")
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("est_"),
      names_to = "years_of_change",
      values_to = "est"
    ) |>
    dplyr::mutate(
      years_of_change = readr::parse_number(years_of_change),
      end_age = baseline_age + years_of_change
    )

  table3_long <- table2_long |>
    dplyr::left_join(
      mean_lookup,
      by = c(
        "test",
        "sex_label",
        "education_level",
        "end_age"
      )
    ) |>
    dplyr::mutate(
      pct_decline = -1 * est / mean_at_end_age
    )

  table3_long |>
    dplyr::select(
      test,
      education_level,
      sex_label,
      baseline_age,
      years_of_change,
      pct_decline
    ) |>
    tidyr::pivot_wider(
      id_cols = c(
        test,
        education_level,
        sex_label,
        baseline_age
      ),
      names_from = years_of_change,
      values_from = pct_decline,
      names_glue = "pct_{years_of_change}"
    ) |>
    dplyr::rename(
      !!education_var := education_level
    ) |>
    dplyr::arrange(
      test,
      .data[[education_var]],
      sex_label,
      baseline_age
    )
}

format_pooled_class2_table1_education_wide <- function(
    table1_x,
    education_var = "maxeducg_label"
) {

  table1_x |>
    janitor::clean_names() |>
    dplyr::select(
      test,
      dplyr::all_of(education_var),
      baseline_age,
      sex_label,
      m,
      se
    ) |>
    tidyr::pivot_wider(
      id_cols = c(
        test,
        dplyr::all_of(education_var),
        baseline_age
      ),
      names_from = sex_label,
      values_from = c(
        m,
        se
      ),
      values_fn = list(
        m = dplyr::first,
        se = dplyr::first
      )
    ) |>
    janitor::clean_names() |>
    dplyr::arrange(
      test,
      .data[[education_var]],
      baseline_age
    ) |>
    dplyr::select(
      test,
      dplyr::all_of(education_var),
      baseline_age,
      m_women,
      se_women,
      m_men,
      se_men
    )
}


# Convenience wrapper: fit the five education-stratified table objects
# (table1, table1_wide, table2, table2_wide, table3_wide) in one call.
make_pooled_class2_education_table_objects <- function(
    class2_fits_success,
    education_var = "maxeducg_label"
) {

  class2_table1_education <- class2_fits_success |>
    dplyr::mutate(
      table1 = purrr::pmap(
        list(
          model,
          data,
          test,
          sex_label,
          class2_model_type
        ),
        ~ make_pooled_class2_table1_by_education(
          model_x = ..1,
          data_x = ..2,
          test_x = ..3,
          sex_x = ..4,
          education_var = education_var,
          class2_model_type_x = ..5
        )
      )
    ) |>
    dplyr::select(
      model_version,
      model_class,
      n_obs,
      n_subjects,
      n_cohorts,
      n_education_levels,
      subject_random_slope_used,
      random_effect_structure,
      table1
    ) |>
    tidyr::unnest(table1) |>
    janitor::clean_names() |>
    dplyr::arrange(
      test,
      sex_label,
      .data[[education_var]],
      baseline_age
    )

  class2_table2_education <- class2_fits_success |>
    dplyr::mutate(
      table2 = purrr::pmap(
        list(
          model,
          data,
          test,
          sex_label,
          class2_model_type
        ),
        ~ make_pooled_class2_table2_by_education(
          model_x = ..1,
          data_x = ..2,
          test_x = ..3,
          sex_x = ..4,
          education_var = education_var,
          class2_model_type_x = ..5
        )
      )
    ) |>
    dplyr::select(
      model_version,
      model_class,
      n_obs,
      n_subjects,
      n_cohorts,
      n_education_levels,
      subject_random_slope_used,
      random_effect_structure,
      table2
    ) |>
    tidyr::unnest(table2) |>
    janitor::clean_names() |>
    dplyr::arrange(
      test,
      sex_label,
      .data[[education_var]],
      baseline_age,
      years_of_change
    )

  class2_table1_education_wide <- format_pooled_class2_table1_education_wide(
    table1_x = class2_table1_education,
    education_var = education_var
  )

  class2_table2_education_wide <- format_pooled_class2_table2_education_wide(
    table2_x = class2_table2_education,
    education_var = education_var
  )

  class2_table3_education_wide <- make_pooled_class2_table3_education_wide(
    table1_x = class2_table1_education,
    table2_wide_x = class2_table2_education_wide,
    education_var = education_var
  )

  list(
    class2_table1_education = class2_table1_education,
    class2_table1_education_wide = class2_table1_education_wide,
    class2_table2_education = class2_table2_education,
    class2_table2_education_wide = class2_table2_education_wide,
    class2_table3_education_wide = class2_table3_education_wide
  )
}
