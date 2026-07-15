# spontaneous_reversal_functions.R
# Shared functions for the Class 1 and Class 2 spontaneous-reversal analyses.
# The two model classes use the same classification and Table 4 procedures,
# but different functions to calculate model-estimated change.
#
# Used by notebooks 05 and 06.


higher_is_worse_tests <- c("trailsa", "trailsb")


# --- Baseline-to-first-follow-up change for one test-by-sex stratum ---

compute_obs_change <- function(data_x) {
  data_x |>
    filter(
      !is.na(score),
      !is.na(baseline_age),
      !is.na(t_followup)
    ) |>
    arrange(t_followup) |>
    group_by(dbgap_subject_id) |>
    filter(n() >= 2) |>
    slice(1:2) |>
    summarise(
      idtype_label    = first(idtype_label),
      baseline_age    = first(baseline_age),
      score_baseline  = score[1],
      score_followup1 = score[2],
      followup_years  = t_followup[2] - t_followup[1],
      .groups = "drop"
    ) |>
    filter(followup_years > 0) |>
    mutate(
      obs_change_annual_raw = (score_followup1 - score_baseline) / followup_years
    )
}


# --- Fixed-effect-only prediction ---

predict_fixed <- function(model_x, newdata) {
  fe_formula <- lme4::nobars(formula(model_x))
  fe_terms   <- delete.response(terms(fe_formula))
  mm   <- model.matrix(fe_terms, data = newdata)
  beta <- lme4::fixef(model_x)
  as.numeric(mm[, names(beta)] %*% beta)
}


# --- Exact-age model-estimated changes ---

# Fixed-effect Class 2 contrast from reference age A-h to index age A:
# Pred(baseline_age = A-h, t_followup = h) - Pred(baseline_age = A-h, t_followup = 0).

compute_model_change_class2 <- function(model_x, unique_baseline_ages) {

  # Fixed-effect Class 2 contrast from reference age A-h to index age A.
  horizons <- c(5, 10, 15, 20)

  # Evaluate model changes at participants' exact baseline ages crossed with each target horizon.
  grid <- tidyr::expand_grid(
    baseline_age_exact = unique_baseline_ages,
    horizon = horizons
  )
  
  reference_baseline_age <- grid$baseline_age_exact - grid$horizon

  newdata_older <- tibble::tibble(
    baseline_age = reference_baseline_age,
    t_followup   = grid$horizon,
    birth_year_c = 0
  )

  newdata_younger <- tibble::tibble(
    baseline_age = reference_baseline_age,
    t_followup   = 0,
    birth_year_c = 0
  )

  grid$model_change_raw <-
    predict_fixed(model_x, newdata_older) -
    predict_fixed(model_x, newdata_younger)

  # Return one row per exact baseline age, with separate columns for each model-estimated horizon-specific change.
  grid |>
    transmute(
      baseline_age = baseline_age_exact,
      horizon,
      model_change_raw
    ) |>
    tidyr::pivot_wider(
      id_cols     = baseline_age,
      names_from  = horizon,
      values_from = model_change_raw,
      names_glue  = "model_change_{horizon}yr_raw"
    )
}


# Fixed-effect Class 1 contrast from reference age A-h to index age A:
# Pred(age = A) - Pred(age = A-h).
compute_model_change_class1 <- function(model_x, unique_baseline_ages) {
  horizons <- c(5, 10, 15, 20)
  grid <- expand_grid(
    baseline_age_exact = unique_baseline_ages,
    horizon = horizons
  )
  newdata_index <- tibble(
    cage_65      = grid$baseline_age_exact - 65,
    birth_year_c = 0
  )
  newdata_younger <- tibble(
    cage_65      = (grid$baseline_age_exact - grid$horizon) - 65,
    birth_year_c = 0
  )
  # Aging-direction contrast: predicted score at index age A minus predicted
  # score at the younger reference age A - h.
  grid$model_change_raw <-
    predict_fixed(model_x, newdata_index) -
    predict_fixed(model_x, newdata_younger)

  grid |>
    transmute(
      baseline_age = baseline_age_exact,
      horizon,
      model_change_raw
    ) |>
    pivot_wider(
      id_cols     = baseline_age,
      names_from  = horizon,
      values_from = model_change_raw,
      names_glue  = "model_change_{horizon}yr_raw"
    )
}


# --- Reversal threshold ---
# Convert direction-adjusted model change to the reversal threshold: decline -> absolute magnitude; improvement -> twice the magnitude.
# Rules:
#   model_change < 0: expected decline; threshold = -1 * model_change
#   model_change > 0: expected improvement; threshold = 2 * model_change
#   model_change = 0: no expected change; threshold = 0
reversal_threshold <- function(model_change) {
  case_when(
    is.na(model_change) ~ NA_real_,
    model_change < 0 ~ (-1) * model_change,
    model_change > 0 ~ 2 * model_change,
    TRUE ~ 0
  )
}


# --- Reversal classification ---

# Classify spontaneous reversal for participant-test records. Direction-adjust observed and model changes, calculate thresholds,and assign the maximum qualifying reversal horizon.

classify_reversal <- function(subject_data) {
  subject_data |>
    mutate(
      # Direction convention: positive change = improvement for all tests.
      score_direction = if_else(
        test %in% higher_is_worse_tests, -1L, 1L
      ),
      
      # Direction-adjust observed annualized change and model-estimated changes.
      obs_change_annual = score_direction * obs_change_annual_raw,
      model_change_5yr  = score_direction * model_change_5yr_raw,
      model_change_10yr = score_direction * model_change_10yr_raw,
      model_change_15yr = score_direction * model_change_15yr_raw,
      model_change_20yr = score_direction * model_change_20yr_raw,
      
      # Convert each `model-estimated change` into the `threshold` required to qualify as spontaneous reversal.
      # `reversal_threshold()` is defined above
      threshold_5yr  = reversal_threshold(model_change_5yr),
      threshold_10yr = reversal_threshold(model_change_10yr),
      threshold_15yr = reversal_threshold(model_change_15yr),
      threshold_20yr = reversal_threshold(model_change_20yr),
      
      # A participant-test record qualifies at a horizon if the annualized observed improvement exceeds the corresponding model-derived threshold.
      reversal_5yr  = !is.na(threshold_5yr)  & obs_change_annual > threshold_5yr,
      reversal_10yr = !is.na(threshold_10yr) & obs_change_annual > threshold_10yr,
      reversal_15yr = !is.na(threshold_15yr) & obs_change_annual > threshold_15yr,
      reversal_20yr = !is.na(threshold_20yr) & obs_change_annual > threshold_20yr,
      
      # Maximum qualifying horizon, used for exclusive Table 4 counts.
      max_reversal_year = case_when(
        reversal_20yr ~ 20L,
        reversal_15yr ~ 15L,
        reversal_10yr ~ 10L,
        reversal_5yr  ~ 5L,
        TRUE ~ 0L
      )
    )
}

# --- Baseline-age restriction for spontaneous reversal outputs ---
# Restrict spontaneous-reversal outputs to index baseline ages 50–90.

restrict_reversal_baseline_age <- function(reversal_data, age_range = c(50, 90)) {
  reversal_data |>
    filter(baseline_age >= age_range[1], baseline_age <= age_range[2])
}


# --- Table 4 aggregation ---

# Table 4 formats:
# exclusive: maximum qualifying horizon;
# cumulative: maximum horizon >= h;
# cumulative_pct: cumulative count divided by N.

make_table4 <- function(reversal_data, test_dictionary,
                        form = c("exclusive", "cumulative", "cumulative_pct"),
                        age_range = c(50, 90)) {
  form <- match.arg(form)
  table4_age_bins <- seq(50, 85, by = 5)

  binned <- reversal_data |>
    filter(baseline_age >= age_range[1], baseline_age <= age_range[2]) |>
    mutate(
      age_bin = case_when(
        baseline_age >= 85 ~ 85L,
        TRUE ~ as.integer(floor(baseline_age / 5) * 5)
      )
    ) |>
    filter(age_bin %in% table4_age_bins)

  base_counts <- binned |>
    group_by(test, sex_label, age_bin) |>
    summarise(N = n(), .groups = "drop")

  if (form == "exclusive") {
    rev_counts <- binned |>
      group_by(test, sex_label, age_bin) |>
      summarise(
        `N reversal 5yr`  = sum(max_reversal_year == 5),
        `N reversal 10yr` = sum(max_reversal_year == 10),
        `N reversal 15yr` = sum(max_reversal_year == 15),
        `N reversal 20yr` = sum(max_reversal_year == 20),
        .groups = "drop"
      )
  } else {
    # "cumulative" and "cumulative_pct" share the same cumulative (>=) counts;
    # the percentage form divides these by N below.
    rev_counts <- binned |>
      group_by(test, sex_label, age_bin) |>
      summarise(
        `N reversal 5yr`  = sum(max_reversal_year >= 5),
        `N reversal 10yr` = sum(max_reversal_year >= 10),
        `N reversal 15yr` = sum(max_reversal_year >= 15),
        `N reversal 20yr` = sum(max_reversal_year >= 20),
        .groups = "drop"
      )
  }

  table4 <- base_counts |>
    left_join(rev_counts, by = c("test", "sex_label", "age_bin")) |>
    left_join(
      test_dictionary |>
        select(test, test_label, xprize_subdomain),
      by = "test"
    ) |>
    mutate(
      age_label = if_else(
        age_bin == 85L, "85-90",
        paste0(age_bin, "-", age_bin + 4L)
      )
    ) |>
    select(
      xprize_subdomain, test, test_label, sex_label,
      age_label, age_bin, N,
      `N reversal 5yr`, `N reversal 10yr`,
      `N reversal 15yr`, `N reversal 20yr`
    ) |>
    arrange(xprize_subdomain, test, sex_label, age_bin)

  if (form == "cumulative_pct") {
    # Convert the cumulative counts to reversal rates (share of N). N >= 1 for
    # every returned row (each row is an observed test x sex x age_bin cell), so
    # the division is well defined. Kept as a proportion in [0, 1].
    table4 <- table4 |>
      mutate(
        across(
          c(`N reversal 5yr`, `N reversal 10yr`,
            `N reversal 15yr`, `N reversal 20yr`),
          ~ .x / N
        )
      )
  }

  table4
}


# --- Colored scatter plot ---

reversal_color_values <- c(
  "None" = "grey70",
  "5y"   = "#AFA9EC",
  "10y"  = "#7F77DD",
  "15y"  = "#534AB7",
  "20y"  = "#26215C"
)

reversal_level_labels <- c(
  "0" = "None", "5" = "5y", "10" = "10y", "15" = "15y", "20" = "20y"
)

plot_reversal_scatter <- function(subject_data, test_dictionary, test_x,
                                  age_range = c(50, 90)) {

  # Test labels used in the plot title.
  test_info <- test_dictionary |> filter(test == test_x)

  plot_data <- subject_data |>
    filter(
      test == test_x,
      baseline_age >= age_range[1], baseline_age <= age_range[2]
    ) |>
    mutate(
      reversal_label = factor(
        reversal_level_labels[as.character(max_reversal_year)],
        levels = reversal_level_labels
      )
    ) |>
    arrange(reversal_label)

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = baseline_age,
      y = obs_change_annual,
      color = reversal_label
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0, linetype = "dashed",
      color = "grey50", linewidth = 0.4
    ) +
    ggplot2::geom_point(alpha = 0.4, size = 1) +
    ggplot2::facet_wrap(ggplot2::vars(sex_label), nrow = 1) +
    ggplot2::scale_color_manual(
      values = reversal_color_values,
      name   = "Max reversal horizon",
      drop   = FALSE
    ) +
    # Restrict to the same baseline-age range used in Table 4.
    ggplot2::scale_x_continuous(breaks = seq(50, 90, by = 10)) +
    ggplot2::labs(
      title = paste0("Spontaneous reversal: ", test_info$test_label[1]),
      subtitle = paste0(
        "Subdomain: ", test_info$xprize_subdomain[1],
        "; annualized baseline-to-first-follow-up change"
      ),
      x = "Baseline age",
      y = "Annualized observed change"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(alpha = 0.8, size = 2.5)
      )
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(size = 15, face = "bold"),
      plot.subtitle    = ggplot2::element_text(size = 12, color = "grey30"),
      axis.title       = ggplot2::element_text(size = 13),
      axis.text        = ggplot2::element_text(size = 10.5),
      strip.background = ggplot2::element_rect(fill = "grey90"),
      strip.text       = ggplot2::element_text(size = 11),
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing.x  = grid::unit(0.8, "lines"),
      legend.position  = "bottom"
    )
}

