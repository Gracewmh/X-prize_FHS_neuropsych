# practice_effect_alltests_functions.R

# Helper functions for notebook 07 practice-effect analyses.
#
# M1 uses practice-adjusted models with categorical prior_count_f entered
# as a main effect only. M2 subtracts the EQ1/EQ2 regression-estimated
# practice effect. M3 combines the M1 thresholds and M2 correction.
#
# This file depends on the shared spontaneous-reversal functions loaded
# before it by R/_load_all.R.

# --- Shared practice-model utilities ------------------------------------------

.pe_control <- lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
PE_HORIZONS <- c(5, 10, 15, 20)

# Add the number of prior administrations within each subject-test series.
# prior_count_f is categorized as 0, 1, 2, or 3+.
add_prior_count <- function(data_x, cap = 3L) {
  labs <- c(as.character(0:(cap - 1L)), paste0(cap, "plus"))
  
  # Count administrations separately for each subject and test.
  grp <- c("dbgap_subject_id", intersect("test", names(data_x)))
  data_x |>
    arrange(across(all_of(c(grp, "t_followup")))) |>
    group_by(across(all_of(grp))) |>
    mutate(
      prior_count      = dplyr::row_number() - 1L,
      prior_count_f    = factor(pmin(prior_count, cap), levels = 0:cap, labels = labs),
      prior_count_sqrt = sqrt(prior_count)
    ) |>
    ungroup() |>
    mutate(
      attained_age   = baseline_age + t_followup,
      attained_age_c = attained_age - 65
    )
}

# Fit the practice-adjusted model with prior_count_f as a categorical main effect. The coefficients for 1, 2, and 3+ prior administrations
# are estimated relative to 0 and do not vary with age.
#
# The primary model includes participant random intercepts and slopes. If that fit is singular or unsuccessful, the participant slope is removed.

fit_practice_model <- function(data_x, class, practice_term = "factor") {
  if (practice_term != "factor")
    stop("fit_practice_model: only practice_term = 'factor' is currently supported.")
  time_term <- if (class == "class1") "cage_65" else "t_followup"
  base_fe   <- if (class == "class1") c("cage_65", "I(cage_65^2)")
               else "baseline_age * (t_followup + I(t_followup^2))"
  pt        <- "prior_count_f"
  main_term <- "prior_count_f1"     # detection token for the factor specification

  # prior_count_f enters as a main effect only. Its category-specific coefficients are constant across age; the age trajectory is shared across prior-count categories.
  fe_main     <- c(base_fe, pt, "birth_year_c")
  re_primary  <- c(paste0("(1 + ", time_term, " | idtype_label)"),
                   paste0("(1 + ", time_term, " | dbgap_subject_id)"))
  re_fallback <- c(paste0("(1 + ", time_term, " | idtype_label)"),
                   "(1 | dbgap_subject_id)")

  combos <- list(
    list(fe = fe_main, re = re_primary,  age_int = FALSE, subj_slope = TRUE),
    list(fe = fe_main, re = re_fallback, age_int = FALSE, subj_slope = FALSE)
  )


  try_combo <- function(cb) {
    f    <- stats::reformulate(c(cb$fe, cb$re), response = "score")
    msgs <- character(0)
    fit  <- tryCatch(
      withCallingHandlers(
        lme4::lmer(f, data = data_x, REML = FALSE, control = .pe_control),
        warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") }
      ),
      error = function(e) { msgs <<- c(msgs, paste("ERROR:", conditionMessage(e))); NULL }
    )
    ok <- !is.null(fit)
    fe <- if (ok) lme4::fixef(fit) else numeric(0)
    has_p1  <- ok && main_term %in% names(fe) && !is.na(fe[[main_term]])
    list(fit = fit, cb = cb, msgs = msgs, ok = ok,
         acceptable = ok && has_p1, singular = ok && lme4::isSingular(fit))
  }
  is_best <- function(a) a$acceptable && !a$singular

  # Try combos in order, stopping once one is acceptable and non-singular (best case).
  attempts <- purrr::reduce(combos, function(acc, cb)
    if (length(acc) && is_best(acc[[length(acc)]])) acc else c(acc, list(try_combo(cb))),
    .init = list())

  # Prefer a non-singular acceptable model; else the simplest acceptable; else any that fit.
  pick <- purrr::detect(attempts, is_best)
  if (is.null(pick)) pick <- purrr::detect(attempts, \(a) a$acceptable, .dir = "backward")
  if (is.null(pick)) pick <- purrr::detect(attempts, \(a) a$ok)
  if (is.null(pick)) stop("fit_practice_model: no model could be fitted for this stratum.")

  fit <- pick$fit; fe <- lme4::fixef(fit)
  list(
    model           = fit,
    formula         = paste(deparse(stats::formula(fit)), collapse = " "),
    practice_term   = practice_term,
    age_interaction = pick$cb$age_int,
    subject_slope   = pick$cb$subj_slope,
    singular        = pick$singular,
    messages        = paste(unique(pick$msgs), collapse = " | "),
    aic             = stats::AIC(fit),
    bic             = stats::BIC(fit),
    fe_prior        = fe[grepl("prior_count", names(fe))]
  )
}

# Apply a method-specific practice correction to observed change and classify reversal using the supplied model-based thresholds.
# practice_gain_raw is retained for diagnostic output.
classify_under_method <- function(obs, model_changes, practice_gain_used, test_x, sex_x,
                                  method_name, practice_gain_raw = practice_gain_used) {
  obs |>
    mutate(
      practice_gain_raw     = practice_gain_raw,
      practice_gain_used    = practice_gain_used,
      raw_change            = score_followup1 - score_baseline,
      raw_change_annual     = raw_change / followup_years,
      adj_change_total      = raw_change - practice_gain_used,
      adj_change_annual     = adj_change_total / followup_years,
      obs_change_annual_raw = adj_change_annual           # quantity classify_reversal() tests
    ) |>
    left_join(model_changes, by = "baseline_age") |>
    mutate(test = test_x, sex_label = sex_x) |>
    classify_reversal() |>
    mutate(method = method_name)
}


# --- Practice-free model thresholds ---------------------------------------

# Reproduce the production backward-age contrasts using the practice-adjusted model with prior_count_f fixed at its reference level, "0".

# Retain the model's factor levels when setting prior_count_f to "0".
.pe_prior0 <- function(model) {
  factor("0", levels = levels(stats::model.frame(model)$prior_count_f))
}

# Class 1 (age-as-time): pred(cage_65 = A - 65) - pred(cage_65 = (A - h) - 65).
compute_model_change_new_class1_backward <- function(model, ages, horizons = PE_HORIZONS) {
  grid <- tidyr::expand_grid(baseline_age_exact = ages, horizon = horizons)
  p0 <- .pe_prior0(model)

  nd_index <- tibble::tibble(cage_65 = grid$baseline_age_exact - 65,
                             prior_count_f = p0, birth_year_c = 0)
  nd_start <- tibble::tibble(cage_65 = (grid$baseline_age_exact - grid$horizon) - 65,
                             prior_count_f = p0, birth_year_c = 0)

  grid$model_change_raw <- predict_fixed(model, nd_index) - predict_fixed(model, nd_start)
  grid |>
    dplyr::transmute(baseline_age = baseline_age_exact, horizon, model_change_raw) |>
    tidyr::pivot_wider(id_cols = baseline_age, names_from = horizon,
                       values_from = model_change_raw,
                       names_glue = "model_change_{horizon}yr_raw")
}

# Class 2 (follow-up-as-time): the reference-start-age follow-up contrast
# pred(baseline_age = A - h, t = h) - pred(baseline_age = A - h, t = 0).
compute_model_change_new_class2_backward <- function(model, ages, horizons = PE_HORIZONS) {
  grid <- tidyr::expand_grid(baseline_age_exact = ages, horizon = horizons)
  p0 <- .pe_prior0(model)
  ref_age <- grid$baseline_age_exact - grid$horizon   # A - h

  nd_index <- tibble::tibble(baseline_age = ref_age, t_followup = grid$horizon,
                             attained_age_c = grid$baseline_age_exact - 65,   # (A - h) + h - 65
                             prior_count_f = p0, birth_year_c = 0)
  nd_start <- tibble::tibble(baseline_age = ref_age, t_followup = 0,
                             attained_age_c = ref_age - 65,                    # (A - h) - 65
                             prior_count_f = p0, birth_year_c = 0)

  grid$model_change_raw <- predict_fixed(model, nd_index) - predict_fixed(model, nd_start)
  grid |>
    dplyr::transmute(baseline_age = baseline_age_exact, horizon, model_change_raw) |>
    tidyr::pivot_wider(id_cols = baseline_age, names_from = horizon,
                       values_from = model_change_raw,
                       names_glue = "model_change_{horizon}yr_raw")
}


# --- Method 2: EQ1/EQ2 regression correction ---------------------------------

# At attained age u = a + h:
# practice effect = Pred_EQ1(retest at age u) -
#                   Pred_EQ2(first test at age u).
# Follow-up interval enters only through attained age u.

# Stack the baseline (EQ2) and first-follow-up (EQ1) rows for one test, from the same baseline-to-first-follow-up records compute_obs_change() produces.
build_m2_equation_data <- function(obs_test) {
  eq2 <- obs_test |>
    dplyr::transmute(test, dbgap_subject_id, idtype_label, sex_label,
                     m2_visit_status = "baseline_firsttest",
                     score = score_baseline, baseline_age, followup_years,
                     age_bl = baseline_age, age_fu1 = NA_real_)
  eq1 <- obs_test |>
    dplyr::transmute(test, dbgap_subject_id, idtype_label, sex_label,
                     m2_visit_status = "first_followup_retest",
                     score = score_followup1, baseline_age, followup_years,
                     age_bl = NA_real_, age_fu1 = baseline_age + followup_years)
  dplyr::bind_rows(eq2, eq1)
}

# Fit one equation. EQ1 uses age_fu1, EQ2 uses age_bl. Full sex-interaction quadratic first,
# stepping down if it is rank-deficient (any NA coefficient) or fails.
fit_m2_equation <- function(dat, equation) {
  age_var <- if (equation == "baseline_firsttest") "age_bl" else "age_fu1"
  d <- dat[dat$m2_visit_status == equation & !is.na(dat$score) & !is.na(dat[[age_var]]), ,
           drop = FALSE]

  forms <- c(
    sprintf("score ~ %s + I(%s^2) + sex_label + %s:sex_label + I(%s^2):sex_label",
            age_var, age_var, age_var, age_var),
    sprintf("score ~ %s + I(%s^2) + sex_label", age_var, age_var),
    sprintf("score ~ %s + sex_label", age_var),
    sprintf("score ~ %s", age_var)
  )

  fits_cleanly <- function(f) {
    m <- tryCatch(stats::lm(stats::as.formula(f), data = d), error = function(e) NULL)
    !is.null(m) && !anyNA(stats::coef(m))
  }
  formula_used <- purrr::detect(forms, fits_cleanly)
  if (is.null(formula_used)) formula_used <- forms[length(forms)]

  list(model = stats::lm(stats::as.formula(formula_used), data = d),
       equation = equation, formula_used = formula_used, n_obs = nrow(d))
}


fit_m2_regressions <- function(obs_test) {
  dat <- build_m2_equation_data(obs_test)
  list(eq2 = fit_m2_equation(dat, "baseline_firsttest"),
       eq1 = fit_m2_equation(dat, "first_followup_retest"),
       data = dat)
}

m2_coefficient_table <- function(m2_test, test_x, model_version) {
  one <- function(eqfit) {
    broom::tidy(eqfit$model) |>
      dplyr::transmute(model_version = model_version, test = test_x, equation = eqfit$equation,
                       formula_used = eqfit$formula_used, term = term, estimate = estimate,
                       std_error = std.error, p_value = p.value, n_obs = eqfit$n_obs)
  }
  dplyr::bind_rows(one(m2_test$eq2), one(m2_test$eq1))
}

# Predict first-test and retest scores at the same attained age.
# For Trails tests, only decreases in completion time are retained as practice effects.
m2_prediction_by_attained_age <- function(m2_test, test_x, model_version,
                                          attained_ages = 50:95,
                                          sex_levels = c("Women", "Men")) {
  score_direction <- if (test_x %in% higher_is_worse_tests) -1L else 1L

  grid <- tidyr::expand_grid(sex_label = factor(sex_levels, levels = sex_levels),
                             attained_age = attained_ages)

  grid$predicted_retest_score <- stats::predict(m2_test$eq1$model,
    newdata = tibble::tibble(age_fu1 = grid$attained_age, sex_label = grid$sex_label))
  grid$predicted_firsttest_score <- stats::predict(m2_test$eq2$model,
    newdata = tibble::tibble(age_bl = grid$attained_age, sex_label = grid$sex_label))

  grid |>
    dplyr::mutate(model_version = model_version, test = test_x,
                  practice_effect_raw = predicted_retest_score - predicted_firsttest_score,
                  score_direction = score_direction,
                  # keep the practice effect only when it points the improvement way, else 0
                  practice_effect_used_raw = dplyr::if_else(score_direction * practice_effect_raw > 0,
                                                            practice_effect_raw, 0)) |>
    dplyr::select(model_version, test, sex_label, attained_age,
                  predicted_firsttest_score, predicted_retest_score,
                  practice_effect_raw, score_direction, practice_effect_used_raw)
}

# Per-participant Method 2 correction for one test x sex stratum, using each record's exact
# attained age u_i = baseline_age_i + followup_years_i. Returned row-for-row with obs_stratum
# so it can be subtracted from the raw change before annualizing.
participant_m2_correction <- function(obs_stratum, m2_test, test_x, sex_x) {
  score_direction <- if (test_x %in% higher_is_worse_tests) -1L else 1L
  u  <- obs_stratum$baseline_age + obs_stratum$followup_years
  sx <- factor(sex_x, levels = c("Women", "Men"))

  raw <- stats::predict(m2_test$eq1$model, newdata = tibble::tibble(age_fu1 = u, sex_label = sx)) -
         stats::predict(m2_test$eq2$model, newdata = tibble::tibble(age_bl  = u, sex_label = sx))

  tibble::tibble(dbgap_subject_id = obs_stratum$dbgap_subject_id,
                 practice_effect_raw = raw,
                 practice_effect_used_raw = dplyr::if_else(score_direction * raw > 0, raw, 0))
}


# --- Table 4 method workbook (local writer for notebook 07 outputs) -----------
# One workbook per method, with both classes as c1_<test> / c2_<test> sheets. 
# Each sheet stacks the three Table 4 forms top-to-bottom (cumulative count, cumulative percentage, exclusive count), 
# reusing the production Table 4 layout from write_table4_workbook().

write_table4_method_workbook <- function(classified_method, test_dictionary, out_file) {
  forms      <- c("cumulative", "cumulative_pct", "exclusive")
  age_bins   <- seq(50, 85, by = 5)
  age_labels <- c("50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-90")

  f <- "Aptos Narrow"; s <- 12
  st_left    <- openxlsx::createStyle(fontName = f, fontSize = s, halign = "left")
  st_center  <- openxlsx::createStyle(fontName = f, fontSize = s, halign = "center")
  st_right   <- openxlsx::createStyle(fontName = f, fontSize = s, halign = "right")
  st_title   <- openxlsx::createStyle(fontName = f, fontSize = s, halign = "left", textDecoration = "bold")
  st_header  <- openxlsx::createStyle(fontName = f, fontSize = s, halign = "center",
                                      border = "top", borderStyle = "thin")
  st_section <- openxlsx::createStyle(fontName = f, fontSize = s, halign = "center",
                                      border = "TopBottom", borderStyle = "thin")
  st_note    <- openxlsx::createStyle(fontName = f, fontSize = s, halign = "left", valign = "top",
                                      wrapText = TRUE, border = "top", borderStyle = "thin")
  st_pct     <- openxlsx::createStyle(fontName = f, fontSize = s, halign = "right", numFmt = "0.0%")

  footnote <- paste(
    "Table 4 in three stacked forms (top to bottom): cumulative count, cumulative percentage,",
    "exclusive count. Thresholds are the expected h-year aging change from reference start age",
    "A-h up to age A, computed at each participant's exact baseline age. N is the number of",
    "baseline-to-first-follow-up records with baseline age 50-90."
  )

  make_block <- function(table4_wide, sex_x, test_x) {
    dplyr::tibble(age_bin = age_bins, Age = age_labels) |>
      dplyr::left_join(dplyr::filter(table4_wide, test == test_x, sex_label == sex_x), by = "age_bin") |>
      dplyr::arrange(age_bin) |>
      dplyr::transmute(Age, N, `5` = `N reversal 5yr`, `10` = `N reversal 10yr`,
                       `15` = `N reversal 15yr`, `20` = `N reversal 20yr`)
  }

  draw_form_block <- function(wb, sheet, table4_wide, test_x, form, r0) {
    is_pct <- (form == "cumulative_pct")
    form_tag <- switch(form, cumulative = "Cumulative count",
                       cumulative_pct = "Cumulative percentage", exclusive = "Exclusive count")
    title <- paste0('Table 4 (', form_tag, '). ',
                    if (is_pct) "Percentage of men and women " else "Counts of men and women ",
                    'naturally exhibiting apparent "reversal" of cognitive aging ',
                    '(baseline to first follow-up)')
    openxlsx::writeData(wb, sheet, title, startRow = r0, startCol = 2)
    openxlsx::addStyle(wb, sheet, st_title, rows = r0, cols = 2)

    openxlsx::writeData(wb, sheet, "N", startRow = r0 + 1, startCol = 3)
    openxlsx::writeData(wb, sheet, if (is_pct) "% with Reversal" else "N with Reversal",
                        startRow = r0 + 1, startCol = 4)
    openxlsx::mergeCells(wb, sheet, cols = 4:7, rows = r0 + 1)
    openxlsx::addStyle(wb, sheet, st_header, rows = r0 + 1, cols = 2:7, gridExpand = TRUE)

    openxlsx::writeData(wb, sheet, "Age", startRow = r0 + 2, startCol = 2)
    col_labels <- if (form == "exclusive") c("Max 5", "Max 10", "Max 15", "Max 20") else
                                           c("5+", "10+", "15+", "20+")
    openxlsx::writeData(wb, sheet, matrix(col_labels, nrow = 1),
                        startRow = r0 + 2, startCol = 4, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, st_left,   rows = r0 + 2, cols = 2)
    openxlsx::addStyle(wb, sheet, st_center, rows = r0 + 2, cols = 4:7, gridExpand = TRUE)

    openxlsx::writeData(wb, sheet, "WOMEN", startRow = r0 + 3, startCol = 2)
    openxlsx::mergeCells(wb, sheet, cols = 2:7, rows = r0 + 3)
    openxlsx::addStyle(wb, sheet, st_section, rows = r0 + 3, cols = 2:7, gridExpand = TRUE)
    openxlsx::writeData(wb, sheet, make_block(table4_wide, "Women", test_x),
                        startRow = r0 + 4, startCol = 2, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, st_right, rows = (r0 + 4):(r0 + 11), cols = 2:7, gridExpand = TRUE)
    if (is_pct) openxlsx::addStyle(wb, sheet, st_pct, rows = (r0 + 4):(r0 + 11), cols = 4:7,
                                   gridExpand = TRUE, stack = TRUE)

    openxlsx::writeData(wb, sheet, "MEN", startRow = r0 + 12, startCol = 2)
    openxlsx::mergeCells(wb, sheet, cols = 2:7, rows = r0 + 12)
    openxlsx::addStyle(wb, sheet, st_section, rows = r0 + 12, cols = 2:7, gridExpand = TRUE)
    openxlsx::writeData(wb, sheet, make_block(table4_wide, "Men", test_x),
                        startRow = r0 + 13, startCol = 2, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, st_right, rows = (r0 + 13):(r0 + 20), cols = 2:7, gridExpand = TRUE)
    if (is_pct) openxlsx::addStyle(wb, sheet, st_pct, rows = (r0 + 13):(r0 + 20), cols = 4:7,
                                   gridExpand = TRUE, stack = TRUE)

    r0 + 22   # one blank spacer row before the next form block
  }

  class_prefix <- c(class1 = "c1", class2 = "c2")
  test_order   <- test_dictionary |> dplyr::arrange(xprize_subdomain, test) |> dplyr::pull(test)

  wb <- openxlsx::createWorkbook()

  purrr::walk(c("class1", "class2"), function(cls) {
    cls_data <- dplyr::filter(classified_method, model_class == cls)
    if (nrow(cls_data) == 0) return(invisible())
    t4 <- purrr::set_names(
      purrr::map(forms, \(form) make_table4(cls_data, test_dictionary, form = form)), forms)

    purrr::walk(intersect(test_order, unique(cls_data$test)), function(test_x) {
      sheet <- substr(paste0(class_prefix[[cls]], "_", test_x), 1, 31)
      openxlsx::addWorksheet(wb, sheet)
      openxlsx::setColWidths(wb, sheet, cols = 2:7, widths = 11)

      r <- purrr::reduce(forms,
        \(row, form) draw_form_block(wb, sheet, t4[[form]], test_x, form, row), .init = 3)

      openxlsx::writeData(wb, sheet, footnote, startRow = r, startCol = 2)
      openxlsx::mergeCells(wb, sheet, cols = 2:7, rows = r)
      openxlsx::addStyle(wb, sheet, st_note, rows = r, cols = 2:7, gridExpand = TRUE)
      openxlsx::setRowHeights(wb, sheet, rows = r, heights = 130)
    })
  })

  openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)
  message("Saved ", out_file, " -- ", length(openxlsx::sheets(wb)), " sheets")
}
