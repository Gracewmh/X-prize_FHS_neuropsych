# model_fitting.R
# Purpose: Fit lme4 mixed-effects models with automatic fallback from subject
#          random slope to random intercept when the slope is not identifiable.
# Used by: Notebook 01 via _load_all.R.
# Main functions provided: fit_pooled_model(), fit_pooled_model_with_fallback(),
#          fit_one_pooled_model_class(), get_lmer_convergence_message()
# Expected inputs: Formula objects, model_ready_analysis tibble, test × sex grid.
# Main outputs: Tibble of fitted model objects with metadata (n_obs, n_subjects,
#          convergence status, fallback flag).
# Notes for release: Uses BOBYQA optimizer; fallback is logged per stratum.

fit_pooled_model <- function(
    dat,
    model_class = c("class1", "class2"),
    covariates = NULL,
    subject_random_slope = TRUE,
    control = NULL,
    class2_model_type = c("quadratic", "linear")
) {

  model_class <- match.arg(model_class)
  class2_model_type <- match.arg(class2_model_type)

  if (is.null(control)) {
    control <- lme4::lmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 200000)
    )
  }

  model_formula <- make_pooled_formula(
    model_class = model_class,
    covariates = covariates,
    subject_random_slope = subject_random_slope,
    class2_model_type = class2_model_type
  )

  lme4::lmer(
    formula = model_formula,
    data = dat,
    REML = FALSE,
    control = control
  )
}


# Try the full random-slope model first; if it fails (singular or
# non-convergent), fall back to random-intercept-only for the subject level.
fit_pooled_model_with_fallback <- function(
    dat,
    model_class = c("class1", "class2"),
    covariates = NULL,
    control = NULL,
    class2_model_type = c("quadratic", "linear")
) {

  model_class <- match.arg(model_class)
  class2_model_type <- match.arg(class2_model_type)

  safe_fit <- purrr::safely(
    fit_pooled_model,
    otherwise = NULL,
    quiet = TRUE
  )

  primary_fit <- safe_fit(
    dat = dat,
    model_class = model_class,
    covariates = covariates,
    subject_random_slope = TRUE,
    control = control,
    class2_model_type = class2_model_type
  )

  if (!is.null(primary_fit$result)) {
    return(
      list(
        model = primary_fit$result,
        fit_success = TRUE,
        subject_random_slope_used = TRUE,
        random_effect_structure = "Cohort random intercept/slope + subject random intercept/slope",
        model_formula = make_pooled_formula_text(
          model_class = model_class,
          covariates = covariates,
          subject_random_slope = TRUE,
          class2_model_type = class2_model_type
        ),
        primary_error_message = NA_character_,
        fallback_error_message = NA_character_
      )
    )
  }

  fallback_fit <- safe_fit(
    dat = dat,
    model_class = model_class,
    covariates = covariates,
    subject_random_slope = FALSE,
    control = control,
    class2_model_type = class2_model_type
  )

  if (!is.null(fallback_fit$result)) {
    return(
      list(
        model = fallback_fit$result,
        fit_success = TRUE,
        subject_random_slope_used = FALSE,
        random_effect_structure = "Cohort random intercept/slope + subject random intercept only",
        model_formula = make_pooled_formula_text(
          model_class = model_class,
          covariates = covariates,
          subject_random_slope = FALSE,
          class2_model_type = class2_model_type
        ),
        primary_error_message = condition_message_or_na(primary_fit$error),
        fallback_error_message = NA_character_
      )
    )
  }

  list(
    model = NULL,
    fit_success = FALSE,
    subject_random_slope_used = NA,
    random_effect_structure = NA_character_,
    model_formula = NA_character_,
    primary_error_message = condition_message_or_na(primary_fit$error),
    fallback_error_message = condition_message_or_na(fallback_fit$error)
  )
}


# Fit one model class (class1 or class2) across all test x sex strata.
# Some Class 2 tests use a linear (no quadratic) follow-up term.
fit_one_pooled_model_class <- function(
    grid_x,
    model_class_x,
    covariates = NULL,
    covariates_linear = NULL,
    linear_followup_tests = NULL
) {

  grid_x |>
    dplyr::mutate(
      model_class = model_class_x,
      class2_model_type = dplyr::if_else(
        model_class_x == "class2" & test %in% linear_followup_tests,
        "linear",
        "quadratic"
      ),
      fit_result = purrr::map2(
        data,
        class2_model_type,
        ~ {
          covariates_x <- if (.y == "linear" && !is.null(covariates_linear)) {
            covariates_linear
          } else {
            covariates
          }

          fit_pooled_model_with_fallback(
            dat = .x,
            model_class = model_class_x,
            covariates = covariates_x,
            class2_model_type = .y
          )
        }
      ),
      model = purrr::map(fit_result, "model"),
      fit_success = purrr::map_lgl(fit_result, "fit_success"),
      subject_random_slope_used = purrr::map_lgl(
        fit_result,
        "subject_random_slope_used"
      ),
      random_effect_structure = purrr::map_chr(
        fit_result,
        "random_effect_structure"
      ),
      model_formula = purrr::map_chr(
        fit_result,
        "model_formula"
      ),
      primary_error_message = purrr::map_chr(
        fit_result,
        "primary_error_message"
      ),
      fallback_error_message = purrr::map_chr(
        fit_result,
        "fallback_error_message"
      )
    ) |>
    dplyr::select(-fit_result)
}


get_lmer_convergence_message <- function(model_x) {
  if (is.null(model_x)) {
    return(NA_character_)
  }
  msg <- model_x@optinfo$conv$lme4$messages
  if (is.null(msg)) {
    return(NA_character_)
  }
  paste(msg, collapse = "; ")
}
