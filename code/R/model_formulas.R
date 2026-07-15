# model_formulas.R
# Purpose: Construct lme4 mixed-effects formula strings/objects for Class 1
#          (age-as-time) and Class 2 (follow-up-as-time) pooled models.
# Used by: Notebook 01 (model fitting) via _load_all.R.
# Main functions provided: make_class1_formula_text(), make_class2_formula_text(),
#          make_pooled_formula_text(), make_pooled_formula()
# Expected inputs: Model class tag, covariate list, random-slope flag.
# Main outputs: Formula text strings or lme4 formula objects.
# Notes for release: Formula structure is fixed by study protocol; do not modify.

# Class 1: score ~ age + age^2 + birth_year + (1 + age | cohort) + (1 [+ age] | subject)
make_class1_formula_text <- function(
    covariates = NULL,
    subject_random_slope = TRUE
) {

  base_fixed_effects <- c(
    "cage_65 + I(cage_65^2)",
    "birth_year_c"
  )

  cohort_random_effects <- c(
    "(1 + cage_65 | idtype_label)"
  )

  subject_random_effects <- if (subject_random_slope) {
    "(1 + cage_65 | dbgap_subject_id)"
  } else {
    "(1 | dbgap_subject_id)"
  }

  model_terms <- c(
    base_fixed_effects,
    covariates,
    cohort_random_effects,
    subject_random_effects
  )

  paste(
    "score ~",
    paste(model_terms, collapse = " + ")
  )
}

# Class 2: score ~ baseline_age * followup [+ followup^2] + birth_year + ranefs
make_class2_formula_text <- function(
    covariates = NULL,
    subject_random_slope = TRUE,
    class2_model_type = c("quadratic", "linear")
) {

  class2_model_type <- match.arg(class2_model_type)

  if (class2_model_type == "linear") {
    base_fixed_effects <- c(
      "baseline_age * t_followup",
      "birth_year_c"
    )
  } else {
    base_fixed_effects <- c(
      "baseline_age * (t_followup + I(t_followup^2))",
      "birth_year_c"
    )
  }

  cohort_random_effects <- c(
    "(1 + t_followup | idtype_label)"
  )

  subject_random_effects <- if (subject_random_slope) {
    "(1 + t_followup | dbgap_subject_id)"
  } else {
    "(1 | dbgap_subject_id)"
  }

  model_terms <- c(
    base_fixed_effects,
    covariates,
    cohort_random_effects,
    subject_random_effects
  )

  paste(
    "score ~",
    paste(model_terms, collapse = " + ")
  )
}


# Dispatcher: returns the formula text string for either model class.
make_pooled_formula_text <- function(
    model_class = c("class1", "class2"),
    covariates = NULL,
    subject_random_slope = TRUE,
    class2_model_type = c("quadratic", "linear")
) {

  model_class <- match.arg(model_class)
  class2_model_type <- match.arg(class2_model_type)

  if (model_class == "class1") {
    make_class1_formula_text(
      covariates = covariates,
      subject_random_slope = subject_random_slope
    )
  } else {
    make_class2_formula_text(
      covariates = covariates,
      subject_random_slope = subject_random_slope,
      class2_model_type = class2_model_type
    )
  }
}


make_pooled_formula <- function(
    model_class = c("class1", "class2"),
    covariates = NULL,
    subject_random_slope = TRUE,
    class2_model_type = c("quadratic", "linear")
) {
  class2_model_type <- match.arg(class2_model_type)

  as.formula(
    make_pooled_formula_text(
      model_class = model_class,
      covariates = covariates,
      subject_random_slope = subject_random_slope,
      class2_model_type = class2_model_type
    )
  )
}
