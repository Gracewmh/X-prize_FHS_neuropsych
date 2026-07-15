# setup_analysis_sample.R
# Purpose: Load the corrected pooled-3-cohort CSV and construct the analysis
#          sample. Filters to >=2 visits per test when SAMPLE="2visit".
# Used by: Notebooks 01–07 via source("R/setup_analysis_sample.R").
# Main functions provided: None (script, not function library).
# Expected inputs: SAMPLE ("full" or "2visit") and covariates (NULL for crude)
#          must be defined in the calling notebook before sourcing this file.
# Main outputs: model_ready_pool, model_ready_analysis, model_required_vars,
#          plus output directory paths (model_table_dir, etc.).
# Produces in the calling environment:
#   model_ready_pool       - full pooled dataset with factor columns
#   model_ready_analysis   - analysis subset (possibly restricted to >=2 visits)
#   model_required_vars    - character vector of columns required by the model
#   model_version, model_tag, dir_tag - naming helpers derived from SAMPLE
#   model_table_dir        - output directory path (created if needed)

model_version <- if (SAMPLE == "2visit") "crude_2visit" else "crude"
model_tag     <- model_version
dir_tag       <- if (SAMPLE == "2visit") "_2visit" else ""

model_table_dir <- paste0(
  "../output/tables/pooled3_01_models_crude", dir_tag
)

dir.create(
  model_table_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

model_ready_pool <- readr::read_csv(
  "../data/neuropsych_pooled3_model_ready_xprize_with_covariates.csv",
  show_col_types = FALSE
) |>
  dplyr::mutate(
    dbgap_subject_id = factor(dbgap_subject_id),
    idtype = factor(idtype),
    idtype_label = factor(idtype_label),
    sex_label = factor(
      sex_label,
      levels = c("Women", "Men")
    )
  )

model_required_vars <- c(
  "score",
  "cage_65",
  "baseline_age",
  "t_followup",
  "birth_year_c",
  covariates
)

model_ready_pool <- model_ready_pool |>
  dplyr::mutate(
    complete_for_model = dplyr::if_all(
      dplyr::all_of(model_required_vars),
      ~ !is.na(.x)
    )
  )

if (SAMPLE == "2visit") {

  min_visits_per_test <- 2

  visits_per_subject_test <- model_ready_pool |>
    dplyr::filter(complete_for_model) |>
    dplyr::count(test, sex_label, dbgap_subject_id, name = "n_visits")

  repeated_measure_keys <- visits_per_subject_test |>
    dplyr::filter(n_visits >= min_visits_per_test) |>
    dplyr::select(test, dbgap_subject_id)

  model_ready_analysis <- model_ready_pool |>
    dplyr::semi_join(
      repeated_measure_keys,
      by = c("test", "dbgap_subject_id")
    )

} else {
  model_ready_analysis <- model_ready_pool
}
