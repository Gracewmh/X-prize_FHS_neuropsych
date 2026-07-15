# FITTED MODELS

This folder held the fitted mixed-effects model objects. These are individual-level: each fitted `lmer` object carries the participant rows it was fit on through its model frame, and the saved tibble also keeps a nested `data` column containing those rows.

## What was here

* `pooled3_crude_2visit_class1_fits.rds`
* `pooled3_crude_2visit_class2_fits.rds`

## How they were made

Output of codebook `01_fit_models_crude_2visit`:

`code/neuropsych_pooled3cohorts_01_fit_models_crude_2visit.Rmd`

The models were fitted on the 2-visit sample, defined as participants with at least two model-ready visits for each test.

* **Class 1:** age-as-time model, with `cage_65` as the time variable.
* **Class 2:** follow-up-as-time model, quadratic in `t_followup` and linear for a small number of tests.

Each file contains one row per test × sex stratum. Alongside the fitted model, each row records the random-effect structure, model formula, sample sizes, and any convergence messages. Both model classes use cohort-level and subject-level random effects.

## What loads them

Every downstream step reads these RDS files:

* Notebook 02: formatted tables
* Notebooks 03 and 04: life-course plots
* Notebooks 05 and 06: spontaneous-reversal analyses
* Notebook 07: practice-effect-adjusted analyses

Notebook 07 also reuses the original Class 1 and Class 2 fitted models.

## To recreate them

Restore the data folder as described in `../../data/README.txt`, then run codebook `01_fit_models_crude_2visit`.

The codebook writes both RDS files back to this folder using the same filenames.
