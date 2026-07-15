# DATA

This folder intentionally stores the data files, which held the individual-level Framingham (FHS) analysis data.

## What was is

One long-format row per participant × test × visit across the three pooled cohorts (Original, Offspring, Gen3) and 14 neuropsychological tests.

## How it was made

Output of codebook `00_datapre_qc` (`code/neuropsych_pooled3cohorts_00_datapre_qc.Rmd`). That step imports the upstream all-tests model-ready file, keeps the three cohorts (`idtype` 0/1/3), rebuilds a calendar-aligned birth year, centers the covariates.

It is the input every later step loads through `code/R/setup_analysis_sample.R`.

## What it contained

### ID

* `dbgap_subject_id`
* `shareid`

### Cohort and sex

* `idtype`
* `idtype_label` (Original / Offspring / Gen3)

### Sex

* `sex`
* `sex_label` (Women / Men)

### Test and outcome

* `test` (test code)
* `score`

### Age and visit

* `age_at_measure`
* `age`
* `np_date` (visit date)
* `baseline_np_date`
* `baseline_age`
* `t_followup` (years since baseline)
* `cage_65` (age centered at 65)

`t_followup` is the Class 2 time variable; `cage_65` is the Class 1 time variable.

### Education

* `education_b1`
* `education_b2`
* `educg`
* `eayrs`
* `maxeducg`
* `maxeayrs`

### Occupation and employment

* `employment`
* `occup_b1`
* `occup_b2`
* `oag`
* `maxoag`

### Other demographics

* `marital`
* `birth_year`
* `birth_year_c` (centered at the pooled median)
* `birth_year_sd`
* `n_birth_year_obs`

The crude pipeline only requires `test`, `dbgap_subject_id`, `idtype`, `sex_label`, `score`, `age`/`baseline_age`, `t_followup`, `cage_65`, and `birth_year_c`; the education and occupation columns support future covariate-adjusted runs.
