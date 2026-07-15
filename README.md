# X-prize_FHS_neuropsych

Code for the XPrize cognitive aging analysis using pooled data from the Framingham Original, Offspring, and Gen3 cohorts. The pipeline fits two longitudinal model classes and produces life-course curves, Tables 1–4, spontaneous-reversal results, and practice-effect-adjusted analyses.

# X-PRIZE NEUROPSYCH PIPELINE

## PROJECT OVERVIEW

The work is part of the X-prize project on cognitive aging. This project examines change in cognitive test performance using pooled data from three Framingham Heart Study (FHS) cohorts: Original, Offspring, and Generation 3.

For each cognitive test, models are fitted separately for women and men. The pipeline produces estimates of cognitive performance by age, expected change over follow-up, percentage decline, and the frequency of apparent "spontaneous reversal" of cognitive aging.

## COGNITIVE TESTS

The analysis includes tests from several cognitive domains:

* Logical Memory (immediate, delayed, recognition): Memory
* Visual Reproductions (immediate, delayed, recognition): Visual Learning
* Paired Associates (immediate, delayed, recognition): Memory
* Digit Span Forward and Backward: Attention / Executive Function
* Trail Making Test A and B: Psychomotor / Executive Function
* WAIS-IV Coding: Psychomotor

For Trails A and Trails B, higher values indicate worse performance because the outcome is completion time. For all other tests, higher scores indicate better performance.

The full test list and test metadata are defined in `make_pooled_test_dictionary()` in `R/workbook_writers.R`.

## MODEL CLASSES

The pipeline fits two model classes using different time scales:

* **Class 1 (age as time):** models test score as a function of attained age across all available visits.

* **Class 2 (follow-up as time):** models within-person change over follow-up, conditional on baseline age.

Each model class is fitted separately by cognitive test and sex. The models in this version are crude models, meaning that no additional covariates are included beyond the variables specified in the model formulas.

In output files, C1 and C2 refer to Class 1 and Class 2. They do not represent different types of practice effect.

## ANALYSIS SAMPLE

This version uses the crude 2-visit+ sample. For each cognitive test, the analysis includes participants with at least two model-ready visits for that test.

## SPONTANEOUS REVERSAL

Spontaneous reversal is defined by comparing each participant's observed change from baseline to first follow-up with a model-based aging threshold.

The thresholds represent the expected aging-related change over 5-, 10-, 15-, or 20-year horizons. A participant is classified as showing apparent reversal at a given horizon when the observed change is more favorable than the corresponding model-based threshold.

Table 4 analyses are restricted to participants with baseline ages from 50 to 90 years.

The original spontaneous-reversal analyses are implemented in `R/spontaneous_reversal_functions.R` and notebooks 05 and 06.

## PRACTICE-EFFECT CORRECTION

Codebook 07 repeats the spontaneous-reversal classification for all cognitive tests using three approaches to reduce bias from repeated test-taking:

* **Method 1 (M1):** refits the Class 1 and Class 2 models with categorical prior test count (`prior_count_f`: 0, 1, 2, or 3+) entered as a main effect only. Practice-free thresholds are generated with `prior_count_f` fixed at 0. The observed participant-level change is not modified.

* **Method 2 (M2):** estimates the expected retest advantage at the same attained age using separate first-test and retest regressions. The estimated practice effect is subtracted from the observed baseline-to-follow-up change before reversal classification. The original model-based thresholds are retained.

* **Method 3 (M3):** combines the M1 practice-free thresholds with the M2 participant-level practice-effect correction.

The M2 practice-effect estimate itself is shared across model classes. C1 and C2 results are both reported because the adjusted observed change is compared with different Class 1 and Class 2 thresholds.

For Trails A and Trails B, improvement corresponds to a decrease in completion time. For all other tests, improvement corresponds to an increase in score. Only estimated practice effects in the direction of improved performance are subtracted in M2 and M3.

## FOLDER STRUCTURE

```text
code/            R Markdown pipeline files and supporting R code
data/            Input data
output/          Tables, plots, and saved model objects
```

The shared release does not include individual-level data or fitted model objects. The `data/` and `output/models/` directories are distributed as empty folders. Each directory contains a `README.txt` describing the expected files and how they can be recreated.

## HOW TO RUN THE PIPELINE

Run the R Markdown notebooks in order:

```text
00 -> 01 -> 02 -> 03 -> 04 -> 05 -> 06 -> 07
```

The order matters because later notebooks use files created by earlier steps. For example, notebook 01 fits and saves the models, and downstream notebooks read those saved model objects.

Notebook 00 prepares and checks the pooled analysis data. Notebook 01 fits the crude models. Notebooks 02-04 generate the main tables and life-course curves. Notebooks 05 and 06 perform the original spontaneous-reversal analyses. Notebook 07 performs the practice-effect-adjusted analyses.

## MAIN OUTPUTS

* **Table 1:** Predicted mean test scores at selected index ages.

* **Table 2:** Expected test-score change over 5, 10, 15, and 20 years.

* **Table 3:** Percentage decline calculated from Tables 1 and 2.

* **Table 4:** Counts and percentages classified as showing apparent spontaneous reversal. Codebooks 05 and 06 produce the original results, and codebook 07 produces the M1, M2, and M3 practice-effect-adjusted results.

Codebook 07 also exports M2 regression specifications, regression coefficients, and predicted practice effects by attained age and sex.

## SOFTWARE

The pipeline is written in R. Main packages include `tidyverse`, `lme4`, `lmerTest`, `emmeans`, `openxlsx`, `janitor`, `broom`, and `broom.mixed`.

Shared functions are loaded through `R/_load_all.R`.
