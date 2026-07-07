# timesaveR 0.0.3

## New capabilities

* Added `t_test()` - a comprehensive wrapper for t-tests that always returns Cohen's d. Supports one-sample, independent-samples, and paired-samples t-tests with both formula and vector interfaces. This replaces the now-deprecated `paired_t_test_d()`.
* Added `run_mediation()` to estimate and `plot_mediation()` to visualize parallel mediation models.
* Added `run_moderated_mediation()` to estimate first-stage (and optionally direct-path) moderated mediation models with bootstrapped confidence intervals, including the index of moderated mediation and conditional indirect/direct effects at low/mean/high moderator values. `plot_moderated_mediation()` can now take its output to draw a path diagram annotated with the actual estimates (in addition to the existing manual, conceptual-diagram mode).
* `plot_mediation()` now supports automatic layouts for 2 and 4 mediators (previously only 1 and 3).
* Added `na_when()` to set values to NA based on logical conditions, and `na_ifs()` to replace multiple values with NA (naming choice guided by existing dplyr::na_if function)
* Added `make_scale_mi()` to estimate scale scores and Cronbach's alpha after multiple-imputation at the item level. According to [Gottschall, West & Enders (2012)](https://doi.org/10.1080/00273171.2012.640589), this is one of the best ways to deal with item-level missing data. If requested, the function can bootstrap confidence intervals, using the `future`-package for parallel computing.
* Added `pcor_matrix()` to calculate partial correlation matrices after parceling out one or several variable.
* Added `run_and_format()` to run any code and return formatted code and output for sharing - in the style of `reprex::reprex()` but without creating a new session (intended for teaching or sharing code examples, rather than bug reporting).
* Added `paste_()` that mimics `paste()` but removes `NA`-values
* Added `dupl_items()` that returns (unique) duplicated items from vector, removing need for subsetting with `duplicated()`

## Enhancements

* **Breaking change**: `make_scale_mi()` output structure has been standardized to match `make_scale()`. The `descriptives` element now uses `"cronbachs_alpha"` (instead of `"cron_alpha"`) for the `reliability_method` field, includes `reversed` as a space-separated string (instead of a data frame), and adds `rev_min` and `rev_max` fields for consistency with `make_scale()`. Note: `"cron_alpha"` is still accepted as input for backward compatibility.
* `report_cor_table()` and `plot_distributions()` now support survey-weighted data. Distribution plots for survey design objects properly account for sampling weights using `svyhist()` for discrete variables and `svysmooth()` for continuous variables. This ensures that distributions reflect the population rather than just the sample composition.
* Added package-level rounding option `timesaveR.round_method` to control rounding behavior consistently across all formatting and rounding functions. Set to `"to_even"` (default, banker's rounding) or `"default"` (standard R rounding). Change via `options(timesaveR.round_method = "default")`. This ensures all `round_()`, `round_df()`, `fmt_p()`, `fmt_pct()`, `fmt_cor()`, and related functions use the same rounding method.
* `make_scale()` now has a `proration_cutoff` argument to specify the maximum share of missing data ignored in each case. This offers a simple way to improve on casewise deletion for handling missing data, without getting scale scores based on an insufficient subset of items. [NB: This is a *breaking* change, earlier versions implicitly had a proration_cutoff of 1, returning scale scores if at least one item was present - which is indefensible when there is a lot of missing data.]
* Added summary `text` to descriptives returned by `make_scale()`
* Enabled `report_lm_with_std()` to show R2 change for more than one pair of models
* Added option to use `t.test()`-style formula notion in `pairwise_t_tests()`
* Allow automatic reverse-coding in `make_scales()`
* `report_cor_table()` now ensures correct ordering of extra columns if `row_names` column is included
* Added `tidy.svy_cor_matrix()` to tidy survey-weighted correlation matrices
* Renamed `wtd_cor_matrix_mi()` to `cor_matrix_mi()` to reflect that weights are optional
* Added output to README (by using README.Rmd) to make it more informative
* `fmt_p()` gained a `equal_sign` argument that determines whether *p*-values that are reported precisely are prefixed with "= "
* `dump_to_clip()` now accepts objects passed directly, or through the pipe (#5)
* `line_to_vector()` now automatically returns numeric vectors when only numbers are passed, and has gained an option to retain NA values for blank entries. It also allows users to pass one or multiple separators to split at explicitly. `l2v()` is now an alias for `line_to_vector()`.

## Deprecations

* `paired_t_test_d()` is now deprecated in favor of the more comprehensive `t_test()` function. Users should switch to `t_test(x, y, paired = TRUE)` instead.

## Bug fixes
* `report_polr_with_std()` works again after `broom::tidy.polr()` update broke it
* `fmt_p()` now supports greater numbers of significant digits properly
* `fmt_p()` now checks argument types, so that it does not return misleading results when characters are passed (#10)
* `pairwise_t_tests()` can now deal with missing data and labelled arguments (#9)
* `make_scales()` now correctly reacts to `print_desc` argument
* `cor_matrix()` now works robustly with `missing = "fiml"` even when some bootstraps do not converge
* `round_df()` and `round_()` now consistently use round-to-even/banker's rounding
* `report_lm_with_std()` no longer errors when `R2_change = TRUE`, and now gives an informative error instead of failing obscurely when passed `mira` models
* `make_scale()`'s default `two_items_reliability` option, `"cronbachs_alpha"`, now works (the default previously pointed to a misspelled option that always errored), and the Pearson's-*r* option now also works with tibbles
* `make_scale_mi()`'s deprecated `scale_items` argument is now correctly mapped to `items`
* Confidence intervals for bootstrapped Cronbach's alpha in `make_scale_mi()` are now correct (previously far too narrow due to an incorrect *t* critical value)
* `svy_make_scale()` now actually applies `proration_cutoff` and supports `two_items_reliability` - both arguments were previously accepted but silently ignored
* `svy_cor_matrix()` no longer mangles variable names that contain `"_1"`, and `tidy.svy_cor_matrix()` now returns the correct columns, including standard errors and confidence intervals
* `pcor_matrix()` now works when `var_names` is passed as a tibble
* `cor_matrix()` no longer resets the random seed when no seed is provided
* `pairwise_t_tests()` now works when the outcome variable is named `x`, and Cohen's *d* now respects `var_equal = TRUE` (pooled SD)
* `t_test(paired = TRUE)` without `y` now gives an informative error instead of silently running a one-sample test
* `fmt_p()` now handles small `digits` values properly (no longer reports "= 1.0") and returns `NA` for `NA` input
* `sigstars(pad_html = TRUE)` now pads `NA` entries to the same width as other entries
* `clip_excel()` now works cross-platform via `clipr`, and is restricted to interactive sessions
* `setup_analysis_project()` now requires an explicit folder to be specified rather than defaulting to the working directory, and `ggsave_show()` only opens the output folder in interactive sessions
* Deprecation warnings in `plot_mediation()` now reference the correct replacement function

## Other

* Updated code to remove `tidyverse` deprecation warnings (e.g. around `across()` and `.data` usage), so that functions run cleanly on current package versions
* `run_mediation()` now bootstraps sequentially by default; use the new `cores` argument to enable parallel processing (previously it always used all-but-one CPU cores, which violated CRAN check limits and could surprise users)

# timesaveR 0.0.2

## New capabilities

* Added `svy_miss_var_summary()` to quickly check for missing data in survey objects
* Added `polr_std()` function to run proportional-odds model with continuous predictors standardised (analogous to `lm_std()`)
* Added `dummy_code()` for creating k-1 dummies with tidy names
* Added `report_anova()` to report F-tests for model comparisons (limited functionalities at present)

## Enhancements

* Added a broom-like tidier for correlation matrices returned by `cor_matrix()`
* Added time to temporary filenames used by `ggplot_save()`

## Bug fixes

* *p*-values > .99 are now correctly reported as "> .99" rather than rounded to 1 by `fmt_p()`
* `ggsave_show()` now correctly takes file type from file extension
* `report_lm_with_std()` and `report_polr_with_std()` broke when `gt` implemented tidyselect. Fixed again.