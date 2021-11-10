# timesaveR dev (towards 0.0.3)

## New capabilities

* Added `run_mediation()` to estimate and `plot_mediation()` to visualise parallel mediation models.

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