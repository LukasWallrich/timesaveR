# timesaveR 0.0.1.9006

## New capabilities

* Added `svy_miss_var_summary()` to quickly check for missing data in survey objects
* Added `polr_std()` function to run proportional-odds model with continuous predictors standardised (analogous to `lm_std()`)

## Enhancements

* Added a tidier for correlation matrices returned by `cor_matrix()`

## Bug fixes

* p values > .99 are now correctly reported as "> .99" rather than rounded to 1 by `fmt_p()`