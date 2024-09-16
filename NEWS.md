# timesaveR 0.0.3

## New capabilities

* Added `run_mediation()` to estimate and `plot_mediation()` to visualize parallel mediation models.
* Added `na_when()` to set values to NA based on logical conditions, and `na_ifs()` to replace multiple values with NA (naming choice guided by existing dplyr::na_if function)
* Added `make_scale_mi()` to estimate scale scores and Cronbach's alpha after multiple-imputation at the item level. According to [Gottschall, West & Enders (2012)](https://doi.org/10.1080/00273171.2012.640589), this is one of the best ways to deal with item-level missing data. If requested, the function can bootstrap confidence intervals, using the `future`-package for parallel computing.
* Added `pcor_matrix()` to calculate partial correlation matrices after parceling out one or several variable.
* Added `run_and_format()` to run any code and return formatted code and output for sharing - in the style of `reprex::reprex()` but without creating a new session (intended for teaching or sharing code examples, rather than bug reporting).
* Added `paste_()` that mimics `paste()` but removes `NA`-values
* Added `dupl_items()` that returns (unique) duplicated items from vector, removing need for subsetting with `duplicated()`

## Enhancements

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
* `line_to_vector()` now automatically returns numeric vectors when only numbers are passed, and has gained an option to retain NA values for blank entries

## Bug fixes
* `report_polr_with_std()` works again after `broom::tidy.polr()` update broke it
* `fmt_p()` now supports greater numbers of significant digits properly
* `fmt_p()` now checks argument types, so that it does not return misleading results when characters are passed (#10)
* `pairwise_t_tests()` can now deal with missing data and labelled arguments (#9)
* `make_scales()` now correctly reacts to `print_desc` argument
* `cor_matrix()` now works robustly with `missing = "fiml"` even when some bootstraps do not converge

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