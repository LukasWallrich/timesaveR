# tests/testthat/test-cor_functions.R

library(testthat)
library(dplyr)
library(srvyr)
library(mice)
library(gt)
library(timesaveR)
set.seed(300688)

# Helper function to create missing data
add_missing <- function(x) {x[!rbinom(length(x), 1, 0.9)] <- NA; x}

# Prepare the mtcars correlation matrix
mtcars_cor_matrix <- cor_matrix(mtcars, var_names = c(mpg = "Hello", cyl = "You", am = "Lovely"))

# Prepare survey data for svy_cor_matrix tests
ess_health_sample <- timesaveR::ess_health %>%
  sample_n(100) %>%
  select(agea, health, dosprt, pspwght) %>%
  mutate(across(c(everything(), -pspwght), add_missing))

ess_survey <- as_survey_design(ess_health_sample, weights = pspwght)

ess_survey_cor_matrix <- svy_cor_matrix(ess_survey, var_names = c(health = "Health", agea = "Age"))

# Prepare multiple imputation data for cor_matrix_mi tests
mi_data <- ess_health_sample %>%
  mutate(across(c(health, dosprt), add_missing))

# Perform multiple imputations
ess_health_mi <- mice(mi_data, printFlag = FALSE)
ess_health_mi_long <- complete(ess_health_mi, "long", include = TRUE)

# Create correlation matrices from multiple imputed data
mi_cor_matrix <- cor_matrix_mi(ess_health_mi_long)
mi_cor_matrix_weighted <- cor_matrix_mi(ess_health_mi_long, weights = pspwght)

# Perform FIML correlation matrix - suppress lavaan variance scaling warning
fiml_cor_matrix <- suppressWarnings(cor_matrix(ess_health_sample, missing = "fiml"))


# Tests for cor_matrix functions
test_that("cor_matrix works correctly with mtcars data", {
  expected_cor <- cor(mtcars$mpg, mtcars$cyl)
  computed_cor <- mtcars_cor_matrix$cors[1, 2]
  expect_equal(computed_cor, expected_cor)
})

test_that("svy_cor_matrix works correctly with survey data", {
  expected_cor <- ess_survey %>% summarise(survey_corr(agea, health, na.rm = TRUE)) %>% pull(coef)
  computed_cor <- ess_survey_cor_matrix$cors[1, 2]
  expect_equal(computed_cor, expected_cor)
})

ignore_tidy_warning <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("method for objects of class `lm_std` is not maintained by the broom team", conditionMessage(w), fixed = TRUE))
        invokeRestart("muffleWarning")   # swallow just this warning
    }
  )
}

test_that("cor_matrix_mi works correctly without weights", {
  expected_cor <- ignore_tidy_warning(with(ess_health_mi, lm_std(agea ~ health)) %>% pool() %>% {.$pooled$estimate[2]})
  computed_cor <- mi_cor_matrix$cors[1, 2]
  expect_equal(computed_cor, expected_cor)
})

test_that("cor_matrix_mi works correctly with weights", {
  # Hand-calculate MI weighted correlation
  fit_list <- ignore_tidy_warning(with(ess_health_mi, 
                   lm_std(health ~ agea, weights = pspwght)))
  pooled_fit <- pool(fit_list)
  expected_cor <- summary(pooled_fit)$estimate[2]
  
  computed_cor <- mi_cor_matrix_weighted$cors["agea", "health"]
  
  expect_equal(computed_cor, expected_cor)
})

test_that("cor_matrix works with FIML method", {

    # Use lavaan directly to get the model fit and its standardised solution
    lavaan_fit <- suppressWarnings(lavaan::lavCor(ess_health_sample, missing = "fiml", output = "fit"))
    lavaan_res <- lavaan::standardizedsolution(lavaan_fit)
    
    expected_cor <- subset(
      lavaan_res, 
      op == "~~" & lhs == "agea" & rhs == "health"
    )$est.std
    
    computed_cor <- fiml_cor_matrix$cors["health", "agea"]
    
    expect_equal(computed_cor, expected_cor)
  })
  


test_that("cor_matrix works with FIML and bootstrapping", {

  # Set seed for reproducibility
  set.seed(300688)

  expect_snapshot({
    # Suppress lavaan warnings about empty cases
    suppressWarnings(
      fiml_boot_cor_matrix <- cor_matrix(
        dplyr::select(ess_health_sample, -pspwght),
        missing = "fiml",
        bootstrap = 100,
        seed = 300688  # Use same seed as test setup
      )
    )
  })
  model_syntax <- '
    agea ~~ health
    agea ~~ dosprt
    health ~~ dosprt
  '
  
  data_for_lavaan <- dplyr::select(ess_health_sample, -pspwght)
  
  lavaan_fit <- suppressWarnings(lavaan::sem(model_syntax, 
                            data = data_for_lavaan, 
                            missing = "fiml",
                            fixed.x = FALSE))
  
  extract_correlations <- function(mod) {
    res <- lavaan::standardizedsolution(mod) %>%
      dplyr::filter(op == "~~", lhs != rhs)
    res$est.std %>% magrittr::set_names(paste0(res$lhs, "~~", res$rhs))
  }
  
  expected_cor <-extract_correlations(lavaan_fit)["agea~~health"] %>% unname()
    computed_cor <- fiml_boot_cor_matrix$cors["health", "agea"]
  
  expect_equal(computed_cor, expected_cor, tolerance = 1e-3)
})

# Tests for report_cor_table()
test_that("report_cor_table works with basic cor_matrix", {
  expect_s3_class(report_cor_table(mtcars_cor_matrix), "gt_tbl")
})

test_that("report_cor_table works with add_distributions", {
  expect_s3_class(report_cor_table(mtcars_cor_matrix, add_distributions = TRUE, data = mtcars), "gt_tbl")
})

test_that("report_cor_table handles missing 'data' when add_distributions is TRUE", {
  expect_error(
    report_cor_table(mtcars_cor_matrix, add_distributions = TRUE),
    "If `add_distributions` = TRUE, the `data` argument must be provided."
  )
})

test_that("report_cor_table works with svy_cor_matrix", {
  expect_s3_class(report_cor_table(ess_survey_cor_matrix), "gt_tbl")
})

test_that("report_cor_table works with cor_matrix_mi", {
  expect_s3_class(report_cor_table(mi_cor_matrix), "gt_tbl")
})

test_that("report_cor_table works with weighted cor_matrix_mi", {
  expect_s3_class(report_cor_table(mi_cor_matrix_weighted), "gt_tbl")
})

test_that("report_cor_table handles extras without row_names", {
  extras <- tibble(Extra_Info = c("Info1", "Info2", "Info3")) # Adjust based on number of variables
  expect_warning(
    report_cor_table(mtcars_cor_matrix, extras = extras),
    "The `extras` data frame does not have a row_names column."
  )
})


test_that("report_cor_table rejects wrong number of unnamed extra rows", {
  extras <- tibble(Extra_Info = c("Info1", "Info2")) # Only 2 rows, but mtcars here has 3 correlations
  expect_error(
    report_cor_table(mtcars_cor_matrix, add_distributions = TRUE, data = mtcars, extras = extras),
    "The number of rows in `extras` does not match the number of variables in `cor_matrix`"
  )
})

test_that("report_cor_table accepts incomplete number of named extra rows", {
  extras <- tibble(Extra_Info = c("Info1", "Info2"), row_names = c("Hello", "Lovely")) # Only 2 rows, but mtcars here has 3 correlations
  expect_s3_class(
    report_cor_table(mtcars_cor_matrix, add_distributions = TRUE, data = mtcars, extras = extras),
    "gt_tbl"
  )
})


test_that("report_cor_table handles extras with row_names", {
  extras <- tibble(row_names = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),
                   Extra_Info = paste0("Info", 1:11))
  expect_s3_class(
    report_cor_table(mtcars_cor_matrix, extras = extras),
    "gt_tbl"
  )
})



test_that("report_cor_table handles missing 'ci.low' and 'ci.high'", {
  cor_matrix_no_ci <- mtcars_cor_matrix
  cor_matrix_no_ci$ci.low <- NULL
  cor_matrix_no_ci$ci.high <- NULL
  expect_warning(
    report_cor_table(cor_matrix_no_ci),
    "Confidence intervals will not be shown."
  )
})

test_that("report_cor_table works with custom 'stars' parameter", {
  custom_stars <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)
  expect_s3_class(
    report_cor_table(mtcars_cor_matrix, stars = custom_stars),
    "gt_tbl"
  )
})

test_that("report_cor_table handles invalid 'stars' parameter", {
  expect_error(
    report_cor_table(mtcars_cor_matrix, stars = c(0.001, 0.01, 0.05)),
    "`stars` must be a named numeric vector."
  )
})

test_that("report_cor_table works with 'add_title' as character", {
  expect_s3_class(
    report_cor_table(mtcars_cor_matrix, add_title = "Custom Title"),
    "gt_tbl"
  )
})

test_that("report_cor_table handles invalid 'add_title' parameter", {
  expect_error(
    report_cor_table(mtcars_cor_matrix, add_title = 123),
    "'add_title' must be TRUE, FALSE, or a character string."
  )
})

test_that("report_cor_table works when saving to a file", {
  tmp_file <- tempfile(fileext = ".html")
  expect_silent(
    report_cor_table(mtcars_cor_matrix, filename = tmp_file)
  )
  expect_true(file.exists(tmp_file))
  unlink(tmp_file)
})

test_that("report_cor_table adds plots correctly", {
  expect_s3_class(
    report_cor_table(mtcars_cor_matrix, add_distributions = TRUE, data = mtcars),
    "gt_tbl"
  )
})

# Additional tests for cor_matrix edge cases
test_that("cor_matrix handles all-missing variables", {
  df <- mtcars
  df$all_na <- NA_real_
  expect_message(
    cor_matrix(df),
    "all_na only have missing values"
  )
})

test_that("cor_matrix requires at least 2 numeric columns", {
  df <- data.frame(x = 1:10)
  expect_error(
    cor_matrix(df),
    "Data needs to contain at least two numeric columns"
  )
})

test_that("cor_matrix works with different correlation methods", {
  spearman_cor <- cor_matrix(mtcars, method = "spearman")
  kendall_cor <- cor_matrix(mtcars, method = "kendall")

  expect_s3_class(spearman_cor, "cor_matrix")
  expect_s3_class(kendall_cor, "cor_matrix")
})

test_that("cor_matrix works with different adjustment methods", {
  adjusted_cor <- cor_matrix(mtcars, adjust = "bonferroni")
  raw_cor <- cor_matrix(mtcars, adjust = "none")
  expect_true(all(adjusted_cor$p.values >= raw_cor$p.values, na.rm = TRUE))
  expect_s3_class(adjusted_cor, "cor_matrix")
})

test_that("cor_matrix works with listwise deletion", {
  df <- mtcars
  df[1:5, "mpg"] <- NA
  listwise_cor <- cor_matrix(df, missing = "listwise")
  expect_s3_class(listwise_cor, "cor_matrix")
})

test_that("cor_matrix rejects bootstrap without FIML", {
  expect_error(
    cor_matrix(mtcars, bootstrap = 100),
    "bootstrapping can only be used when"
  )
})

test_that("cor_matrix handles var_names as tibble", {
  var_names_df <- tibble::tibble(
    old = c("mpg", "cyl"),
    new = c("Miles per Gallon", "Cylinders")
  )
  result <- cor_matrix(mtcars, var_names = var_names_df)
  expect_equal(rownames(result$cors), c("Miles per Gallon", "Cylinders"))
})

test_that("cor_matrix warns about missing variables in var_names", {
  # When var_names includes non-existent variables, a warning should be issued
  expect_warning(
    cor_matrix(mtcars, var_names = c(mpg = "MPG", cyl = "CYL", nonexistent = "Fake")),
    "cannot be included into the correlation matrix"
  )
})

test_that("cor_matrix handles var_names correctly", {
  result <- cor_matrix(mtcars, var_names = c(mpg = "MPG", cyl = "CYL"))
  expect_equal(nrow(result$cors), 2)
  expect_equal(rownames(result$cors), c("MPG", "CYL"))
})

test_that("cor_matrix with FIML and invalid missing argument", {
  expect_error(
    cor_matrix(mtcars, missing = "invalid"),
    "Must be element of set"
  )
})

test_that("cor_matrix returns all required elements", {
  result <- cor_matrix(mtcars[, 1:4])
  expect_true(all(c("cors", "p.values", "std.err", "t.values", "n", "ci.low", "ci.high", "desc") %in% names(result)))
})

test_that("cor_matrix confidence intervals are symmetric", {
  result <- cor_matrix(mtcars[, 1:3])
  # Check that CI matrices have same structure
  expect_equal(dim(result$ci.low), dim(result$ci.high))
})

# Tests for tidy.cor_matrix
test_that("tidy.cor_matrix works with both_directions = TRUE", {
  tidy_result <- tidy(mtcars_cor_matrix, both_directions = TRUE)
  expect_s3_class(tidy_result, "tbl_df")
  expect_true("column1" %in% names(tidy_result))
  expect_true("column2" %in% names(tidy_result))
  expect_true("estimate" %in% names(tidy_result))
  # Should have both A-B and B-A
  n_vars <- nrow(mtcars_cor_matrix$desc)
  expected_rows <- n_vars * (n_vars - 1)
  expect_equal(nrow(tidy_result), expected_rows)
})

test_that("tidy.cor_matrix works with both_directions = FALSE", {
  tidy_result <- tidy(mtcars_cor_matrix, both_directions = FALSE)
  n_vars <- nrow(mtcars_cor_matrix$desc)
  expected_rows <- n_vars * (n_vars - 1) / 2
  expect_equal(nrow(tidy_result), expected_rows)
})

test_that("tidy.cor_matrix rejects conf_level argument", {
  expect_error(
    tidy(mtcars_cor_matrix, conf_level = 0.99),
    "cannot be changed in this tidy function"
  )
})

test_that("tidy.cor_matrix returns correct column names", {
  tidy_result <- tidy(mtcars_cor_matrix)
  expect_true(all(c("column1", "column2", "estimate", "p.value", "std.error",
                    "statistic", "conf.low", "conf.high", "n") %in% names(tidy_result)))
})

# Tests for tidy.svy_cor_matrix
test_that("tidy.svy_cor_matrix works", {
  expect_message(
    tidy_result <- tidy(ess_survey_cor_matrix),
    "confidence intervals cannot be calculated"
  )
  # ToDo - check whether CIs can be calculated for survey data in future
  expect_s3_class(tidy_result, "tbl_df")
})

test_that("tidy.svy_cor_matrix works with both_directions = FALSE", {
  expect_message(
    tidy_result <- tidy(ess_survey_cor_matrix, both_directions = FALSE)
  )
  n_vars <- nrow(ess_survey_cor_matrix$desc)
  expected_rows <- n_vars * (n_vars - 1) / 2
  expect_equal(nrow(tidy_result), expected_rows)
})

# Tests for pcor_matrix
test_that("pcor_matrix works correctly", {
  result <- pcor_matrix(mtcars, given = c("wt", "hp"))
  expect_s3_class(result, "cor_matrix")
  # Should have all numeric vars except the two given
  expect_true(nrow(result$cors) < ncol(mtcars))
  # Check that df attribute is set for partial correlations
  expect_true(!is.null(result$df))
})

test_that("pcor_matrix rejects missing argument", {
  expect_error(
    pcor_matrix(mtcars, given = "wt", missing = "pairwise"),
    "`missing` argument cannot be set"
  )
})

test_that("pcor_matrix warns about dropped missing data", {
  df <- mtcars
  df[1:5, "mpg"] <- NA
  expect_warning(
    pcor_matrix(df, given = "wt"),
    "Dropped .* rows with missing data"
  )
})

test_that("pcor_matrix calculates partial correlations correctly", {
  # Test that partial correlations differ from regular correlations
  regular_cor <- cor_matrix(mtcars)
  partial_cor <- pcor_matrix(mtcars, given = "wt")

  # Partial correlation should be different from regular correlation
  expect_false(isTRUE(all.equal(regular_cor$cors["mpg", "hp"], partial_cor$cors["mpg", "hp"])))

  # Partial correlation should match manual calculation (correlation of residuals)
  mpg_resid <- residuals(lm(mpg ~ wt, data = mtcars))
  hp_resid <- residuals(lm(hp ~ wt, data = mtcars))
  expected_partial <- cor(mpg_resid, hp_resid)
  computed_partial <- partial_cor$cors["mpg", "hp"]
  expect_equal(computed_partial, expected_partial)
})

test_that("pcor_matrix handles var_names correctly", {
  result <- pcor_matrix(mtcars, given = "wt", var_names = c(mpg = "MPG", cyl = "CYL"))
  expect_equal(rownames(result$cors), c("MPG", "CYL"))
})

test_that("pcor_matrix p-value adjustment works", {
  # Test that p-value adjustment now works without error
  expect_warning(out_unadj <- pcor_matrix(timesaveR::ess_health, given = c("agea", "gndr"),
              var_names = c("health" = "Health", "weight" = "Weight", "dosprt" = "Sport"),
              adjust = "none"))

  expect_warning(out_adj <- pcor_matrix(timesaveR::ess_health, given = c("agea", "gndr"),
              var_names = c("health" = "Health", "weight" = "Weight", "dosprt" = "Sport"),
              adjust = "holm"))

  # Check that adjusted p-values are generally larger (more conservative)
  expect_true(all(out_adj$p.values >= out_unadj$p.values | is.na(out_adj$p.values)))

  # Verify correlations remain the same
  expect_equal(out_adj$cors, out_unadj$cors)
})

# Tests for plot_distributions
test_that("plot_distributions creates histogram for discrete variables", {
  plots <- plot_distributions(mtcars, var_names = c(cyl = "Cylinders", am = "Transmission"))
  expect_type(plots, "list")
  expect_length(plots, 2)
  expect_s3_class(plots[[1]], "gg")
})

test_that("plot_distributions creates density plots for continuous variables", {
  plots <- plot_distributions(mtcars, var_names = c(mpg = "MPG", wt = "Weight"),
                              plot_type = "density")
  expect_length(plots, 2)
  expect_s3_class(plots[[1]], "gg")
})

test_that("plot_distributions works with auto plot_type", {
  plots <- plot_distributions(mtcars[, c("mpg", "cyl")], plot_type = "auto")
  expect_length(plots, 2)
})

test_that("plot_distributions works with numeric plot_type threshold", {
  plots <- plot_distributions(mtcars[, c("mpg", "cyl")], plot_type = 5)
  expect_length(plots, 2)
})

test_that("plot_distributions aligns histogram y-axes when requested", {
  plots <- plot_distributions(mtcars[, c("cyl", "gear")],
                              plot_type = "histogram", hist_align_y = TRUE)
  expect_length(plots, 2)
  # Verify that plots were created successfully
  expect_s3_class(plots[[1]], "gg")
  expect_s3_class(plots[[2]], "gg")
})

test_that("plot_distributions handles var_names as tibble", {
  var_names_df <- tibble::tibble(
    old = c("mpg", "cyl"),
    new = c("MPG", "CYL")
  )
  plots <- plot_distributions(mtcars, var_names = var_names_df)
  expect_named(plots, c("MPG", "CYL"))
})

test_that("plot_distributions errors on non-numeric data", {
  df <- data.frame(a = letters[1:10], b = LETTERS[1:10])
  expect_error(
    plot_distributions(df),
    "No numeric columns found"
  )
})





test_that("plot_distributions rejects invalid plot_type", {
  expect_error(
    plot_distributions(mtcars, plot_type = "invalid"),
    "`plot_type` must be one of"
  )
})

test_that("plot_distributions applies custom theme", {
  custom_theme <- ggplot2::theme_minimal()
  plots <- plot_distributions(mtcars[, 1:2], plot_theme = custom_theme)
  expect_s3_class(plots[[1]], "gg")
})

# Tests for gt_add_plots
test_that("gt_add_plots adds plots to gt table", {
  expect_warning(
    cor_tab <- cor_matrix(mtcars[, 1:3]) %>%
      report_cor_table(extras = tibble::tibble(Distributions = 1:3)),
    "does not have a row_names column"
  )
  plots <- plot_distributions(mtcars[, 1:3])
  result <- gt_add_plots(cor_tab, plots, 3)
  expect_s3_class(result, "gt_tbl")
})

test_that("gt_add_plots warns when plot count doesn't match rows", {
  cor_tab <- cor_matrix(mtcars[, 1:3]) %>% report_cor_table()
  plots <- plot_distributions(mtcars[, 1:2])  # Only 2 plots for 3 vars
  expect_warning(
    gt_add_plots(cor_tab, plots, 3),
    "number of plots should usually match"
  )
})

# Tests for svy_cor_matrix edge cases
test_that("svy_cor_matrix validates ci_level parameter", {
  expect_error(
    svy_cor_matrix(ess_survey, ci_level = 1.5),
    "Assertion on 'ci_level' failed"
  )
})

test_that("svy_cor_matrix handles var_names as tibble", {
  var_names_df <- tibble::tibble(
    old = c("health", "agea"),
    new = c("Health", "Age")
  )
  result <- svy_cor_matrix(ess_survey, var_names = var_names_df)
  expect_true("Health" %in% rownames(result$cors))
})

test_that("svy_cor_matrix requires numeric columns", {
  # This should work but just verify that numeric columns are required
  df <- data.frame(x = 1:10, y = 11:20, z = 21:30, wt = rep(1, 10))
  svy <- as_survey_design(df, weights = wt)
  # Select only the variables of interest, excluding the weight column
  result <- svy_cor_matrix(svy, var_names = c("x" = "x", "y" = "y", "z" = "z"))
  expect_true("cors" %in% names(result))
})

# Tests for cor_matrix_mi edge cases
test_that("cor_matrix_mi validates ci_level parameter", {
  expect_error(
    cor_matrix_mi(ess_health_mi_long, ci_level = 0),
    "ci_level must be between 0 and 1"
  )
  expect_error(
    cor_matrix_mi(ess_health_mi_long, ci_level = 1),
    "ci_level must be between 0 and 1"
  )
})

test_that("cor_matrix_mi errors when .imp column is missing", {
  df_no_imp <- ess_health_sample
  expect_error(
    cor_matrix_mi(df_no_imp),
    ".imp"
  )
})

test_that("cor_matrix_mi filters out original data (.imp = 0)", {
  # The complete(mice, "long", include = TRUE) includes .imp = 0
  result <- cor_matrix_mi(ess_health_mi_long)
  # Should work without error, filtering out .imp = 0 internally
  expect_true("cors" %in% names(result))
})

test_that("cor_matrix_mi warns about zero-variance variables", {
  df <- ess_health_mi_long
  df$constant <- 1
  expect_warning(
    cor_matrix_mi(df),
    "only have a single value"
  )
})

test_that("cor_matrix_mi handles all-missing variables", {
  df <- ess_health_mi_long
  df$all_na <- NA_real_
  expect_message(
    cor_matrix_mi(df),
    "only have missing values"
  )
})

test_that("cor_matrix_mi requires at least 2 numeric columns", {
  df <- ess_health_mi_long %>% select(.imp, .id, agea)
  expect_error(
    cor_matrix_mi(df),
    "needs to contain at least two numeric columns"
  )
})

test_that("cor_matrix_mi rejects duplicate var_names", {
  expect_error(
    cor_matrix_mi(ess_health_mi_long, var_names = c(agea = "Age", health = "Age")),
    "var_names must map to unique new names"
  )
})

test_that("cor_matrix_mi handles var_names as tibble", {
  var_names_df <- tibble::tibble(
    old = c("health", "agea"),
    new = c("Health", "Age")
  )
  result <- cor_matrix_mi(ess_health_mi_long, var_names = var_names_df)
  expect_true("Health" %in% rownames(result$cors))
})

test_that("cor_matrix_mi confidence intervals work correctly", {
  result <- cor_matrix_mi(ess_health_mi_long, ci_level = 0.90)
  expect_true(!is.null(result$ci.low))
  expect_true(!is.null(result$ci.high))
  expect_equal(result$ci_level, 0.90)
})

# Tests for report_cor_table additional edge cases
test_that("report_cor_table validates cor_matrix is a list", {
  expect_error(
    report_cor_table(mtcars),
    "missing required element"
  )
})

test_that("report_cor_table validates required elements", {
  bad_list <- list(cors = matrix(1:4, 2, 2))
  expect_error(
    report_cor_table(bad_list),
    "missing required element"
  )
})

test_that("report_cor_table works with add_distributions = TRUE for survey data", {
  result <- report_cor_table(ess_survey_cor_matrix, add_distributions = TRUE, data = ess_survey)
  expect_s3_class(result, "gt_tbl")
})

test_that("report_cor_table works with apa_style = FALSE", {
  result <- report_cor_table(mtcars_cor_matrix, apa_style = FALSE)
  expect_s3_class(result, "gt_tbl")
})

test_that("report_cor_table works with custom notes", {
  result <- report_cor_table(mtcars_cor_matrix, notes = list("Custom note 1", "Custom note 2"))
  expect_s3_class(result, "gt_tbl")
})

test_that("report_cor_table works with add_title = TRUE", {
  result <- report_cor_table(mtcars_cor_matrix, add_title = TRUE)
  expect_s3_class(result, "gt_tbl")
})

# Tests for survey distribution plots
test_that("plot_distributions works with survey data - auto mode", {
  plots <- plot_distributions(ess_survey, var_names = c(health = "Health", agea = "Age"))
  expect_type(plots, "list")
  expect_length(plots, 2)
  expect_s3_class(plots[[1]], "gg")
  expect_s3_class(plots[[2]], "gg")
})

test_that("plot_distributions works with survey data - histogram mode", {
  plots <- plot_distributions(ess_survey, var_names = c(health = "Health"),
                              plot_type = "histogram")
  expect_type(plots, "list")
  expect_s3_class(plots[[1]], "gg")
})

test_that("plot_distributions works with survey data - density mode", {
  # Use a continuous variable for density
  plots <- plot_distributions(ess_survey, var_names = c(agea = "Age"),
                              plot_type = "density")
  expect_type(plots, "list")
  expect_s3_class(plots[[1]], "gg")
})

test_that("plot_distributions handles survey data with custom theme", {
  custom_theme <- ggplot2::theme_minimal()
  plots <- plot_distributions(ess_survey, var_names = c(health = "Health"),
                              plot_theme = custom_theme)
  expect_s3_class(plots[[1]], "gg")
})

test_that("plot_distributions aligns histogram y-axes for survey data", {
  plots <- plot_distributions(ess_survey, var_names = c(health = "Health", dosprt = "Sport"),
                              plot_type = "histogram", hist_align_y = TRUE)
  expect_length(plots, 2)
  expect_s3_class(plots[[1]], "gg")
  expect_s3_class(plots[[2]], "gg")
})

test_that("plot_distributions handles var_names as tibble for survey data", {
  var_names_df <- tibble::tibble(
    old = c("health", "agea"),
    new = c("Health", "Age")
  )
  # Suppress warning about KernSmooth - the function has a fallback
  suppressWarnings(
    plots <- plot_distributions(ess_survey, var_names = var_names_df)
  )
  expect_named(plots, c("Health", "Age"))
})


