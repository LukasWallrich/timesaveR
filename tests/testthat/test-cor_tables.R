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

# Perform FIML correlation matrix
fiml_cor_matrix <- cor_matrix(ess_health_sample, missing = "fiml")


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
  
  expect_snapshot({
    fiml_boot_cor_matrix <- cor_matrix(
      dplyr::select(ess_health_sample, -pspwght),
      missing = "fiml",
      bootstrap = 100,
      seed = 12345
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

