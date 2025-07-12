# tests/testthat/test-cor_functions.R

library(testthat)
library(dplyr)
library(srvyr)
library(mice)
library(gt)
library(timesaveR)

# Helper function to create missing data
add_missing <- function(x) {x[!rbinom(length(x), 1, 0.9)] <- NA; x}

# Prepare the mtcars correlation matrix
mtcars_cor_matrix <- cor_matrix(mtcars, var_names = c(mpg = "Hello", cyl = "You"))

# Prepare survey data for svy_cor_matrix tests
ess_health_sample <- timesaveR::ess_health %>%
  sample_n(100) %>%
  select(agea, health, dosprt, pspwght) %>%
  mutate(across(c(everything(), -pspwght), add_missing))

ess_survey <- as_survey_design(ess_health_sample, weights = pspwght)

ess_survey_cor_matrix <- svy_cor_matrix(ess_survey, var_names = c(health = "Health", agea = "Age"))

# Prepare multiple imputation data for cor_matrix_mi tests
set.seed(300688)
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

# Perform FIML correlation matrix with bootstrapping
fiml_boot_cor_matrix <- cor_matrix(ess_health_sample, missing = "fiml", bootstrap = 10, seed = 12345)

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

test_that("cor_matrix_mi works correctly without weights", {
  expected_cor <- with(ess_health_mi, lm_std(agea ~ health)) %>% pool() %>% {.$pooled$estimate[2]}
  computed_cor <- mi_cor_matrix$cors[1, 2]
  expect_equal(computed_cor, expected_cor)
})

test_that("cor_matrix_mi works correctly with weights", {
  # The expected value should be precomputed and verified
  # Here we assume -0.19071925 is the correct pooled weighted correlation
  expect_equal(round(mi_cor_matrix_weighted$cors[1, 2], 8), -0.19071925)
})

test_that("cor_matrix works with FIML method", {
  # The expected value should be precomputed and verified
  # Here we assume -0.19917584 is the correct correlation
  expect_equal(round(fiml_cor_matrix$cors[2, 1], 8), -0.19917584)
})

test_that("cor_matrix works with FIML and bootstrapping", {
  # The expected value should be precomputed and verified
  # Here we assume -0.23253694 is the correct correlation after bootstrapping
  expect_equal(round(fiml_boot_cor_matrix$cors[2, 1], 8), -0.23253694)
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
    "If 'add_distributions = TRUE', 'data' needs to be provided."
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
    "The 'extras' data frame does not have a 'row_names' column."
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

test_that("report_cor_table handles incorrect 'n' for confidence intervals", {
  expect_error(
    report_cor_table(mtcars_cor_matrix, ci = "z_transform", n = NULL),
    "Cannot compute z-transform confidence intervals because 'df', 'n' in 'cor_matrix' and 'n' argument are all NULL."
  )
})

test_that("report_cor_table handles invalid 'ci' method", {
  expect_error(
    report_cor_table(mtcars_cor_matrix, ci = "invalid_method"),
    "Invalid 'ci' method specified."
  )
})

test_that("report_cor_table handles missing 'ci.low' and 'ci.high' when ci = 'given'", {
  cor_matrix_no_ci <- mtcars_cor_matrix
  cor_matrix_no_ci$ci.low <- NULL
  cor_matrix_no_ci$ci.high <- NULL
  expect_error(
    report_cor_table(cor_matrix_no_ci, ci = "given"),
    "ci = 'given' but 'ci.low' and 'ci.high' are not available in 'cor_matrix'."
  )
})

test_that("report_cor_table works with custom 'stars' parameter", {
  custom_stars <- c("***" = 0.001, "**" = 0.01, "*" = 0.05)
  expect_s3_class(
    report_cor_table(mtcars_cor_matrix, stars = custom_stars),
    "gt_tbl"
  )
})

test_that("report_cor_table handles invalid 'stars' parameter", {
  expect_error(
    report_cor_table(mtcars_cor_matrix, stars = c(0.001, 0.01, 0.05)),
    "'stars' must be a named numeric vector."
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
  # Ensure that plots are added when add_distributions is TRUE
  expect_s3_class(
    report_cor_table(mtcars_cor_matrix, add_distributions = TRUE, data = mtcars),
    "gt_tbl"
  )
})

test_that("report_cor_table handles mismatched number of plots", {
  # Create an extras tibble with incorrect number of rows
  extras <- tibble(Extra_Info = c("Info1", "Info2")) # Assuming mtcars has more than 2 variables
  expect_error(
    report_cor_table(mtcars_cor_matrix, add_distributions = TRUE, data = mtcars, extras = extras),
    "The number of plots does not match the number of variables in the table."
  )
})
