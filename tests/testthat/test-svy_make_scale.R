library(survey)

# Skip all tests if survey package is not available
skip_if_not_installed("survey")

# Create test survey data
data(api)
svy_data <- svydesign(id = ~1, strata = ~stype, weights = ~pw, 
                     data = apistrat, fpc = ~fpc)

test_that("svy_make_scale works with basic functionality", {
  scale_items <- c("ell", "meals")
  result <- svy_make_scale(svy_data, scale_items, "test_scale", print_desc = FALSE, print_hist = FALSE)
  
  expect_true("test_scale" %in% names(result$variables))
  expect_true(is.numeric(result$variables$test_scale))
})

test_that("svy_make_scale works with reverse coding", {
  scale_items <- c("ell", "meals", "mobility")
  reverse_items <- c("ell")
  
  result <- svy_make_scale(svy_data, scale_items, "test_scale_rev", 
                          reverse = "spec", reverse_items = reverse_items,
                          print_desc = FALSE, print_hist = FALSE)
  
  expect_true("test_scale_rev" %in% names(result$variables))
})

test_that("svy_make_scale handles deprecated 'reversed' parameter", {
  scale_items <- c("ell", "meals")
  
  expect_warning(
    result <- svy_make_scale(svy_data, scale_items, "test_scale_dep", 
                            reversed = c("ell"), print_desc = FALSE, print_hist = FALSE),
    "deprecated as of timesaveR"
  )
  
  expect_true("test_scale_dep" %in% names(result$variables))
})

test_that("svy_make_scale validates input parameters", {
  expect_error(
    svy_make_scale(svy_data, c("nonexistent"), "test"),
    "Not all items can be found in the survey data"
  )
  
  expect_error(
    svy_make_scale(svy_data, c("ell"), "test"),
    "must contain at least two variables"
  )
  
  expect_error(
    svy_make_scale(svy_data, c("ell", "meals"), "test", reverse = "auto"),
    "isn't supported"
  )
})

test_that("svy_make_scale parameter alignment with make_scale", {
  # Test that new parameters are accepted without error
  scale_items <- c("ell", "meals")
  
  result <- svy_make_scale(svy_data, scale_items, "aligned_scale",
                          reverse = "none",
                          two_items_reliability = "spearman_brown", 
                          proration_cutoff = 0.4,
                          harmonize_ranges = NULL,
                          return_list = TRUE,
                          print_desc = FALSE, print_hist = FALSE)
  
  expect_true("aligned_scale" %in% names(result$survey_data$variables))
})
