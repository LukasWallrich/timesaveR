test_that("scale works", {
  expect_equal(make_scale(iris, c("Sepal.Length", "Petal.Length"), "Test", return_list = TRUE) %>% {
    paste(round(.$descriptives$reliability, 2), .$scores[[1]])
  }, "0.93 3.25")
  expect_equal(make_scale(iris, c("Sepal.Length", "Petal.Length", "Sepal.Width"), "Test", reverse = "spec", reverse_items = "Sepal.Width", return_list = TRUE) %>% {
    paste(round(.$descriptives$reliability, 2), round(.$scores[[1]], 2))
  }, "0.73 3.97")
})

scales_items <- list(
  depression = c("fltdpr", "flteeff", "slprl", "wrhpp", "fltlnl", 
                 "enjlf", "fltsd", "cldgng"),
  healthy_eating = c("etfruit", "eatveg")
)

scales_reverse <- list(
  depression = c("wrhpp", "enjlf"),
  healthy_eating = c("etfruit", "eatveg")
)

scales <- make_scales(ess_health, items = scales_items, reversed = scales_reverse)

scales_desc <- scales$descriptives %>% round_df()

test_that("scales works", {
  expect_equal(scales_desc$reliability, c(.8, .66))
  expect_equal(scales$scores$depression[1], 1.25)
})

# Create Dataset with missing data
library(mice)
library(dplyr)
set.seed(300688)
ess_health <- timesaveR::ess_health %>% sample_n(100) %>% dplyr::select(cgtsmke, dosprt, health)
add_missing <- function(x) {x[!rbinom(length(x), 1, .9)] <- NA; x}
ess_health <- ess_health %>% mutate(across(everything(), add_missing))

# Impute data
ess_health_mi <- mice(ess_health, printFlag = FALSE) 
ess_health_mi <- complete(ess_health_mi, "long", include = TRUE)

scale1 <- make_scale_mi(ess_health_mi, c("cgtsmke", "dosprt", "health"), "healthy", print_desc = FALSE)

scale2 <- make_scale_mi(ess_health_mi, c("cgtsmke", "dosprt", "health"), "healthy", boot = 100, print_desc = FALSE, parallel = FALSE, alpha_ci = .9)

test_that("make_scale_mi works and returns expected structure", {
  # Test basic functionality without exact value matching (due to seed issues on GH)
  expect_type(scale1, "list")
  expect_true("scores" %in% names(scale1))
  expect_true("descriptives" %in% names(scale1))

  # Check descriptives structure
  desc <- scale1$descriptives
  expect_true("reliability" %in% names(desc))
  expect_true("mean" %in% names(desc))
  expect_true("SD" %in% names(desc))
  expect_true(is.numeric(desc$reliability))
  expect_true(is.numeric(desc$mean))

  # Check that reliability is within plausible bounds
  expect_true(desc$reliability >= -1 && desc$reliability <= 1)
})

test_that("make_scale_mi works with alpha_ci", {
  # Check that CI is calculated when requested
  desc2 <- scale2$descriptives
  expect_true("reliability_ci_lower" %in% names(desc2))
  expect_true("reliability_ci_upper" %in% names(desc2))
  expect_true(is.numeric(desc2$reliability_ci_lower))
  expect_true(is.numeric(desc2$reliability_ci_upper))

  # CI bounds should bracket the estimate
  expect_true(desc2$reliability_ci_lower <= desc2$reliability)
  expect_true(desc2$reliability_ci_upper >= desc2$reliability)
})

# Test error handling ----
test_that("make_scale handles missing items error", {
  expect_error(
    make_scale(iris, c("Sepal.Length", "NonExistent"), "Test"),
    "Not all items can be found"
  )
})

test_that("make_scale handles all NA items error", {
  df <- data.frame(a = NA_real_, b = NA_real_)
  expect_error(
    make_scale(df, c("a", "b"), "Test"),
    "only contain missing values"
  )
})

test_that("make_scale handles single item error", {
  expect_error(
    make_scale(iris, c("Sepal.Length"), "Test"),
    "at least two items"
  )
})

test_that("make_scale warns when some items have zero variance but >=2 have variance", {
  # Item 'a' has zero variance, but 'b' and 'c' have variance
  df <- data.frame(a = rep(1, 20), b = 1:20, c = 11:30)

  expect_warning(
    make_scale(df, c("a", "b", "c"), "Test", print_hist = FALSE, print_desc = FALSE),
    "zero variance.*excluded from reliability"
  )
})

test_that("make_scale errors when fewer than 2 items have variance", {
  # Only item 'b' has variance
  df <- data.frame(a = rep(1, 20), b = 1:20)

  expect_error(
    make_scale(df, c("a", "b"), "Test", print_hist = FALSE, print_desc = FALSE),
    "fewer than 2 items with variance"
  )
})

test_that("make_scale errors when all items have zero variance", {
  # All items have zero variance
  df <- data.frame(a = rep(1, 20), b = rep(2, 20))

  expect_error(
    make_scale(df, c("a", "b"), "Test", print_hist = FALSE, print_desc = FALSE),
    "fewer than 2 items with variance"
  )
})

test_that("make_scale fails with conflicting reverse instructions", {
  expect_error(
    make_scale(iris, c("Sepal.Length", "Petal.Length"), "Test",
               reverse_method = "auto", reverse_items = "Petal.Length"),
    'reverse_items.*should only be specified.*reverse_method.*spec'
  )
})

# Test proration cutoff ----
test_that("make_scale handles proration cutoff correctly", {
  df <- data.frame(
    a = c(1, 2, NA, 4, 5),
    b = c(2, 3, 4, NA, 6),
    c = c(3, 4, 5, 6, NA)
  )

  # With 0.33 cutoff, case 1 should have score (all items present)
  result_strict <- make_scale(df, c("a", "b", "c"), "Test",
                              proration_cutoff = 0.33,
                              print_hist = FALSE, print_desc = FALSE)
  expect_true(is.na(result_strict[3])) # 1 of 3 missing > 33%

  # With 0.4 cutoff, more lenient
  result_lenient <- make_scale(df, c("a", "b", "c"), "Test",
                               proration_cutoff = 0.4,
                               print_hist = FALSE, print_desc = FALSE)
  expect_false(is.na(result_lenient[3])) # 1 of 3 = 33% < 40%
})

# Test harmonize_ranges ----
test_that("make_scale harmonizes ranges when requested", {
  df <- data.frame(
    a = c(1, 2, 3, 4, 5),
    b = c(10, 20, 30, 40, 50)
  )

  expect_message(
    result <- make_scale(df, c("a", "b"), "Test", harmonize_ranges = TRUE,
                        print_hist = FALSE, print_desc = FALSE),
    "Not all items have the same range.*rescaled"
  )

  # Should not harmonize if FALSE
  suppressMessages(
    result2 <- make_scale(df, c("a", "b"), "Test", harmonize_ranges = FALSE,
                         print_hist = FALSE, print_desc = FALSE)
  )
})

test_that("make_scale harmonizes to specified range", {
  df <- data.frame(
    a = c(1, 2, 3, 4, 5),
    b = c(10, 20, 30, 40, 50)
  )

  expect_message(
    result <- make_scale(df, c("a", "b"), "Test", harmonize_ranges = c(0, 10),
                        print_hist = FALSE, print_desc = FALSE),
    "rescaled to the range from 0 to 10"
  )
})

test_that("make_scale errors on invalid harmonize_ranges", {
  df <- data.frame(
    a = c(1, 2, 3, 4, 5),
    b = c(10, 20, 30, 40, 50)
  )

  expect_error(
    make_scale(df, c("a", "b"), "Test", harmonize_ranges = c(1, 2, 3),
               print_hist = FALSE, print_desc = FALSE),
    "exactly two items"
  )
})

# Test two_items_reliability methods ----
test_that("make_scale uses different two-item reliability methods", {
  result_sb <- make_scale(iris, c("Sepal.Length", "Petal.Length"), "Test",
                         two_items_reliability = "spearman_brown",
                         return_list = TRUE, print_hist = FALSE, print_desc = FALSE)
  expect_equal(result_sb$descriptives$reliability_method, "spearman_brown")

  result_alpha <- make_scale(iris, c("Sepal.Length", "Petal.Length"), "Test",
                            two_items_reliability = "cronbachs_alpha",
                            return_list = TRUE, print_hist = FALSE, print_desc = FALSE)
  expect_equal(result_alpha$descriptives$reliability_method, "cronbachs_alpha")

  result_r <- make_scale(iris, c("Sepal.Length", "Petal.Length"), "Test",
                        two_items_reliability = "r",
                        return_list = TRUE, print_hist = FALSE, print_desc = FALSE)
  expect_equal(result_r$descriptives$reliability_method, "r")

  # These should give different values
  expect_false(result_sb$descriptives$reliability == result_r$descriptives$reliability)
})

# Test r_key (reverse entire scale) ----
test_that("make_scale reverses entire scale with r_key", {
  result_normal <- make_scale(iris, c("Sepal.Length", "Petal.Length"), "Test",
                             print_hist = FALSE, print_desc = FALSE)

  result_reversed <- make_scale(iris, c("Sepal.Length", "Petal.Length"), "Test",
                               r_key = -1, print_hist = FALSE, print_desc = FALSE)

  # Reversed should be different and inversely related
  expect_true(result_reversed[1] != result_normal[1])
  # Check that the correlation is negative
  expect_true(cor(result_normal, result_reversed, use = "complete.obs") < 0)

  # Test with specific max
  result_rev_max <- make_scale(iris, c("Sepal.Length", "Petal.Length"), "Test",
                               r_key = 10, print_hist = FALSE, print_desc = FALSE)
  expect_true(result_rev_max[1] != result_normal[1])
})

# Test automatic reverse coding ----
test_that("make_scale detects items needing reversal automatically", {
  # Create data where one item correlates negatively
  df <- data.frame(
    a = 1:10,
    b = 1:10,
    c = 10:1  # Reversed
  )

  expect_output(
    result <- make_scale(df, c("a", "b", "c"), "Test",
                        reverse_method = "auto",
                        print_hist = FALSE, print_desc = FALSE,
                        return_list = TRUE),
    NA  # psych::alpha may print warnings about reverse coding
  )

  # Check that item was reversed in descriptives
  expect_true(result$descriptives$reversed != "")
})

# Test spearman_brown function ----
test_that("spearman_brown calculates correctly", {
  result_num <- spearman_brown(iris, c("Sepal.Length", "Petal.Length"), SB_only = TRUE)
  expect_true(is.numeric(result_num))
  expect_true(result_num >= 0 && result_num <= 1)

  result_df <- spearman_brown(iris, c("Sepal.Length", "Petal.Length"),
                             name = "Test", SB_only = FALSE)
  expect_true(is.data.frame(result_df))
  expect_true(all(c("correlation", "spearman_brown") %in% names(result_df)))
})

# Test make_scales edge cases ----
test_that("make_scales handles missing items error", {
  items <- list(scale1 = c("a", "missing"))
  expect_error(
    make_scales(iris, items),
    "Not all items can be found"
  )
})

test_that("make_scales works with partial reverse lists", {
  items <- list(
    scale1 = c("Sepal.Length", "Petal.Length"),
    scale2 = c("Sepal.Width", "Petal.Width")
  )

  reversed <- list(
    scale1 = c("Petal.Length")
  )

  suppressMessages({
    result <- make_scales(iris, items, reversed = reversed, print_desc = FALSE, print_hist = FALSE)
  })

  expect_true(all(c("scale1", "scale2") %in% names(result$scores)))
  expect_equal(nrow(result$descriptives), 2)
})

test_that("make_scales works with all scales without reversing", {
  items <- list(
    scale1 = c("Sepal.Length", "Petal.Length"),
    scale2 = c("Sepal.Width", "Petal.Width")
  )

  suppressMessages({
    result <- make_scales(iris, items, reversed = FALSE, print_desc = FALSE, print_hist = FALSE)
  })

  expect_equal(ncol(result$scores), 2)
  expect_equal(nrow(result$descriptives), 2)
  expect_true(is.list(result))
  expect_true(all(c("scores", "descriptives") %in% names(result)))
  expect_s3_class(result$scores, "tbl_df")
  expect_s3_class(result$descriptives, "tbl_df")
  expect_true("Scale" %in% names(result$descriptives))
})

# Test print options ----
test_that("make_scale respects print options", {
  # Should not print when print_desc = FALSE and print_hist = FALSE
  # May still show message about ranges - suppress that
  suppressMessages(
    result <- make_scale(iris, c("Sepal.Length", "Petal.Length"), "Test",
              print_desc = FALSE, print_hist = FALSE, harmonize_ranges = FALSE)
  )
  expect_true(is.numeric(result))
})

test_that("make_scale returns correct structure based on return_list", {
  # return_list = FALSE should return vector
  result_vector <- make_scale(iris, c("Sepal.Length", "Petal.Length"), "Test",
                             return_list = FALSE, print_hist = FALSE, print_desc = FALSE)
  expect_true(is.numeric(result_vector))
  expect_equal(length(result_vector), nrow(iris))

  # return_list = TRUE should return list with scores and descriptives
  result_list <- make_scale(iris, c("Sepal.Length", "Petal.Length"), "Test",
                           return_list = TRUE, print_hist = FALSE, print_desc = FALSE)
  expect_true(is.list(result_list))
  expect_true(all(c("scores", "descriptives") %in% names(result_list)))
  expect_true(all(c("n_items", "reliability", "mean", "SD") %in%
                   names(result_list$descriptives)))
})
