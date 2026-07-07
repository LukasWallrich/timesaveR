# Regression tests for bugs fixed in R/ source files.
# Each test_that block is self-contained and skips gracefully if a Suggests
# package it needs is not installed.

# --- 1. make_scale(): two_items_reliability default / cronbachs_alpha / cron_alpha ----

test_that("make_scale two_items_reliability default is spearman_brown and cronbachs_alpha/cron_alpha work", {
  df <- data.frame(a = 1:20, b = c(1:19, 25))

  result_default <- make_scale(df, c("a", "b"), "Test",
                                return_list = TRUE, print_hist = FALSE, print_desc = FALSE)
  expect_equal(result_default$descriptives$reliability_method, "spearman_brown")

  result_alpha <- make_scale(df, c("a", "b"), "Test",
                              two_items_reliability = "cronbachs_alpha",
                              return_list = TRUE, print_hist = FALSE, print_desc = FALSE)
  expect_equal(result_alpha$descriptives$reliability_method, "cronbachs_alpha")
  expect_true(is.numeric(result_alpha$descriptives$reliability))
  expect_false(is.na(result_alpha$descriptives$reliability))

  # Legacy alias
  result_legacy <- make_scale(df, c("a", "b"), "Test",
                               two_items_reliability = "cron_alpha",
                               return_list = TRUE, print_hist = FALSE, print_desc = FALSE)
  expect_equal(result_legacy$descriptives$reliability_method, "cronbachs_alpha")
  expect_equal(result_legacy$descriptives$reliability, result_alpha$descriptives$reliability)

  expect_error(
    make_scale(df, c("a", "b"), "Test",
               two_items_reliability = "invalid_choice",
               print_hist = FALSE, print_desc = FALSE)
  )
})

# --- 2. make_scale_mi(scale_items = ) deprecated argument ----

test_that("make_scale_mi supports deprecated scale_items argument", {
  skip_if_not_installed("mice")
  library(mice)

  set.seed(300688)
  dat <- data.frame(
    age = rep(1:2, 13),
    bmi = c(22, 24, NA, 25, 27, 26, 23, NA, 28, 21, 22, 24, 25,
            26, 27, NA, 23, 22, 24, 25, 26, 27, 28, 21, 22, 23),
    hyp = c(1, 1, 2, NA, 1, 2, 1, 1, 2, 1, 1, 2, 1,
            2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2)
  )

  imp <- suppressWarnings(mice(dat, m = 2, maxit = 1, printFlag = FALSE))
  imp_long <- complete(imp, "long", include = TRUE)

  expect_warning(
    result_deprecated <- make_scale_mi(imp_long, scale_items = c("bmi", "hyp"),
                                        scale_name = "test_scale", print_desc = FALSE),
    "deprecated"
  )

  result_current <- make_scale_mi(imp_long, items = c("bmi", "hyp"),
                                   scale_name = "test_scale", print_desc = FALSE)

  expect_equal(result_deprecated$scores, result_current$scores)
  expect_equal(result_deprecated$descriptives$reliability, result_current$descriptives$reliability)
})

# --- 3. pool_estimates() uses correct t critical value ----

test_that("pool_estimates uses t critical value (qt), not normal z", {
  res <- timesaveR:::pool_estimates(list(qbar = 0.8, df = 20, t = 0.01), ci = .95)

  crit <- stats::qt(.975, 20)
  expect_equal(unname(crit), 2.085963, tolerance = 1e-5)

  expected_lwr <- 0.8 - crit * sqrt(0.01)
  expected_upr <- 0.8 + crit * sqrt(0.01)

  expect_equal(unname(res["lwr"]), expected_lwr, tolerance = 1e-8)
  expect_equal(unname(res["upr"]), expected_upr, tolerance = 1e-8)
})

# --- 4. svy_make_scale(): proration_cutoff works ----

test_that("svy_make_scale proration_cutoff = 0 sets rows with any missing item to NA", {
  skip_if_not_installed("survey")
  library(survey)

  data(api)
  svy_data <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
                        data = apistrat, fpc = ~fpc)

  # Introduce missingness in one item, for a known set of rows
  na_rows <- c(2, 5, 9)
  svy_data$variables$meals[na_rows] <- NA

  result <- svy_make_scale(svy_data, c("ell", "meals"), "test_scale",
                            proration_cutoff = 0, print_hist = FALSE, print_desc = FALSE)

  scale_vals <- result$variables$test_scale

  expect_true(all(is.na(scale_vals[na_rows])))
  # Rows without any missing item data should still have a valid score
  other_rows <- setdiff(seq_along(scale_vals), na_rows)
  non_na_item_rows <- other_rows[!is.na(svy_data$variables$ell[other_rows]) &
                                   !is.na(svy_data$variables$meals[other_rows])]
  expect_true(all(!is.na(scale_vals[non_na_item_rows])))
})

# --- 5. pairwise_t_tests(): outcome column named "x"; var_equal pooled-SD Cohen's d ----

test_that("pairwise_t_tests works when the outcome column is literally named 'x'", {
  set.seed(1)
  df <- data.frame(
    x = c(rnorm(15, 0, 1), rnorm(15, 1, 1)),
    grp = rep(c("A", "B"), each = 15)
  )

  result <- pairwise_t_tests(df, x, grp)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_true(is.numeric(result$cohens_d))
})

test_that("pairwise_t_tests var_equal = TRUE uses pooled-SD Cohen's d, different from var_equal = FALSE for unbalanced heteroscedastic data", {
  set.seed(42)
  df <- data.frame(
    outcome = c(rnorm(10, 0, 1), rnorm(40, 0, 5)),
    grp = rep(c("A", "B"), c(10, 40))
  )

  res_unequal <- pairwise_t_tests(df, outcome, grp, var_equal = FALSE)
  res_equal <- pairwise_t_tests(df, outcome, grp, var_equal = TRUE)

  expect_false(isTRUE(all.equal(res_unequal$cohens_d, res_equal$cohens_d)))

  # Manually compute pooled-SD Cohen's d to confirm var_equal = TRUE result
  n1 <- 10; n2 <- 40
  m1 <- mean(df$outcome[df$grp == "A"]); m2 <- mean(df$outcome[df$grp == "B"])
  v1 <- var(df$outcome[df$grp == "A"]); v2 <- var(df$outcome[df$grp == "B"])
  pooled_sd <- sqrt(((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2))
  expected_d <- (m1 - m2) / pooled_sd

  expect_equal(res_equal$cohens_d, expected_d, tolerance = 1e-8)
})

# --- 6. t_test(x, paired = TRUE) without y errors informatively ----

test_that("t_test errors informatively when paired = TRUE but y is missing", {
  expect_error(
    t_test(mtcars$mpg, paired = TRUE),
    "paired"
  )
})

# --- 7. report_lm_with_std(..., R2_change = TRUE) ----

test_that("report_lm_with_std works with R2_change = TRUE for a pair of lm models", {
  skip_if_not_installed("gt")
  skip_if_not_installed("modelsummary")
  skip_if_not_installed("htmltools")

  mod1 <- lm(mpg ~ hp, mtcars)
  mod2 <- lm(mpg ~ hp + wt, mtcars)
  mod1_std <- lm_std(mpg ~ hp, mtcars)
  mod2_std <- lm_std(mpg ~ hp + wt, mtcars)

  result <- suppressWarnings(
    report_lm_with_std(list(mod1, mod2), list(mod1_std, mod2_std), R2_change = TRUE)
  )

  expect_true("html_code" %in% names(result))
  expect_true(grepl("&Delta;", result$html_code, fixed = TRUE))
})

test_that("report_lm_with_std errors informatively for mira objects with R2_change = TRUE", {
  skip_if_not_installed("mice")
  library(mice)

  set.seed(300688)
  imp <- suppressWarnings(mice(nhanes, m = 2, maxit = 1, printFlag = FALSE))
  mod1 <- with(imp, lm(bmi ~ age))
  mod2 <- suppressWarnings(with(imp, lm(bmi ~ age + hyp)))

  expect_error(
    report_lm_with_std(mod1, mod2, R2_change = TRUE),
    "mira"
  )
})

# --- 8. cor_matrix() does not perturb RNG state when seed is NULL ----

test_that("cor_matrix does not alter RNG state when seed = NULL", {
  set.seed(123)
  before <- .Random.seed
  invisible(cor_matrix(mtcars[, 1:3]))
  after <- .Random.seed
  expect_identical(before, after)
})

# --- 9. pcor_matrix() with tibble var_names ----

test_that("pcor_matrix works with a tibble var_names", {
  var_names_df <- tibble::tibble(
    old = c("mpg", "cyl"),
    new = c("MPG", "Cylinders")
  )

  result <- pcor_matrix(mtcars, given = "wt", var_names = var_names_df)
  expect_true(all(c("MPG", "Cylinders") %in% rownames(result$cors)))
})

# --- 10. svy_cor_matrix() with a variable name containing "_1" ----

test_that("svy_cor_matrix preserves variable names containing '_1'", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")
  skip_if_not_installed("jtools")
  skip_if_not_installed("weights")
  library(survey)
  library(srvyr)

  data(api)
  svy_data <- apistrat %>% as_survey_design(strata = stype, weights = pw, fpc = fpc)
  names(svy_data$variables)[names(svy_data$variables) == "api00"] <- "api_1"

  result <- svy_cor_matrix(svy_data, var_names = c(api_1 = "API", meals = "Meals"))

  expect_true("API" %in% rownames(result$cors))
  expect_false(any(grepl("_.1", rownames(result$cors), fixed = TRUE)))
})

# --- 11. tidy.svy_cor_matrix() returns correct columns/values ----

test_that("tidy.svy_cor_matrix returns estimate/statistic/std.error/p.value/conf.low/conf.high with correct values", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")
  skip_if_not_installed("jtools")
  skip_if_not_installed("weights")
  library(survey)
  library(srvyr)

  data(api)
  svy_data <- apistrat %>% as_survey_design(strata = stype, weights = pw, fpc = fpc)

  cm <- svy_cor_matrix(svy_data, var_names = c(api00 = "API", meals = "Meals", ell = "ELL"))
  tidied <- tidy(cm, both_directions = FALSE)

  expect_true(all(c("estimate", "statistic", "std.error", "p.value", "conf.low", "conf.high") %in% names(tidied)))

  row <- tidied[tidied$column1 == "Meals" & tidied$column2 == "API", ]
  expect_equal(row$estimate, cm$cors["API", "Meals"], tolerance = 1e-8)
  expect_equal(row$statistic, cm$t.values["API", "Meals"], tolerance = 1e-8)
  expect_equal(row$std.error, cm$std.err["API", "Meals"], tolerance = 1e-8)
  expect_equal(row$p.value, cm$p.values["API", "Meals"], tolerance = 1e-8)
  expect_equal(row$conf.low, cm$ci.low["API", "Meals"], tolerance = 1e-8)
  expect_equal(row$conf.high, cm$ci.high["API", "Meals"], tolerance = 1e-8)
})

# --- 12. sigstars() NA padding with pad_html = TRUE ----

test_that("sigstars pads NA position to same rendered width as other results with pad_html = TRUE", {
  skip_if_not_installed("xml2")

  result <- sigstars(c(0.01, NA, 0.5), pad_html = TRUE)
  rendered_width <- function(x) nchar(xml2::xml_text(xml2::read_html(paste0("<x>", x, "</x>"))))

  widths <- vapply(result, rendered_width, integer(1))

  expect_equal(unname(widths[2]), unname(widths[1]))
  expect_equal(unname(widths[2]), unname(widths[3]))
  expect_false(result[2] == "")
})

# --- 13. .make_stars_note() does not strip first character of "1" ----

test_that(".make_stars_note does not strip the first character of a threshold of 1", {
  result <- timesaveR:::.make_stars_note(stars = c(`ns` = 1, `*` = 0.05))
  expect_true(grepl("ns \\*p\\* < 1", result))
})
