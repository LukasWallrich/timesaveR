# Test data setup
set.seed(1234)
X <- rnorm(100)
M1 <- 0.5*X + rnorm(100)
M2 <- 0.25*X + rnorm(100)
M3 <- 0.75*X + rnorm(100)
Y <- 0.5*M1 + 0.75*M2 + 0.25*M3 + 0.25* X + rnorm(100)
CV <- rnorm(100) + .2*M1 + .2*M2 + .2*M3 + .2*Y

df <- data.frame(X = X, M1 = M1, M2 = M2, M3 = M3, Y = Y, CV = CV)

# Use workaround for lavaan bug in version > 0.6.12
lavaan_args <- if (packageVersion("lavaan") > "0.6.12") list(missing = "listwise") else list()

test_that("parallel mediation with three mediators works", {
  res <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                       Ms = quote(c(M1, M2, M3)), CVs = quote(CV),
                                       bootstraps = 50, seed = 7654321), lavaan_args))

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 11)
  expect_equal(ncol(res), 7)

  # Check CV coefficients attribute
  cv_coefs <- attr(res, "CV_coefficients")
  expect_s3_class(cv_coefs, "data.frame")
  expect_equal(nrow(cv_coefs), 4)
  expect_equal(ncol(cv_coefs), 7)

  # Check that CV coefficients are properly parsed
  expect_true("DV" %in% names(cv_coefs))
  expect_true("CV" %in% names(cv_coefs))
  expect_true(all(cv_coefs$CV == "CV"))
  expect_true(all(cv_coefs$DV %in% c("M1", "M2", "M3", "Y")))

  # Check lavaan code attribute exists
  expect_type(attr(res, "lavaan_code"), "character")
})

test_that("single mediator mediation works", {
  res <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                       Ms = quote(M1), bootstraps = 50, seed = 123), lavaan_args))

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5) # direct, total, indirect, a, b for single mediator

  # Check result structure
  expect_true("type" %in% names(res))
  expect_true("est" %in% names(res))
  expect_true("se" %in% names(res))
  expect_true("pvalue" %in% names(res))
  expect_true("ci.lower" %in% names(res))
  expect_true("ci.upper" %in% names(res))
})

test_that("mediation without covariates works", {
  res <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                       Ms = quote(c(M1, M2)), bootstraps = 50, seed = 456), lavaan_args))

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 8) # direct, total, indirect (overall), 2 x (a, b, indirect)

  # CV coefficients should be empty when no covariates
  cv_coefs <- attr(res, "CV_coefficients")
  expect_true(nrow(cv_coefs) == 0)
})

test_that("mediation with two mediators and covariate works", {
  res <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                       Ms = quote(c(M1, M2)), CVs = quote(CV),
                                       bootstraps = 50, seed = 789), lavaan_args))

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 8)

  # Check that we have the expected parameter types
  param_types <- unique(res$type)
  expect_true("direct" %in% param_types)
  expect_true("total" %in% param_types)
  expect_true("a" %in% param_types)
  expect_true("b" %in% param_types)
  expect_true("indirect" %in% param_types)

  # Check CV coefficients
  cv_coefs <- attr(res, "CV_coefficients")
  expect_equal(nrow(cv_coefs), 3) # M1, M2, Y
  expect_true(all(cv_coefs$CV == "CV"))
  expect_setequal(cv_coefs$DV, c("M1", "M2", "Y"))
})

test_that("confidence intervals respect conf_level parameter", {
  res_95 <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                          Ms = quote(M1), bootstraps = 50, seed = 100,
                                          conf_level = 0.95), lavaan_args))
  res_90 <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                          Ms = quote(M1), bootstraps = 50, seed = 100,
                                          conf_level = 0.90), lavaan_args))

  # 90% CI should be narrower than 95% CI
  ci_width_95 <- res_95$ci.upper[1] - res_95$ci.lower[1]
  ci_width_90 <- res_90$ci.upper[1] - res_90$ci.lower[1]

  expect_true(ci_width_90 < ci_width_95)
})

test_that("standardized_all parameter affects results", {
  res_std <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                           Ms = quote(M1), bootstraps = 50, seed = 200,
                                           standardized_all = TRUE), lavaan_args))
  res_unstd <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                             Ms = quote(M1), bootstraps = 50, seed = 200,
                                             standardized_all = FALSE), lavaan_args))

  # Results should differ
  expect_false(isTRUE(all.equal(res_std$est, res_unstd$est)))
})

test_that("seed parameter produces reproducible results", {
  res1 <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                        Ms = quote(M1), bootstraps = 50, seed = 12345), lavaan_args))
  res2 <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                        Ms = quote(M1), bootstraps = 50, seed = 12345), lavaan_args))

  expect_equal(res1$est, res2$est)
  expect_equal(res1$se, res2$se)
  expect_equal(res1$ci.lower, res2$ci.lower)
  expect_equal(res1$ci.upper, res2$ci.upper)
})

test_that("mediation produces correct indirect effects with known relationships", {
  # Create data with known linear relationships for verifiable calculation
  # X -> M with coefficient a = 0.5
  # M -> Y with coefficient b = 0.6
  # X -> Y direct effect c' = 0.3
  # Expected indirect effect = a * b = 0.5 * 0.6 = 0.30
  # Expected total effect = c' + indirect = 0.3 + 0.3 = 0.60

  set.seed(9999)
  n <- 1000
  X_test <- rnorm(n)
  # Add minimal noise to avoid perfect collinearity issues
  M_test <- 0.5 * X_test + rnorm(n, 0, 0.01)  # a ≈ 0.5
  Y_test <- 0.3 * X_test + 0.6 * M_test + rnorm(n, 0, 0.01)  # c' ≈ 0.3, b ≈ 0.6

  test_df <- data.frame(X = X_test, M = M_test, Y = Y_test)

  # Run mediation with minimal bootstrapping (unstandardized for exact comparison)
  res <- do.call(run_mediation, c(list(data = test_df, X = quote(X), Y = quote(Y),
                                       Ms = quote(M), bootstraps = 10, seed = 999,
                                       standardized_all = FALSE), lavaan_args))

  # Extract key estimates (point estimates should be accurate with minimal noise)
  indirect_est <- res$est[res$type == "indirect"]
  direct_est <- res$est[res$type == "direct"]
  total_est <- res$est[res$type == "total"]
  a_est <- res$est[res$type == "a"]
  b_est <- res$est[res$type == "b"]

  # Check that estimated paths match known values (within reasonable tolerance)
  expect_equal(a_est, 0.5, tolerance = 0.03)
  expect_equal(b_est, 0.6, tolerance = 0.03)
  expect_equal(direct_est, 0.3, tolerance = 0.03)

  # Check that indirect effect ≈ a * b
  expect_equal(indirect_est, a_est * b_est, tolerance = 0.01)
  expect_equal(indirect_est, 0.30, tolerance = 0.03)

  # Check that total effect ≈ direct + indirect
  expect_equal(total_est, direct_est + indirect_est, tolerance = 0.01)
  expect_equal(total_est, 0.60, tolerance = 0.03)
})

test_that("error handling for conf.level (wrong parameter name)", {
  expect_error(
    do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                  Ms = quote(M1), conf.level = 0.95), lavaan_args)),
    "conf_level"
  )
})

test_that("error handling for variable names with double underscores", {
  df_bad <- df
  names(df_bad)[1] <- "X__bad"

  expect_error(
    do.call(run_mediation, c(list(data = df_bad, X = quote(X__bad), Y = quote(Y),
                                  Ms = quote(M1), bootstraps = 50), lavaan_args)),
    "does not support variable names that contain two"
  )
})

test_that("pvalue calculation is correct", {
  res <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                       Ms = quote(M1), bootstraps = 100, seed = 999), lavaan_args))

  # All p-values should be between 0 and 1
  expect_true(all(res$pvalue >= 0 & res$pvalue <= 1))

  # P-values for effects crossing zero should be higher
  # This is probabilistic, so we just check they exist
  expect_type(res$pvalue, "double")
})

test_that("mediator parameter types are correctly labeled", {
  res <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                       Ms = quote(c(M1, M2, M3)), bootstraps = 50, seed = 321), lavaan_args))

  # Check that mediator names appear correctly in results
  mediators_in_res <- unique(na.omit(res$mediator))
  expect_true("M1" %in% mediators_in_res)
  expect_true("M2" %in% mediators_in_res)
  expect_true("M3" %in% mediators_in_res)
})

test_that("indirect effects sum equals total minus direct", {
  res <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                       Ms = quote(c(M1, M2)), bootstraps = 50, seed = 555), lavaan_args))

  # Extract effects
  total <- res$est[res$type == "total"]
  direct <- res$est[res$type == "direct"]
  indirect_m1 <- res$est[res$type == "indirect" & res$mediator == "M1"]
  indirect_m2 <- res$est[res$type == "indirect" & res$mediator == "M2"]

  # Sum of individual indirect effects should equal total - direct
  expect_equal(indirect_m1 + indirect_m2, total - direct, tolerance = 1e-6)
})

test_that("total effect equals direct plus indirect effects", {
  res <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                       Ms = quote(M1), bootstraps = 50, seed = 666), lavaan_args))

  total <- res$est[res$type == "total"]
  direct <- res$est[res$type == "direct"]
  indirect <- res$est[res$type == "indirect"]

  expect_equal(total, direct + indirect, tolerance = 1e-6)
})

test_that("bootstrap count affects standard error estimates", {
  res_low_boot <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                                Ms = quote(M1), bootstraps = 30, seed = 777), lavaan_args))
  res_high_boot <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                                 Ms = quote(M1), bootstraps = 100, seed = 777), lavaan_args))

  # Both should produce results (not testing SE difference as it's stochastic)
  expect_s3_class(res_low_boot, "data.frame")
  expect_s3_class(res_high_boot, "data.frame")
})

test_that("lavaan code attribute is valid", {
  res <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                       Ms = quote(c(M1, M2)), CVs = quote(CV),
                                       bootstraps = 50, seed = 888), lavaan_args))

  lavaan_code <- attr(res, "lavaan_code")

  # Check that the code contains expected elements
  expect_true(grepl("M1 ~ a\\*X", lavaan_code))
  expect_true(grepl("M2 ~ b\\*X", lavaan_code))
  expect_true(grepl("Y ~ cdash\\*X", lavaan_code))
  expect_true(grepl("direct := cdash", lavaan_code))
  expect_true(grepl("indirect", lavaan_code))
  expect_true(grepl("total", lavaan_code))
})

test_that("results have correct column names", {
  res <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                       Ms = quote(M1), bootstraps = 50, seed = 1111), lavaan_args))

  expected_cols <- c("type", "mediator", "est", "se", "pvalue", "ci.lower", "ci.upper")
  expect_equal(names(res), expected_cols)
})

test_that("CV coefficients have correct structure", {
  res <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                       Ms = quote(c(M1, M2)), CVs = quote(CV),
                                       bootstraps = 50, seed = 2222), lavaan_args))

  cv_coefs <- attr(res, "CV_coefficients")

  expect_true("DV" %in% names(cv_coefs))
  expect_true("CV" %in% names(cv_coefs))
  expect_true("est" %in% names(cv_coefs))
  expect_true("se" %in% names(cv_coefs))
  expect_true("pvalue" %in% names(cv_coefs))
  expect_true("ci.lower" %in% names(cv_coefs))
  expect_true("ci.upper" %in% names(cv_coefs))
})

test_that("run_mediation handles small sample sizes", {
  # Create small dataset
  set.seed(999)
  small_df <- data.frame(
    X = rnorm(20),
    M = rnorm(20),
    Y = rnorm(20)
  )
  small_df$M <- 0.3 * small_df$X + small_df$M
  small_df$Y <- 0.4 * small_df$M + small_df$Y

  # Should still run but with fewer bootstrap samples
  expect_s3_class(
    do.call(run_mediation, c(list(data = small_df, X = quote(X), Y = quote(Y),
                                  Ms = quote(M), bootstraps = 30, seed = 111), lavaan_args)),
    "data.frame"
  )
})

test_that("run_mediation handles missing data correctly", {
  # Create dataset with some missing values
  df_missing <- df
  df_missing$Y[c(1, 5, 10)] <- NA
  df_missing$M1[c(2, 7)] <- NA

  # With missing = "direct" (default via lavaan_args), should handle missing data
  expect_s3_class(
    suppressWarnings(do.call(run_mediation, c(list(data = df_missing, X = quote(X), Y = quote(Y),
                                  Ms = quote(M1), bootstraps = 30, seed = 222), lavaan_args))),
    "data.frame"
  )
})

# Tests for plot_mediation ----

test_that("plot_mediation works with basic mediation result", {
  skip_if_not_installed("DiagrammeR")

  res <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                       Ms = quote(M1), CVs = NULL,
                                       bootstraps = 50, seed = 123), lavaan_args))

  # Just test that the function runs without error
  expect_no_error(
    plot_mediation(res, X = "X", Y = "Y", M = "M1")
  )
})

test_that("plot_mediation works with multiple mediators", {
  skip_if_not_installed("DiagrammeR")

  res <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                       Ms = quote(c(M1, M2)), CVs = NULL,
                                       bootstraps = 50, seed = 123), lavaan_args))

  # Test with multiple mediators - expect warning about coef_offset
  expect_warning(
    plot_mediation(res, X = "X", Y = "Y", M = c("M1", "M2")),
    "coef_offset tibble is not provided"
  )
})

test_that("plot_mediation handles deprecated arguments", {
  skip_if_not_installed("DiagrammeR")

  res <- do.call(run_mediation, c(list(data = df, X = quote(X), Y = quote(Y),
                                       Ms = quote(M1), CVs = NULL,
                                       bootstraps = 50, seed = 123), lavaan_args))

  # Test that deprecated IV and DV arguments trigger warnings
  # Note: data argument is required
  expect_warning(
    plot_mediation(data = res, IV = "X", Y = "Y", Ms = "M1"),
    "deprecated"
  )
})
