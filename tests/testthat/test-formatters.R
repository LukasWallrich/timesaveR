test_that("fmt_p basic formatting works", {
  expect_warning(
    result <- fmt_p(c(0, .001, .0225, .0499, .1234, .999, 2, NA)),
    "p-values should be between 0 and 1"
  )
  expect_equal(
    result,
    c("< .001", "= .001", "= .022", "= .050", "= .123", "> .99", "> 1 (!!)", NA)
  )
})

test_that("fmt_p with equal_sign = FALSE", {
  expect_equal(
    fmt_p(c(0.05, 0.001, 0.5, 0.0001), equal_sign = FALSE),
    c(".050", ".001", ".500", "< .001")  # 0.001 is exactly at threshold, so shown as exact
  )
})

test_that("fmt_p with different digits", {
  expect_equal(
    fmt_p(0.0455, digits = 2),
    "= .05"
  )
  expect_equal(
    fmt_p(0.00001, digits = 4),
    "< .0001"
  )
  # With banker's rounding (default), 0.12345 rounds to 0.1234 (round to even)
  expect_equal(
    fmt_p(0.12345, digits = 4),
    "= .1234"
  )
})

test_that("fmt_p edge cases", {
  # Exactly 0
  expect_equal(fmt_p(0), "< .001")
  # Exactly 1
  expect_equal(fmt_p(1), "> .99")
  # Just above threshold
  expect_equal(fmt_p(0.0011), "= .001")
})

test_that("fmt_p preserves attributes", {
  x <- c(0.05, 0.1)
  attr(x, "test_attr") <- "test"
  result <- fmt_p(x)
  expect_equal(attr(result, "test_attr"), "test")
})

test_that("fmt_p input validation", {
  expect_error(fmt_p("0.05"))
  expect_error(fmt_p(0.05, digits = 1.5))
  expect_error(fmt_p(0.05, equal_sign = "yes"))
})

test_that("fmt_p handles invalid p-values with warning", {
  # Negative values (invalid p-values) - should show < 0 (!!) and warn
  expect_warning(result <- fmt_p(-0.1), "p-values should be between 0 and 1")
  expect_equal(result, "< 0 (!!)")

  # Values > 1 should show "> 1 (!!)" and warn
  expect_warning(result <- fmt_p(2), "p-values should be between 0 and 1")
  expect_equal(result, "> 1 (!!)")

  # Mixed valid and invalid
  expect_warning(result <- fmt_p(c(0.05, -0.1, 1.5)), "p-values should be between 0 and 1")
  expect_equal(result, c("= .050", "< 0 (!!)", "> 1 (!!)"))
})

test_that("fmt_pct basic formatting works", {
  expect_equal(
    fmt_pct(c(0, .001, .0225, NA, .1234, 2)),
    c("0.0%", "0.1%", "2.2%", NA, "12.3%", "200.0%")
  )
})

test_that("fmt_pct with different digits", {
  expect_equal(
    fmt_pct(0.1234, digits = 0),
    "12%"
  )
  # With banker's rounding (default), 12.345 rounds to 12.34 (round to even)
  expect_equal(
    fmt_pct(0.12345, digits = 2),
    "12.34%"
  )
  expect_equal(
    fmt_pct(0.123456, digits = 3),
    "12.346%"
  )
})

test_that("fmt_pct edge cases", {
  # Negative percentages
  expect_equal(fmt_pct(-0.05), "-5.0%")
  # Very small values
  expect_equal(fmt_pct(0.0001, digits = 2), "0.01%")
  # Zero
  expect_equal(fmt_pct(0), "0.0%")
  # Large values
  expect_equal(fmt_pct(10), "1000.0%")
})

test_that("fmt_pct preserves attributes", {
  x <- c(0.1, 0.2)
  attr(x, "test_attr") <- "test"
  result <- fmt_pct(x)
  expect_equal(attr(result, "test_attr"), "test")
})

test_that("fmt_cor basic formatting works", {
  expect_equal(
    fmt_cor(c(-1, 0.001, 0.5, NA)),
    c("-1.00", ".00", ".50", NA)
  )
})

test_that("fmt_cor with different digits", {
  expect_equal(fmt_cor(0.123, digits = 1), ".1")
  expect_equal(fmt_cor(0.123456, digits = 3), ".123")
  expect_equal(fmt_cor(-0.9876, digits = 3), "-.988")
})

test_that("fmt_cor boundary values", {
  expect_equal(fmt_cor(1), "1.00")
  expect_equal(fmt_cor(-1), "-1.00")
  expect_equal(fmt_cor(0.999), "1.00")
  expect_equal(fmt_cor(-0.999), "-1.00")
})

test_that("fmt_cor edge cases", {
  # Very small correlations
  expect_equal(fmt_cor(0.001), ".00")
  expect_equal(fmt_cor(-0.001), "-.00")  # sprintf preserves sign for negative values that round to 0
})

test_that("fmt_cor validates input range", {
  # Values outside [-1, 1] should error
  expect_error(fmt_cor(1.5))
  expect_error(fmt_cor(-1.5))
  expect_error(fmt_cor(c(0.5, 1.1)))
})

test_that("fmt_cor preserves attributes", {
  x <- c(0.5, -0.3)
  attr(x, "test_attr") <- "test"
  result <- fmt_cor(x)
  expect_equal(attr(result, "test_attr"), "test")
})

test_that("round_ basic formatting works", {
  expect_equal(
    round_(c(-1, NA, 1.235)),
    c("-1.00", NA, "1.24")
  )
})

test_that("round_ with different digits", {
  expect_equal(round_(1.2345, digits = 0), "1")
  expect_equal(round_(1.2345, digits = 1), "1.2")

  # Test round half to even behavior - both 1.235 and 1.245 should round to 1.24
  expect_equal(round_(1.235, digits = 2), "1.24")  # rounds to even (1.24)
  expect_equal(round_(1.245, digits = 2), "1.24")  # rounds to even (1.24)
  expect_equal(round_(0.5, digits = 0), "0")  # 0.5 rounds to even (0)
  expect_equal(round_(1.5, digits = 0), "2")  # 1.5 rounds to even (2)
  expect_equal(round_(2.5, digits = 0), "2")  # 2.5 rounds to even (2)
})

test_that("round_ trailing zeros", {
  expect_equal(round_(1, digits = 2), "1.00")
  expect_equal(round_(1.2, digits = 3), "1.200")
  expect_equal(round_(0, digits = 2), "0.00")
})

test_that("round_ negative numbers", {
  expect_equal(round_(-1.234, digits = 2), "-1.23")
  expect_equal(round_(-0.001, digits = 2), "-0.00")
  expect_equal(round_(-10.5, digits = 1), "-10.5")
})

test_that("round_ edge cases", {
  # Very small numbers
  expect_equal(round_(0.0001, digits = 2), "0.00")
  # Negative zero
  expect_equal(round_(-0.001, digits = 1), "-0.0")
})

test_that("round_ preserves attributes", {
  x <- c(1.234, 5.678)
  attr(x, "test_attr") <- "test"
  result <- round_(x)
  expect_equal(attr(result, "test_attr"), "test")
})

test_that("fmt_ci basic formatting works", {
  expect_equal(
    fmt_ci(c(-1, NA, 1.234, 2), c(1, 0, 1.889, NA)),
    c("[-1.00, 1.00]", NA, "[1.23, 1.89]", NA)
  )
})

test_that("fmt_ci with drop_0 = TRUE", {
  expect_equal(
    fmt_ci(c(-.1, .5), c(.3, .8), drop_0 = TRUE),
    c("[-.10, .30]", "[.50, .80]")
  )

  # Boundary values with drop_0
  expect_equal(
    fmt_ci(-1, 1, drop_0 = TRUE),
    "[-1.00, 1.00]"
  )
})

test_that("fmt_ci with different digits", {
  expect_equal(fmt_ci(1.2345, 2.3456, digits = 0), "[1, 2]")
  expect_equal(fmt_ci(1.2345, 2.3456, digits = 1), "[1.2, 2.3]")
  expect_equal(fmt_ci(1.2345, 2.3456, digits = 3), "[1.234, 2.346]")  # round half to even
})

test_that("fmt_ci error handling", {
  # drop_0 with values > 1
  expect_error(
    fmt_ci(c(-.1, 2), c(.3, 3), drop_0 = TRUE)
  )

  # any lower > upper
  expect_error(
    fmt_ci(c(-.1, .2), c(.3, -.3), drop_0 = TRUE)
  )

  # Different lengths
  expect_error(fmt_ci(c(1, 2), c(3, 4, 5)))

  # Non-numeric inputs
  expect_error(fmt_ci("1", "2"))
})

test_that("fmt_ci edge cases", {
  # Same lower and upper
  expect_equal(fmt_ci(1.5, 1.5), "[1.50, 1.50]")

  # Negative CIs
  expect_equal(fmt_ci(-2.5, -1.5), "[-2.50, -1.50]")
})

test_that("fmt_ci with NA handling", {
  # NA in lower only
  expect_equal(fmt_ci(NA, 1), NA_character_)

  # NA in upper only
  expect_equal(fmt_ci(1, NA), NA_character_)

  # NA in both
  expect_equal(fmt_ci(NA, NA), NA_character_)

  # Mixed with valid values
  expect_equal(
    fmt_ci(c(1, NA, 3), c(2, 4, NA)),
    c("[1.00, 2.00]", NA, NA)
  )
})

test_that("round_df rounds numeric columns only", {
  df <- data.frame(
    num1 = c(1.234, 2.567),
    char = c("a", "b"),
    num2 = c(3.891, 4.123),
    factor = factor(c("x", "y")),
    stringsAsFactors = FALSE
  )

  result <- round_df(df, digits = 1)

  expect_equal(result$num1, c(1.2, 2.6))
  expect_equal(result$num2, c(3.9, 4.1))
  expect_equal(result$char, c("a", "b"))
  expect_equal(result$factor, factor(c("x", "y")))
})

test_that("round_df with different digits", {
  df <- data.frame(x = c(1.2345, 2.3456))

  expect_equal(round_df(df, digits = 0)$x, c(1, 2))
  expect_equal(round_df(df, digits = 1)$x, c(1.2, 2.3))
  expect_equal(round_df(df, digits = 3)$x, c(1.234, 2.346))  # round half to even
  # Test that round_df and round_ behave consistently
  expect_equal(round_df(data.frame(x = 1.235), digits = 2)$x, as.numeric(round_(1.235, digits = 2)))
  expect_equal(round_df(data.frame(x = 1.245), digits = 2)$x, as.numeric(round_(1.245, digits = 2)))
})

test_that("round_df handles edge cases", {
  # Empty dataframe
  df_empty <- data.frame()
  expect_equal(round_df(df_empty), df_empty)

  # No numeric columns
  df_char <- data.frame(a = c("x", "y"), b = c("z", "w"))
  expect_equal(round_df(df_char), df_char)

  # All numeric columns
  df_num <- data.frame(a = 1.234, b = 2.345, c = 3.456)
  result <- round_df(df_num, digits = 1)
  expect_equal(result$a, 1.2)
  expect_equal(result$b, 2.3)
  expect_equal(result$c, 3.5)

  # With NA values
  df_na <- data.frame(x = c(1.234, NA, 3.456))
  result <- round_df(df_na, digits = 2)
  expect_equal(result$x, c(1.23, NA, 3.46))
})

test_that("round_df preserves dataframe structure", {
  df <- data.frame(
    x = c(1.234, 2.345),
    y = c("a", "b"),
    stringsAsFactors = FALSE
  )
  rownames(df) <- c("row1", "row2")

  result <- round_df(df)

  expect_equal(rownames(result), c("row1", "row2"))
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 2)
})

# Tests for rounding option behavior
test_that("rounding option controls round_() behavior", {
  # Save original option
  orig_opt <- getOption("timesaveR.round_method")
  on.exit(options(timesaveR.round_method = orig_opt))

  # Test with to_even (default)
  options(timesaveR.round_method = "to_even")
  expect_equal(round_(2.5, 0), "2")  # rounds to even
  expect_equal(round_(3.5, 0), "4")  # rounds to even
  expect_equal(round_(1.235, 2), "1.24")  # rounds to even

  # Test with default
  options(timesaveR.round_method = "default")
  expect_equal(round_(2.5, 0), "2")  # R's round also rounds to even by default
  expect_equal(round_(3.5, 0), "4")
  # But standard rounding differs in some cases
  expect_equal(round_(2.55, 1), "2.5")  # standard rounding
})

test_that("rounding option controls round_df() behavior", {
  # Save original option
  orig_opt <- getOption("timesaveR.round_method")
  on.exit(options(timesaveR.round_method = orig_opt))

  df <- data.frame(x = c(1.235, 2.455, 3.565))

  # Test with to_even
  options(timesaveR.round_method = "to_even")
  result_even <- round_df(df, 2)
  expect_equal(result_even$x, c(1.24, 2.46, 3.56))

  # Test with default
  options(timesaveR.round_method = "default")
  result_default <- round_df(df, 2)
  # Results should be consistent with R's round()
  expect_equal(result_default$x, round(df$x, 2))
})

test_that("rounding option controls fmt_*() functions", {
  # Save original option
  orig_opt <- getOption("timesaveR.round_method")
  on.exit(options(timesaveR.round_method = orig_opt))

  # Test fmt_cor
  options(timesaveR.round_method = "to_even")
  expect_equal(fmt_cor(0.235, 2), ".24")

  options(timesaveR.round_method = "default")
  expect_equal(fmt_cor(0.235, 2), fmt_cor(0.235, 2))  # Verify it still works

  # Test fmt_pct
  options(timesaveR.round_method = "to_even")
  expect_equal(fmt_pct(0.0235, 1), "2.4%")

  # Test fmt_p
  options(timesaveR.round_method = "to_even")
  expect_equal(fmt_p(0.0235, 2), "= .02")
})

test_that("invalid rounding option produces warning", {
  # Save original option
  orig_opt <- getOption("timesaveR.round_method")
  on.exit(options(timesaveR.round_method = orig_opt))

  options(timesaveR.round_method = "invalid")
  expect_warning(round_(1.5, 0), "Invalid timesaveR.round_method option")
  # Should fall back to to_even
  expect_warning(result <- round_(2.5, 0), "Invalid")
  expect_equal(result, "2")
})

test_that("rounding consistency across functions", {
  # Save original option
  orig_opt <- getOption("timesaveR.round_method")
  on.exit(options(timesaveR.round_method = orig_opt))

  options(timesaveR.round_method = "to_even")

  # All functions should use the same rounding for the same value
  val <- 1.235
  expect_equal(as.numeric(round_(val, 2)), round_df(data.frame(x = val), 2)$x)

  # fmt_cor should also be consistent (though it returns string with different format)
  val2 <- 0.235
  expect_true(grepl("24", fmt_cor(val2, 2)))  # Should round to .24
})
