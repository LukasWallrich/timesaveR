mod1 <- lm(mpg ~ wt + am, mtcars)
mod2 <- lm(mpg ~ wt * am, mtcars)

test_that("report_anova for lm models", {
  expect_equal(report_anova(anova(mod1, mod2)), glue::glue("<em>F</em>(1, 28) = 13.45, <em>p</em> = .001"))
})

test_that("report_anova for lm models - reverse order", {
  # Test with models in different order - should handle negative df correctly
  # R's anova() returns negative Df when simpler model comes second, but we correct this
  expect_equal(report_anova(anova(mod2, mod1)), glue::glue("<em>F</em>(1, 28) = 13.45, <em>p</em> = .001"))
})

test_that("report_anova produces identical output regardless of model order", {
  # Both orders should produce the same output
  result_normal <- report_anova(anova(mod1, mod2))
  result_reverse <- report_anova(anova(mod2, mod1))
  expect_equal(result_normal, result_reverse)
})

test_that("report_anova edge cases - extreme p-values", {
  # Test with model that has very small p-value
  mod_null <- lm(mpg ~ 1, mtcars)
  mod_full <- lm(mpg ~ wt + hp + qsec, mtcars)
  result <- report_anova(anova(mod_null, mod_full))
  expect_match(result, "<em>p</em> < \\.001")
})


if (requireNamespace("car", quietly = TRUE)) {
  test_that("report_anova for linearHypothesis", {
    expect_equal(report_anova(car::linearHypothesis(mod1, c("(Intercept) = 30"))), glue::glue("<em>F</em>(1, 29) = 5.74, <em>p</em> = .023"))
  })

  test_that("report_anova for linearHypothesis - different hypothesis", {
    result <- report_anova(car::linearHypothesis(mod1, c("wt = 0")))
    expect_match(result, "<em>F</em>\\(1, 29\\)")
    expect_match(result, "<em>p</em>")
  })
}

library(lavaan)
mod1_sem <- sem("mpg ~ wt + am + 0 * wt:am", mtcars)
mod2_sem <- sem("mpg ~ wt + am + wt:am", mtcars)

test_that("report_anova for sem models", {
  expect_equal(report_anova(anova(mod2_sem, mod1_sem)), glue::glue("<em>&chi;</em><sup>2</sup>(1) = 12.55, <em>p</em> < .001"))
})

test_that("report_anova for sem models - reverse order", {
  result <- report_anova(anova(mod1_sem, mod2_sem))
  expect_match(result, "<em>&chi;</em><sup>2</sup>")
  expect_match(result, "<em>p</em>")
})

# Error handling tests
test_that("report_anova errors on more than 2 models", {
  mod3 <- lm(mpg ~ wt + am + hp, mtcars)
  expect_error(
    report_anova(anova(mod1, mod2, mod3)),
    "only supports comparisons between two models"
  )
})

test_that("report_anova errors on single model", {
  expect_error(
    report_anova(anova(mod1)),
    "only supports comparisons between two models"
  )
})

test_that("report_anova errors on object without heading attribute", {
  fake_anova <- data.frame(
    Df = c(NA, 1),
    `Res.Df` = c(29, 28),
    RSS = c(278.3, 195.5),
    F = c(NA, 13.45),
    `Pr(>F)` = c(NA, 0.001),
    check.names = FALSE
  )
  expect_error(
    report_anova(fake_anova),
    "not yet supported"
  )
})

test_that("report_anova errors on unsupported anova type", {
  # Create an anova-like object with unrecognized heading
  fake_anova <- data.frame(
    Df = c(NA, 1),
    `Res.Df` = c(29, 28),
    RSS = c(278.3, 195.5),
    F = c(NA, 13.45),
    `Pr(>F)` = c(NA, 0.001),
    check.names = FALSE
  )
  attr(fake_anova, "heading") <- "Some Unsupported Test Type"

  expect_error(
    report_anova(fake_anova),
    "not yet supported"
  )
})

# Output format validation tests
test_that("report_anova output contains proper HTML formatting", {
  result <- report_anova(anova(mod1, mod2))
  # Check for italic tags
  expect_match(result, "<em>F</em>")
  expect_match(result, "<em>p</em>")
})

test_that("report_anova output for SEM contains proper HTML formatting", {
  result <- report_anova(anova(mod2_sem, mod1_sem))
  # Check for chi-square symbol and superscript
  expect_match(result, "<em>&chi;</em>")
  expect_match(result, "<sup>2</sup>")
  expect_match(result, "<em>p</em>")
})
