mod1 <- lm(mpg ~ wt + am, mtcars)
mod2 <- lm(mpg ~ wt * am, mtcars)
test_that("report_anova for lm models", {
  expect_equal(report_anova(anova(mod1, mod2)), glue::glue("<em>F</em>(1, 28) = 13.45, <em>p</em> = .001"))
})

if (requireNamespace("car", quietly = TRUE)) {
  test_that("report_anova for linearHypothesis ", {
    expect_equal(report_anova(car::linearHypothesis(mod1, c("(Intercept) = 30"))), glue::glue("<em>F</em>(1, 29) = 5.74, <em>p</em> = .023"))
  })
}

library(lavaan)
mod1 <- sem("mpg ~ wt + am + 0 * wt:am", mtcars)
mod2 <- sem("mpg ~ wt + am + wt:am", mtcars)

test_that("report_anova for sem models", {
  expect_equal(report_anova(anova(mod2, mod1)), glue::glue("<em>&chi;</em><sup>2</sup>(1) = 12.55, <em>p</em> < .001"))
})
