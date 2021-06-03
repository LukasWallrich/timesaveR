mod1 <- lm(mpg ~ wt + am, mtcars)
mod2 <- lm(mpg ~ wt * am, mtcars)
test_that("report_anova for lm models", {
  expect_equal(report_anova(anova(mod1, mod2)), glue::glue("<em>F</em>(1, 28) = 13.45, <em>p</em> = .001"))
})
