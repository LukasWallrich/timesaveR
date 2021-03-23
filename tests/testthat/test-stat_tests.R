
mod1 <- lm_std(mpg ~ wt + hp, mtcars, weights = cyl)
library(magrittr)

mod4 <- with(mtcars, lm_std(mpg ~ wt + hp, weights = 1:32))

attach(mtcars, name = "mtcars")
on.exit(detach("mtcars"))

mod2 <- lm_std(mpg ~ wt + hp, weights = cyl)

mod3 <- lm_std(mpg ~ wt + hp, weights = 1:32)

test_that("lm_std works", {
  expect_equal(round(summary(mod1)$coefficients[2,1], 3), -0.585)
  expect_equal(summary(mod1)$coefficients, summary(mod2)$coefficients)
  expect_equal(summary(mod3)$coefficients, summary(mod4)$coefficients)
  expect_equal(round(summary(mod3)$coefficients[2,1], 3), -0.666)
})
