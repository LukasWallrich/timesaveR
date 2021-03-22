
mod1 <- lm_std(mpg ~ wt + hp, mtcars, weights = cyl)
library(magrittr)

attach(mtcars, name = "mtcars")
on.exit(detach("mtcars"))

mod2 <- lm_std(mpg ~ wt + hp, weights = cyl)

mod3 <- lm_std(mpg ~ wt + hp, weights = 1:32)

test_that("lm_std works", {
  expect_equal(round(tidy(mod1)$estimate[2], 3), -0.585)
  expect_equal(tidy(mod1), tidy(mod2))
  expect_equal(round(tidy(mod3)$estimate[2], 3), -0.666)
})
