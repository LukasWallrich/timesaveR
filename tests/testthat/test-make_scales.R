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
ess_health <- timesaveR::ess_health %>% sample_n(100) %>% select(cgtsmke, dosprt, health)
add_missing <- function(x) {x[!rbinom(length(x), 1, .9)] <- NA; x}
ess_health <- ess_health %>% mutate(across(everything(), add_missing))

# Impute data
ess_health_mi <- mice(ess_health, printFlag = FALSE) 
ess_health_mi <- complete(ess_health_mi, "long", include = TRUE)

scale1 <- make_scale_mi(ess_health_mi, c("cgtsmke", "dosprt", "health"), "healthy", print_desc = FALSE)

scale2 <- make_scale_mi(ess_health_mi, c("cgtsmke", "dosprt", "health"), "healthy", boot = 100, print_desc = FALSE, parallel = 2, alpha_ci = .9)
scale2$descriptives$reliability_ci_lower

test_that("make_scale_mi works (including parallel)", {
  expect_equal(scale1$descriptives$reliability, 0.068783606)
  expect_equal(scale1$descriptives$mean, 3.7546667)
  expect_equal(scale2$descriptives$reliability, 0.068783606)
  expect_equal(scale2$descriptives$reliability_ci_lower, 0.046360357)
})
