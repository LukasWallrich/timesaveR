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
