test_that("scale works", {
  expect_equal(make_scale(iris, c("Sepal.Length", "Petal.Length"), "Test", return_list = TRUE) %>% {
    paste(round(.$descriptives$Reliability, 2), .$scores[[1]])
  }, "0.93 3.25")
  expect_equal(make_scale(iris, c("Sepal.Length", "Petal.Length", "Sepal.Width"), "Test", reverse = "spec", reverse_items = "Sepal.Width", return_list = TRUE) %>% {
    paste(round(.$descriptives$Reliability, 2), round(.$scores[[1]], 2))
  }, "0.73 3.97")
})
