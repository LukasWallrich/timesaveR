x <- mtcars %>% dplyr::select(cyl, mpg, wt) %>% cor_matrix()

res1 <- tidy(x)
res2 <- tidy(x, both_directions = FALSE)

test_that("expected tibble format", {
  check_tibble(res1, any.missing = FALSE)
  expect_true(all(c("column1", "column2", "estimate", "std.error", "p.value", "conf.low", "conf.high", "n", "statistic") %in% names(res1)), label = "Test of column names")
  expect_equal(nrow(res1), 6) 
  expect_equal(nrow(res2), 3)
})

test_that("reject conf_level", {
  expect_error(tidy(x, conf_level == .9))
})