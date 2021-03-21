x <- mtcars %>% dplyr::select(cyl, mpg, wt) %>% cor_matrix()

res1 <- tidy(x)
res2 <- tidy(x, both_directions = FALSE)

test_that("expected tibble format", {
  check_tibble(res1, any.missing = FALSE)
  c("column1", "column1", "estimate", "std.error", "p.value", "conf.low", "conf.high", "n") %in% names(res1)
  expect_equal(nrow(res1), 6) 
  expect_equal(nrow(res2), 3)
})

test_that("reject conf_level", {
  expect_error(tidy(x, conf_level == .9))
})