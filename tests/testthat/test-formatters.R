test_that("statistical number formatting works", {
  expect_equal(
    fmt_p(c(0, .001, .0225, .0499, .1234, .999, 2, NA)),
    c("< .001", "= .001", "= .022", "= .050", "= .123", "> .99", "> 1 (!!)", NA)
  )
  expect_equal(
    fmt_pct(c(0, .001, .0225, NA, .1234, 2)),
    c("0.0%", "0.1%", "2.2%", NA, "12.3%", "200.0%")
  )
  expect_equal(
    fmt_cor(c(-1, 0.001, 0.5, NA)),
    c("-1.00", ".00", ".50", NA)
  )
  expect_equal(
    round_(c(-1, NA, 1.235)),
    c("-1.00", NA, "1.24")
  )
  expect_equal(
    fmt_ci(c(-1, NA, 1.234, 2), c(1, 0, 1.889, NA)),
    c("[-1.00, 1.00]", NA, "[1.23, 1.89]", NA)
  )
  expect_error(
    fmt_ci(c(-.1, 2), c(.3, 3), drop_0 = TRUE)
  )
  expect_equal(
    fmt_ci(c(-.1, .5), c(.3, .8), drop_0 = TRUE),
    c("[-.10, .30]", "[.50, .80]")
  )
})
