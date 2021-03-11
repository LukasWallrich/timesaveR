test_that("significance stars work", {
  expect_equal(sigstars(c(0, .001, .01, .05, .1, 2)), c("***", "**", "*", "&dagger;", "", ""))
})

test_that("can unescape HTML", {
  expect_equal(.unescape_html(c("&amp;", "hi", ".01", "&dagger;")), 
               c("&", "hi", ".01", "†"))
})

test_that("line_to_vector works", {
  expect_equal(line_to_vector("Today   is  really  rather freezing", return = "vector")[c(3,5)], 
               c("really", "freezing"))
  expect_equal(line_to_vector("1 2 3", string = FALSE), 
               "c(1, 2, 3)")
})
