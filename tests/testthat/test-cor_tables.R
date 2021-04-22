test_that("cor_matrix works", {
  expect_equal(cor_matrix(mtcars, c(mpg = "Hello", cyl = "You"))[[1]][1,2], 
               cor(mtcars$mpg, mtcars$cyl))
})

ess_survey <- srvyr::as_survey(ess_health,
                        weights = pweight)

test_that("svy_cor_matrix works", {
  set.seed(1234)
  expect_equal(svy_cor_matrix(ess_survey, c(health = "Health", agea = "Age"))[[1]][1,2], 
               0.27805754)
})
