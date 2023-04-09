set.seed(1234)
X <- rnorm(100)
M1 <- 0.5*X + rnorm(100)
M2 <- 0.25*X + rnorm(100)
M3 <- 0.75*X + rnorm(100)
Y <- 0.5*M1 + 0.75*M2 + 0.25*M3 + 0.25* X + rnorm(100)
CV <- rnorm(100) + .2*M1 + .2*M2 + .2*M3 + .2*Y

df <- data.frame(X = X, M1 = M1, M2 = M2, M3 = M3, Y = Y, CV = CV)

res <- run_mediation(df, X, Y, c(M1, M2, M3), CV, bootstraps = 50, seed = 987654321)

test_that("parallel mediation works", {
  expect_data_frame(res, nrows = 11, ncols = 7)
  expect_equal(res[[8,3]], 0.074510899)
  expect_equal(res[[8,7]], 0.16711136)
  expect_data_frame(attr(res, "CV_coefficients"), nrows = 4, ncols = 7)
})
