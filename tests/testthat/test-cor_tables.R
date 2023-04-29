test_that("cor_matrix works", {
  expect_equal(cor_matrix(mtcars, c(mpg = "Hello", cyl = "You"))[[1]][1,2], 
               cor(mtcars$mpg, mtcars$cyl))
})

ess_survey <- srvyr::as_survey(ess_health %>% dplyr::select("agea", "health", "dosprt", "pweight"),
                               weights = pweight)

test_that("svy_cor_matrix works", {
  set.seed(1234)
  expect_equal(svy_cor_matrix(ess_survey, c(health = "Health", agea = "Age"))[[1]][1,2], 
               0.27805754)
})


# Create Dataset with missing data
library(mice)
library(dplyr)
set.seed(300688)
ess_health <- timesaveR::ess_health %>% sample_n(100) %>% select(cgtsmke, dosprt, health,  pspwght)
add_missing <- function(x) {x[!rbinom(length(x), 1, .9)] <- NA; x}
ess_health <- ess_health %>% mutate(across(c(everything(), -pspwght), add_missing))

# Impute data
ess_health_mi <- mice(ess_health, printFlag = FALSE) 
ess_health_mi <- complete(ess_health_mi, "long", include = TRUE)

out1 <- cor_matrix_mi(ess_health_mi)
out2 <- cor_matrix_mi(ess_health_mi, weights = pspwght)

test_that("cor_matrix_mi works", {
  expect_equal(out1[[1]][1,2], 
               -0.186112523)
  expect_equal(out2[[1]][1,2], 
               -0.19071925)
})


# Create Dataset with missing data
library(mice)
library(dplyr)
set.seed(300688)
ess_health <- timesaveR::ess_health %>% sample_n(100) %>% dplyr::select(cgtsmke, dosprt, health)
add_missing <- function(x) {x[!rbinom(length(x), 1, .9)] <- NA; x}
ess_health <- ess_health %>% mutate(across(c(everything()), add_missing))

out <- cor_matrix(ess_health, missing = "fiml")

test_that("cor_matrix works with fiml", {
  expect_equal(out$cors[2,1], -0.19917584)
})

out <- cor_matrix(ess_health, missing = "fiml", bootstrap = 10, seed = 12345)

test_that("cor_matrix works with bootstrapping", {
  expect_equal(out$cors[2,1], -0.23253694)
})  
