
mod1 <- lm_std(mpg ~ wt + hp, mtcars, weights = cyl)
library(magrittr)

mod4 <- with(mtcars, lm_std(mpg ~ wt + hp, weights = 1:32))

attach(mtcars, name = "mtcars")
on.exit(detach("mtcars"))

mod2 <- lm_std(mpg ~ wt + hp, weights = cyl)

mod3 <- lm_std(mpg ~ wt + hp, weights = 1:32)

test_that("lm_std works", {

})

test_that("lm_std fits a basic linear model correctly", {
  model <- lm_std(mpg ~ cyl, data = mtcars)
  expect_s3_class(model, "lm_std")
  expect_true(attr(model, "standardized"))
  
  # With single predictor, std regression coefficient = correlation coefficient
  
  expect_equal(cor(iris$Sepal.Length, iris$Sepal.Width) %>% unname(),
               lm_std(Sepal.Length ~ Sepal.Width, iris)$coefficients[2]  %>% unname())

})

test_that("lm_std works with weights", {
  # Create weights
  mtcars$wt_sample <- runif(nrow(mtcars), 1, 3)
  
  model_weighted <- lm_std(mpg ~ cyl + disp, data = mtcars, weights = wt_sample)
  expect_s3_class(model_weighted, "lm_std")
  expect_true(attr(model_weighted, "standardized"))
  
  # Check that the model was fitted with weights
  expect_equal(model_weighted$weights, mtcars$wt_sample)
})

test_that("lm_std handles variables with few distinct values", {
  mtcars$gear_num <- as.numeric(mtcars$gear) # gear has few distinct values
  
  expect_warning(
    model <- lm_std(mpg ~ cyl + disp + gear_num, data = mtcars),
    "The following numeric variables have fewer than three distinct values: gear_num. Consider converting them to factors as standardizing them is typically not recommended."
  )
  
  expect_s3_class(model, "lm_std")
})

test_that("lm_std handles non-existent variables when data is NULL", {
  expect_error(
    lm_std(mpg ~ cyl + disp + nonexistent_var),
    "Variable 'nonexistent_var' not found in the parent environment."
  )
})

test_that("lm_std handles factor variables correctly", {
  mtcars$am_factor <- factor(mtcars$am)
  
  # Fit model with factor variable
  model <- lm_std(mpg ~ cyl + disp + am_factor, data = mtcars)
  expect_s3_class(model, "lm_std")
  
  # Check that factor variable is included as is
  expect_true("am_factor1" %in% names(coef(model)))
})

test_that("lm_std prevents usage of 'subset' argument", {
  expect_error(
    lm_std(mpg ~ cyl + disp, data = mtcars, subset = mpg > 20),
    "Cannot subset in this function as that would happen after standardisation - please subset first."
  )
})

test_that("lm_std handles '...' arguments correctly", {
  model <- lm_std(mpg ~ cyl + disp, data = mtcars, na.action = na.exclude)
  expect_s3_class(model, "lm_std")
  
  # Check that na.action is set correctly
  expect_equal(model$na.action, na.exclude)
})

test_that("lm_std works without data argument", {
  mpg <- mtcars$mpg
  cyl <- mtcars$cyl
  disp <- mtcars$disp
  
  model <- lm_std(mpg ~ cyl + disp)
  expect_s3_class(model, "lm_std")
  
  # Check coefficients
  expected_cor_cyl_mpg <- cor(cyl, mpg)
  expect_equal(round(coef(model)["cyl"], 4), round(expected_cor_cyl_mpg, 4))
})

test_that("lm_std assigns correct class and attributes", {
  model <- lm_std(mpg ~ cyl + disp, data = mtcars)
  expect_s3_class(model, "lm_std")
  expect_true(attr(model, "standardized"))
  
  # Ensure it still inherits from "lm"
  expect_s3_class(model, "lm")
})

test_that("lm_std fails gracefully with non-numeric data", {
  mtcars$gear_factor <- factor(mtcars$gear)
  
  expect_error(
    lm_std(mpg ~ cyl + disp + gear_factor, data = mtcars),
    "The following numeric variables have fewer than three distinct values: gear_factor."
  )
})


test_that("lm_std works", {
  expect_equal(round(summary(mod1)$coefficients[2,1], 3), -0.585)
  expect_equal(summary(mod1)$coefficients, summary(mod2)$coefficients)
  expect_equal(summary(mod3)$coefficients, summary(mod4)$coefficients)
  expect_equal(round(summary(mod3)$coefficients[2,1], 3), -0.666)
})

data("airquality")
airquality$month <- factor(airquality$Month, labels = month.abb[5:9])
x <- pairwise.t.test(airquality$Ozone, airquality$Month)

pl <- get_pairwise_letters(x)
out <- tibble::tribble(
  ~level, ~letters, ~a,    ~b,    ~c,    
  "5",    "a",      "a",   NA,  NA, 
  "6",    "ab",     "a",   "b",   NA, 
  "7",    "bc",     NA,  "b",   "c",  
  "8",    "c",      NA,  NA,  "c",  
  "9",    "a",      "a",   NA,  NA
)

test_that("pairwise letters work", {
  expect_equal(pl, out)
})


x <- paired_t_test_d(iris, "Sepal.Width", "Petal.Length")

test_that("paired t-test with d works", {
  expect_equal(x$d, -0.35185088)
  expect_equal(unname(x$t_test$statistic), -4.3092756)
})

dummies <- dummy_code(iris[c(1:5, 51:55, 101:105),]$Species) 
out <- tibble::tribble(
  ~species_versicolor, ~species_virginica, 
  FALSE,               FALSE,             
  FALSE,               FALSE,             
  FALSE,               FALSE,             
  FALSE,               FALSE,             
  FALSE,               FALSE,             
  TRUE,                FALSE,             
  TRUE,                FALSE,             
  TRUE,                FALSE,             
  TRUE,                FALSE,             
  TRUE,                FALSE,             
  FALSE,               TRUE,              
  FALSE,               TRUE,              
  FALSE,               TRUE,              
  FALSE,               TRUE,              
  FALSE,               TRUE
)
test_that("dummy_code works", {
  expect_equal(dummies, out)
})

test_that(".clean_names works", {
  expect_equal(
    .clean_names(c("HelloWorld", "How are you", "'Never BEEN better")), 
    c("hello_world", "how_are_you", "never_BEEN_better"))
})

test_that("pairwise t-test works", {
  expect_warning(t1 <- pairwise_t_tests(mtcars, wt, cyl)) # As cyl is not a factor
  expect_warning(t2 <- pairwise_t_tests(mtcars, wt ~ cyl)) # As cyl is not a factor
  expect_equal(t1, t2)
  expect_equal(t1$t_value, c(-3.80952793,  -6.44497389, -3.62123044))
  expect_equal(pairwise_t_tests(data.frame(x = 1:10, y = rep(c("a", "b"), 5)), x ~ y)$t_value, -.5)
})
                