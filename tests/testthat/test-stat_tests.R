
mod1 <- lm_std(mpg ~ wt + hp, mtcars, weights = cyl)
library(magrittr)

mod4 <- with(mtcars, lm_std(mpg ~ wt + hp, weights = 1:32))

attach(mtcars, name = "mtcars")
on.exit(detach("mtcars"))

mod2 <- lm_std(mpg ~ wt + hp, weights = cyl)

mod3 <- lm_std(mpg ~ wt + hp, weights = 1:32)

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
                