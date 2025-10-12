
library(magrittr)

mod1 <- lm_std(mpg ~ wt + hp, mtcars, weights = cyl)

mod4 <- with(mtcars, lm_std(mpg ~ wt + hp, weights = 1:32))

attach(mtcars, name = "mtcars")
on.exit(detach("mtcars"))

mod2 <- lm_std(mpg ~ wt + hp, weights = cyl)

mod3 <- lm_std(mpg ~ wt + hp, weights = 1:32)


test_that("lm_std fits a basic linear model correctly", {
  model <- lm_std(mpg ~ hp, data = mtcars)
  expect_s3_class(model, "lm_std")
  expect_true(attr(model, "standardized"))
  
  # With single predictor, std regression coefficient = correlation coefficient
  
  expect_equal(cor(iris$Sepal.Length, iris$Sepal.Width) %>% unname(),
               lm_std(Sepal.Length ~ Sepal.Width, iris)$coefficients[2]  %>% unname())

})

test_that("lm_std works with weights", {
  # Create weights
  mtcars$wt_sample <- runif(nrow(mtcars), 1, 3)
  
  model_weighted <- lm_std(mpg ~ hp + wt, data = mtcars, weights = wt_sample)
  expect_s3_class(model_weighted, "lm_std")
  expect_true(attr(model_weighted, "standardized"))
  
  # Check that the model was fitted with weights
  expect_equal(model_weighted$weights, mtcars$wt_sample)
})

test_that("lm_std handles variables with few distinct values", {
  mtcars$gear_num <- as.numeric(mtcars$gear) # gear has few distinct values
  
  expect_warning(
    model <- lm_std(mpg ~ cyl + disp + gear_num, data = mtcars),
    "numeric variables have fewer than three distinct values",
    fixed = TRUE
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
  model <- lm_std(mpg ~ hp + wt + am_factor, data = mtcars)
  expect_s3_class(model, "lm_std")
  
  # Check that factor variable is included as is
  expect_true("am_factor1" %in% names(coef(model)))
})

test_that("lm_std prevents usage of 'subset' argument", {
  expect_error(
    lm_std(mpg ~ hp + wt, data = mtcars, subset = mpg > 20),
    "Cannot subset in this function as that would happen after standardisation - please subset first."
  )
})

test_that("lm_std handles '...' arguments correctly", {
  model <- lm_std(mpg ~ hp + wt, data = mtcars, na.action = na.exclude)
  expect_s3_class(model, "lm_std")
  
  # Check that na.action is set correctly
  expect_equal(model$na.action, na.exclude)
})

test_that("lm_std works without data argument", {
  mpg <- mtcars$mpg
  hp <- mtcars$hp
  wt <- mtcars$wt
  
  model <- lm_std(mpg ~ hp + wt)
  expect_s3_class(model, "lm_std")
  
  # Check coefficients (should match manual standardisation)
  expected_beta <- coef(lm(scale(mpg) ~ scale(hp) + scale(wt)))["scale(hp)"]
  expect_equal(
    round(unname(coef(model)["hp"]), 4),
    round(unname(expected_beta), 4)
  )
})

test_that("lm_std assigns correct class and attributes", {
  model <- lm_std(mpg ~ hp + wt, data = mtcars)
  expect_s3_class(model, "lm_std")
  expect_true(attr(model, "standardized"))
  
  # Ensure it still inherits from "lm"
  expect_s3_class(model, "lm")
})

test_that("lm_std warns when binary variables are not factors", {
  
  expect_warning(
    lm_std(mpg ~ hp + wt + am, data = mtcars),
    'numeric variables have fewer than three distinct values',
    fixed = TRUE
  )
})


test_that("lm_std works", {
  expect_equal(round(summary(mod1)$coefficients[2,1], 3), -0.624)
  expect_equal(summary(mod1)$coefficients, summary(mod2)$coefficients)
  expect_equal(summary(mod3)$coefficients, summary(mod4)$coefficients)
  expect_equal(round(summary(mod3)$coefficients[2,1], 3), -0.647)
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


x <- suppressWarnings(paired_t_test_d(iris, "Sepal.Width", "Petal.Length"))

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

test_that("get_pairwise_letters handles all non-significant comparisons", {
  # Create p-value matrix where all comparisons are non-significant
  tests <- tibble::tibble(
    x = c("A", "A", "B"),
    y = c("B", "C", "C"),
    p_value = c(0.1, 0.2, 0.3)  # All > 0.05
  )

  result <- get_pairwise_letters(tests, alpha_level = 0.05)

  # All groups should have the same letter since no significant differences
  expect_equal(length(unique(result$letters)), 1)
})

test_that("get_pairwise_letters handles all significant comparisons", {
  # Create p-value matrix where all comparisons are significant
  tests <- tibble::tibble(
    x = c("A", "A", "B"),
    y = c("B", "C", "C"),
    p_value = c(0.001, 0.002, 0.003)  # All < 0.05
  )

  result <- get_pairwise_letters(tests, alpha_level = 0.05)

  # Each group should have a different letter
  expect_equal(nrow(result), 3)
  expect_equal(length(unique(result$letters)), 3)
})

test_that("get_pairwise_letters handles NaN p-values correctly", {
  # Create test with NaN p-values (e.g., from groups with zero variance)
  # All comparisons are either non-significant or NaN
  pmat <- matrix(c(1, 0.3, NaN,
                   0.3, 1, NaN,
                   NaN, NaN, 1),
                 nrow = 3, byrow = TRUE,
                 dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

  pw_test <- list(p.value = pmat[-1, -ncol(pmat), drop = FALSE])
  class(pw_test) <- "pairwise.htest"

  # NaN should be treated as non-significant (groups can't be shown to differ)
  # With all p-values >= 0.05 or NaN, all groups should get the same letter
  result <- suppressWarnings(get_pairwise_letters(pw_test))
  expect_equal(length(unique(result$letters)), 1)
  expect_s3_class(result, "data.frame")
})

test_that("lm_std handles zero variance variables", {
  test_data <- mtcars
  test_data$constant <- 5  # Zero variance

  expect_warning(
    lm_std(mpg ~ hp + constant, data = test_data),
    "zero variance"
  )
})

test_that("lm_std handles weighted_standardize parameter correctly", {
  test_data <- mtcars
  test_data$weights <- runif(nrow(test_data), 0.5, 1.5)

  # Should use weighted standardization by default when weights are provided
  expect_message(
    lm_std(mpg ~ hp + wt, data = test_data, weights = weights, weighted_standardize = "auto"),
    "weighted means and standard deviations"
  )

  # Should use unweighted when explicitly requested
  # Note: The respective warning appears only once per session, so we suppress all warnings
  expect_message(
    suppressWarnings(
      lm_std(mpg ~ hp + wt, data = test_data, weights = weights, weighted_standardize = FALSE)
    ),
    "UNweighted"
  )

  # Should error if weighted_standardize = TRUE but no weights
  expect_error(
    lm_std(mpg ~ hp + wt, data = test_data, weighted_standardize = TRUE),
    "requires a.*weights.*argument"
  )
})

test_that("polr_std handles edge cases", {
  skip_if_not_installed("MASS")

  # Test with factor outcome
  test_data <- WVS

  expect_s3_class(
    polr_std(poverty ~ religion + age, data = test_data),
    "polr"
  )

  # Test error when factor() is in formula
  expect_error(
    polr_std(poverty ~ factor(religion) + age, data = test_data),
    "factor\\(\\) needs to be used before polr_std"
  )
})

test_that("dummy_code handles edge cases", {
  # Test with NULL prefix
  result_null <- dummy_code(iris$Species[1:10], prefix = NULL, verbose = FALSE)
  expect_true(all(!grepl("^species_", names(result_null))))

  # Test with drop_first = FALSE
  result_all <- dummy_code(iris$Species[1:10], drop_first = FALSE, verbose = FALSE)
  expect_equal(ncol(result_all), 3)  # Should have all levels

  # Test with character vector
  char_vec <- c("A", "B", "A", "C", "B")
  result_char <- dummy_code(char_vec, prefix = "test", verbose = FALSE)
  expect_equal(ncol(result_char), 2)  # n-1 dummies
})

# Tests for multiply imputed data t-tests ----

test_that("t_test_mi works with basic two-group comparison", {
  skip_if_not_installed("mice")

  # Create imputed data with a two-level grouping variable
  set.seed(123)
  nhanes_subset <- mice::nhanes2
  # Create a binary group variable
  nhanes_subset$binary_group <- factor(ifelse(nhanes_subset$age %in% c("20-39"), "young", "older"))

  # Suppress mice warnings about logged events
  imp <- suppressWarnings(mice::mice(nhanes_subset, m = 5, printFlag = FALSE))
  imp_list <- mice::complete(imp, action = "long") %>%
    dplyr::group_split(.imp)

  result <- t_test_mi(imp_list, bmi, binary_group)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("x", "y", "mean_diff", "t_value", "df", "p_value", "group_var"))
  expect_equal(result$group_var, "binary_group")
  expect_equal(nrow(result), 1)
  expect_true(is.numeric(result$mean_diff))
  expect_true(is.numeric(result$p_value))
})

test_that("t_test_mi errors with more than two groups", {
  skip_if_not_installed("mice")

  set.seed(123)
  imp <- mice::mice(mice::nhanes2, m = 3, printFlag = FALSE)
  imp_list <- mice::complete(imp, action = "long") %>%
    dplyr::group_split(.imp)

  # age variable has 3 levels
  expect_error(
    t_test_mi(imp_list, bmi, age),
    "Group should only have two levels"
  )
})

test_that("pairwise_t_test_mi works with multiple groups", {
  skip_if_not_installed("mice")

  set.seed(123)
  imp <- mice::mice(mice::nhanes2, m = 5, printFlag = FALSE)
  imp_list <- mice::complete(imp, action = "long") %>%
    dplyr::group_split(.imp)

  result <- pairwise_t_test_mi(imp_list, bmi, hyp, p.adjust.method = "none")

  expect_s3_class(result, "tbl_df")
  expect_true("p_value" %in% names(result))
  expect_true("mean_diff" %in% names(result))
  expect_true(nrow(result) >= 1)  # Should have at least one comparison
  expect_true(all(result$p_value >= 0 & result$p_value <= 1))
})

test_that("pairwise_t_test_mi p-value adjustment works", {
  skip_if_not_installed("mice")

  set.seed(123)
  imp <- mice::mice(mice::nhanes2, m = 5, printFlag = FALSE)
  imp_list <- mice::complete(imp, action = "long") %>%
    dplyr::group_split(.imp)

  result_none <- pairwise_t_test_mi(imp_list, bmi, hyp, p.adjust.method = "none")
  result_holm <- pairwise_t_test_mi(imp_list, bmi, hyp, p.adjust.method = "holm")

  # Adjusted p-values should be >= unadjusted (more conservative)
  expect_true(all(result_holm$p_value >= result_none$p_value))
})

# Tests for t_test function ----

test_that("t_test works for one-sample t-test", {
  result <- t_test(mtcars$mpg, mu = 20)

  expect_s3_class(result, "timesaveR_t_test")
  expect_equal(result$test_type, "one_sample")
  expect_true(!is.null(result$cohens_d))
  expect_true(is.numeric(result$statistic))
  expect_true(is.numeric(result$p.value))

  # Compare to base t.test
  base_result <- t.test(mtcars$mpg, mu = 20)
  expect_equal(result$statistic, unname(base_result$statistic))
  expect_equal(result$p.value, base_result$p.value)

  # Check Cohen's d calculation
  expected_d <- (mean(mtcars$mpg) - 20) / sd(mtcars$mpg)
  expect_equal(result$cohens_d, expected_d)
})

test_that("t_test works for independent samples with formula", {
  result <- t_test(mpg ~ am, data = mtcars)

  expect_s3_class(result, "timesaveR_t_test")
  expect_equal(result$test_type, "independent")
  expect_true(!is.null(result$cohens_d))

  # Compare to base t.test
  base_result <- t.test(mpg ~ am, data = mtcars)
  expect_equal(result$statistic, unname(base_result$statistic))
  expect_equal(result$p.value, base_result$p.value)
})

test_that("t_test works for independent samples with vectors", {
  result <- t_test(mtcars$mpg[mtcars$am == 0], mtcars$mpg[mtcars$am == 1])

  expect_s3_class(result, "timesaveR_t_test")
  expect_equal(result$test_type, "independent")
  expect_true(!is.null(result$cohens_d))

  # Compare to base t.test
  base_result <- t.test(mtcars$mpg[mtcars$am == 0], mtcars$mpg[mtcars$am == 1])
  expect_equal(result$statistic, unname(base_result$statistic))
  expect_equal(result$p.value, base_result$p.value)
})

test_that("t_test works for paired samples", {
  result <- t_test(iris$Sepal.Width, iris$Petal.Length, paired = TRUE)

  expect_s3_class(result, "timesaveR_t_test")
  expect_equal(result$test_type, "paired")
  expect_true(!is.null(result$cohens_d))

  # Compare to base t.test
  base_result <- t.test(iris$Sepal.Width, iris$Petal.Length, paired = TRUE)
  expect_equal(result$statistic, unname(base_result$statistic))
  expect_equal(result$p.value, base_result$p.value)

  # Check Cohen's d calculation
  diff_vec <- iris$Sepal.Width - iris$Petal.Length
  expected_d <- mean(diff_vec) / sd(diff_vec)
  expect_equal(result$cohens_d, expected_d)
})

test_that("t_test handles var.equal parameter correctly", {
  result_welch <- t_test(mpg ~ am, data = mtcars, var.equal = FALSE)
  result_student <- t_test(mpg ~ am, data = mtcars, var.equal = TRUE)

  # Degrees of freedom should differ
  expect_false(result_welch$parameter == result_student$parameter)

  # Student's t-test should have integer df
  expect_true(result_student$parameter == (nrow(mtcars) - 2))
})

test_that("t_test handles alternative hypothesis correctly", {
  result_two <- t_test(mtcars$mpg, mu = 20, alternative = "two.sided")
  result_less <- t_test(mtcars$mpg, mu = 20, alternative = "less")
  result_greater <- t_test(mtcars$mpg, mu = 20, alternative = "greater")

  expect_equal(result_two$alternative, "two.sided")
  expect_equal(result_less$alternative, "less")
  expect_equal(result_greater$alternative, "greater")

  # P-values should differ
  expect_false(result_two$p.value == result_less$p.value)
})

test_that("t_test handles confidence level correctly", {
  result_95 <- t_test(mtcars$mpg, mu = 20, conf.level = 0.95)
  result_99 <- t_test(mtcars$mpg, mu = 20, conf.level = 0.99)

  # 99% CI should be wider than 95% CI
  width_95 <- result_95$conf.high - result_95$conf.low
  width_99 <- result_99$conf.high - result_99$conf.low
  expect_true(width_99 > width_95)
})

test_that("t_test errors with invalid formula", {
  expect_error(
    t_test(mpg ~ am + cyl, data = mtcars),
    "Formula must be of the form: outcome ~ group"
  )
})

test_that("t_test errors with non-numeric outcome", {
  test_data <- mtcars
  test_data$char_var <- as.character(test_data$mpg)

  expect_error(
    t_test(char_var ~ am, data = test_data),
    "Outcome variable must be numeric"
  )
})

test_that("t_test errors when grouping variable has wrong number of levels", {
  expect_error(
    t_test(mpg ~ cyl, data = mtcars),
    "must have exactly 2 levels"
  )
})

test_that("t_test print method works", {
  result <- t_test(mtcars$mpg, mu = 20)

  # Check that print doesn't error and returns invisibly
  expect_output(print(result), "Cohen's d")
  expect_output(print(result), "t =")
  expect_output(print(result), "p-value")

  # Check invisible return
  printed <- capture.output(returned <- print(result))
  expect_identical(returned, result)
})

test_that("t_test Cohen's d matches paired_t_test_d for paired samples", {
  # Use the same data as the existing paired_t_test_d test
  old_result <- suppressWarnings(paired_t_test_d(iris, "Sepal.Width", "Petal.Length"))
  new_result <- t_test(iris$Sepal.Width, iris$Petal.Length, paired = TRUE)

  # Cohen's d should match
  expect_equal(new_result$cohens_d, old_result$d, tolerance = 0.0001)
})

test_that("t_test handles missing values correctly", {
  x_with_na <- c(mtcars$mpg[1:20], NA, NA)
  y_with_na <- c(mtcars$hp[1:20], NA, NA)

  # Should handle NAs via na.rm in calculations
  result <- t_test(x_with_na, mu = 20)
  expect_true(!is.na(result$cohens_d))
  expect_true(!is.na(result$statistic))

  # Paired with NAs
  result_paired <- t_test(x_with_na, y_with_na, paired = TRUE)
  expect_true(!is.na(result_paired$cohens_d))
})

