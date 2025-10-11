
library(gt)
library(timesaveR)
modelsummary::config_modelsummary(startup_message = FALSE)
library(modelsummary)
library(mice)

set.seed(300688)
imp <- mice(nhanes)

mod1 <- with(imp, lm(bmi ~ age))
mod2 <- with(imp, lm_std(bmi ~ age))

tab <- report_lm_with_std(mod1, mod2)

test_that("mira accepted by report_lm_with_std", {
  expect_equal(tab$gt_tab$`_data`$Model1[1], "29.78 (2.59)***")
})



# Tests for mira.lm_F_test ----

test_that("mira.lm_F_test works with mira objects", {
  skip_if_not_installed("miceadds")

  # Use existing imputed data
  result <- mira.lm_F_test(mod1, return_list = TRUE)

  expect_type(result, "list")
  expect_named(result, c("F", "DoF", "DoF_residual", "p.value"))
  expect_true(is.numeric(result$F))
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
})

test_that("mira.lm_F_test returns formatted string", {
  skip_if_not_installed("miceadds")

  result <- mira.lm_F_test(mod1, return_list = FALSE)

  expect_type(result, "character")
  expect_match(result, "\\*F\\*\\(")  # Check for F-test format
  expect_match(result, "\\*p\\*")  # Check for p-value
})

test_that("mira.lm_F_test works with multiple predictors", {
  skip_if_not_installed("miceadds")

  # Create a more complex model
  mod_complex <- with(imp, lm(bmi ~ age + chl + hyp))
  result <- mira.lm_F_test(mod_complex, return_list = TRUE)

  expect_type(result, "list")
  expect_true(result$DoF > 1)  # Should have multiple DoF
  expect_true(is.numeric(result$F))
})
