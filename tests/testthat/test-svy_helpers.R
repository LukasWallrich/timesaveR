library(survey)
data(api)

svy_df <- svydesign(id = ~1, strata = ~stype, weights = ~pw, 
                    data = apistrat, fpc = ~fpc)
res <- svy_miss_var_summary(svy_df, acs.core, target, name)  

test_that("svy_miss_var_summary works", {
  expect_equal(res$n_miss[res$variable == ".any_missing"], 4238, ignore_attr = TRUE)
  expect_equal(nrow(res), 3)
})

res <- svy_miss_var_summary(svy_df, dplyr::starts_with("acs"))


test_that("svy_miss_var_summary uses selection helpers", {
  expect_equal(res$n_miss[res$variable == "acs.k3"], 1906, ignore_attr = TRUE)
  expect_equal(nrow(res), 4)
})

# Tests for svy_cohen_d_pair and svy_pairwise_t_test ----

test_that("svy_cohen_d_pair works with two-level factor", {
  # Create simple survey design with two groups
  test_data <- data.frame(
    id = 1:100,
    group = factor(rep(c("A", "B"), each = 50)),
    outcome = c(rnorm(50, mean = 10, sd = 2), rnorm(50, mean = 12, sd = 2)),
    weight = runif(100, 0.5, 1.5)
  )

  svy_test <- svydesign(id = ~id, weights = ~weight, data = test_data)

  result <- svy_cohen_d_pair(svy_test, "outcome", "group")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("pair", "d", "t", "df", "p.value") %in% names(result)))
  expect_equal(nrow(result), 1)
  expect_true(is.numeric(result$d))
  expect_true(is.numeric(result$p.value))
})

test_that("svy_cohen_d_pair works with specified pair", {
  # Use api data with multiple groups
  result <- svy_cohen_d_pair(svy_df, "api00", "stype", pair = c("E", "H"))

  expect_s3_class(result, "tbl_df")
  expect_equal(result$pair, "E & H")
  expect_true(is.numeric(result$d))
})

test_that("svy_cohen_d_pair errors appropriately", {
  # Test error when pair not specified for multi-level factor
  expect_error(
    svy_cohen_d_pair(svy_df, "api00", "stype"),
    "pair must not be NULL unless iv has exactly two distinct values"
  )

  # Test error when pair values not in data
  expect_error(
    svy_cohen_d_pair(svy_df, "api00", "stype", pair = c("X", "Y")),
    "All values in"
  )
})

test_that("svy_cohen_d_pair works without t-test", {
  result <- svy_cohen_d_pair(svy_df, "api00", "stype", pair = c("E", "H"), ttest = FALSE)

  expect_s3_class(result, "tbl_df")
  expect_equal(ncol(result), 2)  # Only pair and d columns
  expect_named(result, c("pair", "d"))
})

test_that("svy_pairwise_t_test works with multiple groups", {
  result <- svy_pairwise_t_test(svy_df, "api00", "stype", cats = c("E", "H", "M"))

  expect_s3_class(result, "tbl_df")
  expect_true("p.value" %in% names(result))
  expect_true("d" %in% names(result))
  expect_equal(nrow(result), 3)  # 3 groups = 3 pairwise comparisons
  expect_true(all(result$p.value >= 0 & result$p.value <= 1))
})

test_that("svy_pairwise_t_test p-value adjustment works", {
  result_none <- svy_pairwise_t_test(svy_df, "api00", "stype",
                                      cats = c("E", "H", "M"),
                                      p.adjust = "none")
  result_holm <- svy_pairwise_t_test(svy_df, "api00", "stype",
                                      cats = c("E", "H", "M"),
                                      p.adjust = "holm")

  # Adjusted p-values should be >= unadjusted
  expect_true(all(result_holm$p.value >= result_none$p.value))
})

