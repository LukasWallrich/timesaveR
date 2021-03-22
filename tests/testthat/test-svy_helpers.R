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

