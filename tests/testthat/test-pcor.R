

test_that("pcor_matrix works", {
  expect_warning(out <- pcor_matrix(timesaveR::ess_health, given = c("agea", "gndr"), 
              var_names = c("health" = "Health", "weight" = "Weight", "dosprt" = "Sport")) %>% tidy(both_directions = FALSE))
  check_tibble(out, any.missing = FALSE)
  expect_true(all(c("column1", "column2", "estimate", "std.error", "p.value", "conf.low", "conf.high", "n", "statistic") %in% names(out)), label = "Test of column names")
  expect_equal(nrow(out), 3) 
  expect_equal(out$estimate[1], 0.144275925)
})

test_that("pcor_matrix p-value adjustment works", {
  # Test that p-value adjustment now works without error
  expect_warning(out_unadj <- pcor_matrix(timesaveR::ess_health, given = c("agea", "gndr"), 
              var_names = c("health" = "Health", "weight" = "Weight", "dosprt" = "Sport"), 
              adjust = "none"))
  
  expect_warning(out_adj <- pcor_matrix(timesaveR::ess_health, given = c("agea", "gndr"), 
              var_names = c("health" = "Health", "weight" = "Weight", "dosprt" = "Sport"), 
              adjust = "holm"))
  
  # Check that adjusted p-values are generally larger (more conservative)
  expect_true(all(out_adj$p.values >= out_unadj$p.values | is.na(out_adj$p.values)))
  
  # Verify correlations remain the same
  expect_equal(out_adj$cors, out_unadj$cors)
})
