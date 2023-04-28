

test_that("pcor_matrix works", {
  expect_warning(out <- pcor_matrix(ess_health, given = c("agea", "gndr"), 
              var_names = c("health" = "Health", "weight" = "Weight", "dosprt" = "Sport")) %>% tidy(both_directions = FALSE))
  check_tibble(out, any.missing = FALSE)
  expect_true(all(c("column1", "column2", "estimate", "std.error", "p.value", "conf.low", "conf.high", "n", "statistic") %in% names(out)), label = "Test of column names")
  expect_equal(nrow(out), 3) 
  expect_equal(out$estimate[1], 0.144275925)
})
