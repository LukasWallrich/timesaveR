
library(gt)

gt::gt(mtcars)

# library(mice)
# 
# set.seed(300688)
# imp <- mice(nhanes)
# 
# mod1 <- with(imp, lm(bmi ~ age))
# mod2 <- with(imp, lm_std(bmi ~ age))
# 
# tab <- report_lm_with_std(mod1, mod2)
# 
# test_that("mira accepted", {
#   expect_equal(tab$gt_tab$`_data`$Model1[1], "29.78 (2.59)***")
# })
# 
# library(MASS)
# pov_att <- polr(poverty ~ religion + age + gender, data = WVS)
# pov_att_std <- polr_std(poverty ~ religion + age + gender, data = WVS)
# tab <- report_polr_with_std(pov_att, pov_att_std, coef_omit = "\\|")
# 
# test_that("mira accepted", {
#   expect_equal(tab$gt_tab$`_data`$Model2[2], "1.25 *** [1.19, 1.32]")
# })

library(MASS)
pov_att <- polr(poverty ~ religion + age + gender, data = WVS[1:200,])
pov_att_std <- polr_std(poverty ~ religion + age + gender, data = WVS[1:200,])
res <- report_polr_with_std(pov_att, pov_att_std, coef_omit = "\\|")

test_that("polr table works", {
  expect_equal(res$gt_tab$`_data`$Model2, 
               c("1.02 &nbsp;&nbsp;&nbsp; [0.51, 2.03]", "1.28 &dagger;&nbsp;&nbsp; [0.98, 1.66]", "0.99 &nbsp;&nbsp;&nbsp; [0.59, 1.68]"))
})
