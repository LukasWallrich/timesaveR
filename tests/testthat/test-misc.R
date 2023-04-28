test_that("significance stars work", {
  expect_equal(sigstars(c(0, .001, .01, .05, .1, 2)), c("***", "**", "*", "&dagger;", "", ""))
})

test_that("significance stars can be padded", {
  expect_equal(sigstars(c(0, .001, .01, .05, .1, 2), pad_html = TRUE), 
               c("***", "**&nbsp;", "*&nbsp;&nbsp;", "†&nbsp;&nbsp;", "&nbsp;&nbsp;&nbsp;", "&nbsp;&nbsp;&nbsp;"))
})


test_that("can unescape HTML", {
  expect_equal(
    .unescape_html(c("&amp;", "hi", ".01", "&dagger;")),
    c("&", "hi", ".01", "†")
  )
})

test_that("line_to_vector works", {
  expect_equal(
    line_to_vector("Today   is  really  rather freezing", return = "vector")[c(3, 5)],
    c("really", "freezing")
  )
  expect_equal(
    line_to_vector("1 2 3", string = FALSE),
    "c(1, 2, 3)"
  )
})

set.seed(300688)
x <- cut_p(iris$Sepal.Length, p = c(.25, .50, .25), fct_levels = c("short", "middling", "long"), verbose = FALSE)

test_that("cut_p works", {
  expect_equal(levels(x), c("short", "middling", "long"))
  expect_equal(prop.table(table(x))[2], .5, ignore_attr = TRUE)
})

rn1 <- rename_cat_variables(mtcars, 
                     var_names = data.frame(old = c("wt", "mpg"), new = c("Weight", "Efficiency")),
                     level_names = data.frame(var = c("cyl", "cyl"), level_old = c(6, 8), level_new = c("many", "a lot"))
                                        )

test_that("rename_cat_variables works", {
  expect(all(c("Weight", "Efficiency") %in% names(rn1)), "Renamed columns missing")
  expect_equal(dim(mtcars), dim(rn1))
  expect_equal(table(rn1$cyl)["many"], 7, ignore_attr = TRUE)
})


# Copy-pasting somehow changes whitespace here - so that needs to be ignored for testing
tribble_code <- stringr::str_squish("
tibble::tribble(
  ~mpg,  ~cyl, ~disp, 
   21,    6,    160,  
   21,    6,    160,  
   22.8,  4,    108,  
   21.4,  6,    258,  
   18.7,  8,    360
)
")

test_that("to_tribble works", {
    expect_equal(stringr::str_squish(to_tribble(mtcars[1:5, 1:3])), tribble_code)
})

var_names <- stringr::str_squish('var_names <-  tibble::tribble(
  ~old,            ~new,            
  "Sepal.Length",  "Sepal.length", 
  "Sepal.Width",   "Sepal.width",  
  "Petal.Length",  "Petal.length", 
  "Petal.Width",   "Petal.width",  
  "Species",       "Species"
)')

level_names <- stringr::str_squish('level_names <-  tibble::tribble(
  ~var,       ~level_old,    ~level_new,    
   "Species",  "setosa",      "Setosa",     
   "Species",  "versicolor",  "Versicolor", 
   "Species",  "virginica",   "Virginica"
)')

test_that("get_rename_tribbles works", {
  expect_equal(stringr::str_squish(get_rename_tribbles(iris, show = FALSE)[[1]]), var_names)
  expect_equal(stringr::str_squish(get_rename_tribbles(iris, show = FALSE)[[2]]), level_names)
})


test_that("NA helper works", {
  expect_equal(na_share(c(1,2,3,NA), 1), .2)
  expect_equal(na_ifs(LETTERS, c("A", "D"))[1:4], c(NA, "B", "C", NA))
  expect_equal(na_when(x, x < 0, x > 2), c(NA, NA, NA, 0, 1, 2, NA))
  x <- -3:3
  expect_equal(na_when(x, x < 0, x %% 2 == 1, operator = "&"), c(NA, -2, NA, 0, 1, 2, 3))
})

test_that("named_list works", {
  name <- "Paul"
  age <- 10
  expect_names(names(named_list(name, age)), identical.to = c("name", "age"))
  expect_equal(named_list(name, age)$age, 10)
  expect_warning(name %>% named_list(age))
})
