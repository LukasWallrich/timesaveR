test_that("significance stars work", {
  expect_equal(sigstars(c(0, .001, .01, .05, .1, 2)), c("***", "**", "*", "&dagger;", "", ""))
})

test_that("significance stars can be padded", {
  expect_equal(sigstars(c(0, .001, .01, .05, .1, 2), pad_html = TRUE), 
               c("***", "**&nbsp;", "*&nbsp;&nbsp;", "&dagger;&nbsp;&nbsp;", "&nbsp;&nbsp;&nbsp;", "&nbsp;&nbsp;&nbsp;"))
})

test_that("significance stars padding with ns values", {
  result <- sigstars(c(0, .1, 2), pad_html = TRUE, ns = TRUE)
  expect_true(all(stringr::str_detect(result, "&nbsp;") | result == "***"))
  expect_true(stringr::str_detect(result[3], "ns"))
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
    line_to_vector("1 2 3", strings = FALSE),
    "c(1, 2, 3)"
  )
  expect_equal(
    line_to_vector("1 2 3", strings = TRUE),
    'c("1", "2", "3")'
  )
  expect_equal(
    line_to_vector("1 2 3", return = "v"),
    c(1, 2, 3)
  )
})


test_that("Function handles different separators", {
  expect_equal(
    line_to_vector("0 a,b\nc\td", separators = "all", return = "vector"),
    c("0", "a", "b", "c", "d")
  )
  expect_equal(
    line_to_vector("a,b\nc\td", separators = "top-level", return = "vector"),
    c("a,b", "c\td")
  )
  expect_equal(
    line_to_vector("a,b\nc\td", separators = c(",", "\t"), return = "vector"),
    c("a", "b\nc", "d")
  )
})


test_that("Function handles keep_blank_as_na correctly", {
  expect_equal(
    line_to_vector("a  b   c", keep_blank_as_na = TRUE, separators = "all", return = "vector"),
    c("a", NA, "b", NA, NA, "c")
  )
  expect_equal(
    line_to_vector("a  b   c", keep_blank_as_na = FALSE, separators = "all", return = "vector"),
    c("a", "b", "c")
  )
})



test_that("Function stops with appropriate error when separators are invalid", {
  expect_error(
    line_to_vector("a b c", separators = "invalid_separator"),
  )
})



test_that("Function handles commas and spaces together correctly", {
  expect_equal(
    line_to_vector("a,b,c", return = "vector"),
    c("a", "b", "c")
  )
  expect_equal(
    line_to_vector("a, b, c", return = "vector"),
    c("a", "b", "c")
  )
})

set.seed(300688)
x <- cut_p(iris$Sepal.Length, p = c(.25, .50, .25), fct_levels = c("short", "middling", "long"), verbose = FALSE)

test_that("cut_p works", {
  expect_equal(levels(x), c("short", "middling", "long"))
  expect_equal(prop.table(table(x))[2], .5, ignore_attr = TRUE)
})

test_that("cut_p works with different tie methods", {
  set.seed(123)
  x_random <- cut_p(c(1, 1, 2, 2, 3, 1, 1), p = c(.4, .6), ties.method = "random", verbose = FALSE)
  expect_equal(length(levels(x_random)), 2)
  
  x_in_order <- cut_p(c(1, 1, 2, 2, 3, 1, 1), p = c(.4, .6), ties.method = "in_order", verbose = FALSE)
  expect_equal(length(levels(x_in_order)), 2)
  
  expect_true(
    any(as.character(x_random) != as.character(x_in_order)),
    "Random and in_order tie methods should yield different results"
  )
  
  expect_error(cut_p(c(1, 2, 3), p = c(.5, .5), ties.method = "invalid"))
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
  x <- -3:3
  expect_equal(na_when(x, x < 0, x > 2), c(NA, NA, NA, 0, 1, 2, NA))
  expect_equal(na_when(x, x < 0, x %% 2 == 1, operator = "&"), c(NA, -2, NA, 0, 1, 2, 3))
})

test_that("named_list works", {
  name <- "Paul"
  age <- 10
  expect_names(names(named_list(name, age)), identical.to = c("name", "age"))
  expect_equal(named_list(name, age)$age, 10)
  expect_warning(name %>% named_list(age))
})

test_that("paste_ function works as expected", {
  expect_equal(paste_("hello", NA, "you"), "hello you")
  expect_equal(paste_("", NA, "you"), "you")
  expect_equal(paste_(NA, NA, NA), "")
  expect_equal(paste_(c("hello", "world"), c(NA, "everyone"), c("you", NA)), c("hello you", "world everyone"))
  expect_equal(paste_(1, 2, NA, 4), "1 2 4")
  expect_equal(paste_("", "", ""), "")
  expect_equal(paste_(c("hello", NA, "world"), collapse = ", "), "hello, world")
  expect_equal(paste_("hello", NA, "world", sep = "-"), "hello-world")
  expect_equal(paste_("hello", c(NA, "world")), c("hello", "hello world"))
  expect_equal(paste_(), "")
})

# Tests for scale_blank and scale_weighted ----

test_that("scale_blank works with basic input", {
  x <- c(1, 2, 3, 4, 5)
  result <- scale_blank(x)

  expect_type(result, "double")
  expect_length(result, 5)
  expect_equal(mean(result), 0, tolerance = 1e-10)
  expect_equal(sd(result), 1, tolerance = 1e-10)
})

test_that("scale_blank handles zero variance", {
  x <- rep(5, 10)
  result <- scale_blank(x)

  expect_type(result, "double")
  expect_length(result, 10)
  expect_true(all(result == 0))
})

test_that("scale_blank handles all NA input", {
  x <- rep(NA_real_, 10)
  result <- scale_blank(x)

  expect_type(result, "double")
  expect_length(result, 10)
  expect_true(all(is.na(result)))
})

test_that("scale_blank respects center and scale parameters", {
  x <- c(10, 20, 30, 40, 50)

  # Center only
  result_center <- scale_blank(x, center = TRUE, scale = FALSE)
  expect_equal(mean(result_center), 0, tolerance = 1e-10)
  expect_false(isTRUE(all.equal(sd(result_center), 1)))

  # Neither center nor scale
  result_none <- scale_blank(x, center = FALSE, scale = FALSE)
  expect_equal(result_none, x)
})

test_that("scale_weighted works with basic input", {
  set.seed(123)
  x <- c(1, 2, 3, 4, 5)
  w <- c(1, 1, 1, 1, 1)  # Equal weights
  result <- scale_weighted(x, w)

  expect_type(result, "double")
  expect_length(result, 5)
  # With equal weights, should be similar to regular standardization
  expect_true(abs(mean(result[!is.na(result)])) < 1e-10)
})

test_that("scale_weighted handles unequal weights", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(2, 1, 1, 1, 1)  # Higher weight on first value

  # Weighted mean = (1*2 + 2 + 3 + 4 + 5) / (2 + 1 + 1 + 1 + 1) = 16 / 6 ≈ 2.667
  # Weighted variance = sum(w * (x - mean_w)^2) / sum(w) = 13.33 / 6 = 2.222
  # Weighted SD = sqrt(2.222) ≈ 1.491
  #
  # Standardized values: (x - 2.667) / 1.491
  expected <- c(-1.118, -0.447, 0.224, 0.894, 1.565)

  result <- scale_weighted(x, w)

  expect_type(result, "double")
  expect_length(result, 5)
  expect_true(all(!is.na(result)))
  expect_equal(result, expected, tolerance = 1e-3)
})

test_that("scale_weighted errors on length mismatch", {
  x <- c(1, 2, 3)
  w <- c(1, 1)

  expect_error(
    scale_weighted(x, w),
    "must have the same length"
  )
})

test_that("scale_weighted handles NA values", {
  x <- c(1, 2, NA, 4, 5)
  w <- c(1, 1, 1, NA, 1)

  result <- scale_weighted(x, w, na.rm = TRUE)

  expect_type(result, "double")
  expect_length(result, 5)
  # NA positions should remain NA
  expect_true(is.na(result[3]))
  expect_true(is.na(result[4]))
})

test_that("scale_weighted handles zero variance", {
  x <- rep(5, 10)
  w <- rep(1, 10)

  result <- scale_weighted(x, w)

  expect_type(result, "double")
  expect_length(result, 10)
  expect_true(all(result == 0))
})

test_that("scale_weighted handles all NA input", {
  x <- rep(NA_real_, 5)
  w <- rep(1, 5)

  result <- scale_weighted(x, w)

  expect_length(result, 5)
  expect_true(all(is.na(result)))
})
