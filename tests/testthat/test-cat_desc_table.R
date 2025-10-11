test_that("report_cat_vars works with basic inputs", {
  skip_if_not_installed("gt")

  result <- report_cat_vars(iris, Sepal.Length, Species)

  # Check output structure
  expect_type(result, "list")
  expect_named(result, c("descr", "tab", "html_code"))

  # Check descr tibble
  expect_s3_class(result$descr, "data.frame")
  expect_named(result$descr, c("group_var", "level", "N", "Share", "M", "SD", "letters"))
  expect_equal(nrow(result$descr), 3) # 3 species
  expect_equal(unique(result$descr$group_var), "Species")

  # Check that shares sum to 1
  expect_equal(sum(result$descr$Share), 1)

  # Check tab is gt object
  expect_s3_class(result$tab, "gt_tbl")

  # Check html_code is character
  expect_type(result$html_code, "character")
  expect_true(nchar(result$html_code) > 0)
})

test_that("report_cat_vars works without DV (dv = NULL)", {
  skip_if_not_installed("gt")

  result <- report_cat_vars(iris, NULL, Species)

  # Check output structure
  expect_type(result, "list")
  expect_named(result, c("descr", "tab", "html_code"))

  # After fix: descr is a proper data frame with only N and Share
  expect_s3_class(result$descr, "data.frame")
  expect_named(result$descr, c("group_var", "level", "N", "Share"))
  expect_equal(nrow(result$descr), 3)

  # Check that html_code doesn't contain M(SD) references
  expect_false(grepl("M \\(SD\\)", result$html_code, fixed = FALSE))
})

test_that("report_cat_vars works with multiple categorical variables", {
  skip_if_not_installed("gt")

  # Create test data with multiple categorical variables
  test_data <- data.frame(
    outcome = rnorm(100),
    cat1 = sample(c("A", "B"), 100, replace = TRUE),
    cat2 = sample(c("X", "Y", "Z"), 100, replace = TRUE)
  )

  result <- report_cat_vars(test_data, outcome, cat1, cat2)

  # Check that both variables are in descr
  expect_equal(sort(unique(result$descr$group_var)), c("cat1", "cat2"))

  # Check correct number of rows (2 levels for cat1 + 3 levels for cat2)
  expect_equal(nrow(result$descr), 5)
})

test_that("report_cat_vars handles both variable and level renaming", {
  skip_if_not_installed("gt")

  var_renames <- tibble::tribble(
    ~old,     ~new,
    "Species", "Type"
  )

  level_renames <- tibble::tribble(
    ~var,      ~level_old,  ~level_new,
    "Species", "setosa",    "Type A",
    "Species", "versicolor", "Type B",
    "Species", "virginica", "Type C"
  )

  result <- report_cat_vars(iris, Sepal.Length, Species,
                           var_names = var_renames, level_names = level_renames)

  expect_equal(unique(result$descr$group_var), "Type")
  expect_setequal(result$descr$level, c("Type A", "Type B", "Type C"))
})

test_that("report_cat_vars handles missing values in DV with na.rm = TRUE", {
  skip_if_not_installed("gt")

  test_data <- iris
  test_data$Sepal.Length[c(1, 5, 10)] <- NA

  result <- report_cat_vars(test_data, Sepal.Length, Species, na.rm = TRUE)

  # Should complete without error and calculate means
  expect_s3_class(result$descr, "data.frame")
  expect_false(any(is.na(result$descr$M)))
  expect_false(any(is.na(result$descr$SD)))
})

test_that("report_cat_vars handles missing values in DV with na.rm = FALSE", {
  skip_if_not_installed("gt")

  test_data <- iris
  test_data$Sepal.Length[1:10] <- NA

  result <- report_cat_vars(test_data, Sepal.Length, Species, na.rm = FALSE)

  # Should have NA means for setosa (first 50 rows include the NAs)
  expect_true(is.na(result$descr$M[result$descr$level == "setosa"]))
})

test_that("report_cat_vars handles missing values in categorical variable with exclude_na = TRUE", {
  skip_if_not_installed("gt")

  test_data <- iris
  test_data$Species[1:5] <- NA

  result <- report_cat_vars(test_data, Sepal.Length, Species, exclude_na = TRUE)

  # Should have 3 levels (NAs excluded)
  expect_equal(nrow(result$descr), 3)
  expect_false(any(is.na(result$descr$level)))
})

test_that("report_cat_vars handles missing values in categorical variable with exclude_na = FALSE", {
  skip_if_not_installed("gt")

  test_data <- iris
  test_data$Species[1:5] <- NA

  result <- report_cat_vars(test_data, Sepal.Length, Species, exclude_na = FALSE)

  # Should have 4 levels (including NA)
  expect_equal(nrow(result$descr), 4)
  expect_true(any(is.na(result$descr$level)))
})

test_that("report_cat_vars uses different p-value adjustment methods", {
  skip_if_not_installed("gt")

  result_holm <- report_cat_vars(iris, Sepal.Length, Species, p_adjust = "holm")
  result_bonf <- report_cat_vars(iris, Sepal.Length, Species, p_adjust = "bonferroni")
  result_none <- report_cat_vars(iris, Sepal.Length, Species, p_adjust = "none")

  # All should complete without error
  expect_s3_class(result_holm$descr, "data.frame")
  expect_s3_class(result_bonf$descr, "data.frame")
  expect_s3_class(result_none$descr, "data.frame")

  # Letters might differ based on adjustment method
  # For iris data with clear differences, all methods should show differences
  expect_true(all(c("a", "b", "c") %in% result_none$descr$letters))
  expect_true(all(c("a", "b", "c") %in% result_bonf$descr$letters))
  expect_true(all(c("a", "b", "c") %in% result_holm$descr$letters))

  # Check footnote mentions method
  expect_true(grepl("Holm", result_holm$html_code))
  expect_true(grepl("Bonferroni", result_bonf$html_code))
  expect_false(grepl("adjusted", result_none$html_code))
})

test_that("report_cat_vars respects alpha_level parameter", {
  skip_if_not_installed("gt")

  # Create data where groups are close but distinguishable
  set.seed(123)
  test_data <- data.frame(
    outcome = c(rnorm(50, 10, 1), rnorm(50, 10.3, 1)),
    group = rep(c("A", "B"), each = 50)
  )

  result_05 <- report_cat_vars(test_data, outcome, group, alpha_level = 0.05)
  result_00001 <- report_cat_vars(test_data, outcome, group, alpha_level = 0.00001)

  # Both should complete
  expect_s3_class(result_05$descr, "data.frame")
  expect_s3_class(result_00001$descr, "data.frame")

  # At alpha = 0.00001, groups should not be different
  expect_equal(length(unique(result_01$descr$letters)), 1)
})

test_that("report_cat_vars handles custom notes", {
  skip_if_not_installed("gt")

  custom_notes <- list("This is a custom note", "Another note")

  result <- report_cat_vars(iris, Sepal.Length, Species, notes = custom_notes)

  # Check that custom notes appear in HTML
  expect_true(grepl("This is a custom note", result$html_code))
  expect_true(grepl("Another note", result$html_code))
})

test_that("report_cat_vars handles custom dv_name", {
  skip_if_not_installed("gt")

  result <- report_cat_vars(iris, Sepal.Length, Species, dv_name = "Sepal Length (cm)")

  # Check that custom DV name appears in footnote
  expect_true(grepl("Sepal Length \\(cm\\)", result$html_code))
})

test_that("report_cat_vars respects bold_vars parameter", {
  skip_if_not_installed("gt")

  result_bold <- report_cat_vars(iris, Sepal.Length, Species, bold_vars = TRUE)
  result_not_bold <- report_cat_vars(iris, Sepal.Length, Species, bold_vars = FALSE)

  # Both should complete
  expect_s3_class(result_bold$tab, "gt_tbl")
  expect_s3_class(result_not_bold$tab, "gt_tbl")

  # Check for bold styling in HTML (simplified check)
  expect_true(grepl("bold", result_bold$html_code))
})

test_that("report_cat_vars respects apa_style parameter", {
  skip_if_not_installed("gt")

  result_apa <- report_cat_vars(iris, Sepal.Length, Species, apa_style = TRUE)
  result_no_apa <- report_cat_vars(iris, Sepal.Length, Species, apa_style = FALSE)

  # Both should complete
  expect_s3_class(result_apa$tab, "gt_tbl")
  expect_s3_class(result_no_apa$tab, "gt_tbl")
})

test_that("report_cat_vars handles css_tags parameter", {
  skip_if_not_installed("gt")

  css_tags <- list(".gt_table" = "border: 2px solid red")

  result <- report_cat_vars(iris, Sepal.Length, Species, css_tags = css_tags)

  # Check that CSS was added to HTML
  expect_true(grepl("border: 2px solid red", result$html_code))
})

test_that("report_cat_vars saves to file when filename provided", {
  skip_if_not_installed("gt")

  temp_file <- tempfile(fileext = ".html")

  result <- report_cat_vars(iris, Sepal.Length, Species, filename = temp_file)

  # Check that file was created
  expect_true(file.exists(temp_file))

  # Check that content is in file
  content <- readr::read_file(temp_file)
  expect_true(nchar(content) > 0)
  expect_true(grepl("Species", content))

  # Clean up
  unlink(temp_file)

  # Result should be returned invisibly
  expect_type(result, "list")
})

test_that("report_cat_vars handles single-level categorical variable", {
  skip_if_not_installed("gt")

  # Create data with only one level
  test_data <- data.frame(
    outcome = rnorm(50),
    cat = rep("A", 50)
  )

  # Should now work with the fix
  expect_warning(
    result <- report_cat_vars(test_data, outcome, cat),
    "Only one level found"
  )

  # Should work with single level
  expect_equal(nrow(result$descr), 1)
  expect_equal(result$descr$level, "A")
  expect_equal(result$descr$Share, 1)
  expect_equal(result$descr$letters, "a")
})

test_that("report_cat_vars handles groups with identical means", {
  skip_if_not_installed("gt")

  # Create data where all groups have same mean
  test_data <- data.frame(
    outcome = rep(5, 90),
    cat = rep(c("A", "B", "C"), each = 30)
  )

  result <- report_cat_vars(test_data, outcome, cat)

  # Should complete without error
  expect_s3_class(result$descr, "data.frame")
  expect_equal(nrow(result$descr), 3)

  # All groups should have same letter (no significant differences)
  expect_equal(length(unique(result$descr$letters)), 1)
  expect_equal(result$descr$letters[1], "a")
})

test_that("report_cat_vars handles very small sample sizes", {
  skip_if_not_installed("gt")

  # Create minimal data
  test_data <- data.frame(
    outcome = c(1, 2, 3, 4),
    cat = c("A", "A", "B", "B")
  )

  result <- report_cat_vars(test_data, outcome, cat)

  # Should complete without error
  expect_s3_class(result$descr, "data.frame")
  expect_equal(nrow(result$descr), 2)
  expect_equal(sum(result$descr$N), 4)
})

test_that("report_cat_vars validates input parameters", {
  skip_if_not_installed("gt")

  # Invalid p_adjust method
  expect_error(
    report_cat_vars(iris, Sepal.Length, Species, p_adjust = "invalid_method"),
    regexp = "p_adjust"
  )

  # Invalid alpha_level (not in 0-1 range)
  expect_error(
    report_cat_vars(iris, Sepal.Length, Species, alpha_level = 1.5),
    regexp = "alpha_level"
  )

  expect_error(
    report_cat_vars(iris, Sepal.Length, Species, alpha_level = -0.1),
    regexp = "alpha_level"
  )

  # Invalid bold_vars (not logical)
  expect_error(
    report_cat_vars(iris, Sepal.Length, Species, bold_vars = "yes"),
    regexp = "bold_vars"
  )

  # Invalid na.rm (not logical)
  expect_error(
    report_cat_vars(iris, Sepal.Length, Species, na.rm = "yes"),
    regexp = "na.rm"
  )

  # Invalid exclude_na (not logical)
  expect_error(
    report_cat_vars(iris, Sepal.Length, Species, exclude_na = "yes"),
    regexp = "exclude_na"
  )
})

test_that("report_cat_vars validates var_names structure", {
  skip_if_not_installed("gt")

  # Missing 'old' column
  bad_var_names <- tibble::tribble(
    ~new,
    "New Name"
  )

  expect_error(
    report_cat_vars(iris, Sepal.Length, Species, var_names = bad_var_names),
    regexp = "old"
  )

  # Missing 'new' column
  bad_var_names2 <- tibble::tribble(
    ~old,
    "Species"
  )

  expect_error(
    report_cat_vars(iris, Sepal.Length, Species, var_names = bad_var_names2),
    regexp = "new"
  )
})

test_that("report_cat_vars validates level_names structure", {
  skip_if_not_installed("gt")

  # Missing 'level_old' column
  bad_level_names <- tibble::tribble(
    ~var, ~level_new,
    "Species", "New Name"
  )

  expect_error(
    report_cat_vars(iris, Sepal.Length, Species, level_names = bad_level_names),
    regexp = "level_old"
  )

  # Missing 'level_new' column
  bad_level_names2 <- tibble::tribble(
    ~var, ~level_old,
    "Species", "setosa"
  )

  expect_error(
    report_cat_vars(iris, Sepal.Length, Species, level_names = bad_level_names2),
    regexp = "level_new"
  )
})

test_that("report_cat_vars validates css_tags structure", {
  skip_if_not_installed("gt")

  # Unnamed list
  expect_error(
    report_cat_vars(iris, Sepal.Length, Species, css_tags = list("border: 1px")),
    regexp = "css_tags"
  )
})

test_that("report_cat_vars output has correct class", {
  skip_if_not_installed("gt")

  result <- report_cat_vars(iris, Sepal.Length, Species)

  expect_true(inherits(result, "timesaveR_raw_html"))
})

test_that("report_cat_vars works with numeric categorical variable", {
  skip_if_not_installed("gt")

  test_data <- data.frame(
    outcome = rnorm(100),
    cat_num = sample(c(1, 2, 3), 100, replace = TRUE)
  )

  result <- report_cat_vars(test_data, outcome, cat_num)

  # Should convert numeric to character levels
  expect_s3_class(result$descr, "data.frame")
  expect_equal(nrow(result$descr), 3)
  expect_type(result$descr$level, "character")
})

test_that("report_cat_vars handles factors correctly", {
  skip_if_not_installed("gt")

  test_data <- iris
  test_data$Species <- as.factor(test_data$Species)

  result <- report_cat_vars(test_data, Sepal.Length, Species)

  # Should work with factors
  expect_s3_class(result$descr, "data.frame")
  expect_equal(nrow(result$descr), 3)
})

test_that("report_cat_vars handles ordered factors", {
  skip_if_not_installed("gt")

  test_data <- data.frame(
    outcome = rnorm(90),
    rating = ordered(rep(c("Low", "Medium", "High"), each = 30),
                    levels = c("Low", "Medium", "High"))
  )

  result <- report_cat_vars(test_data, outcome, rating)

  # Should work with ordered factors
  expect_s3_class(result$descr, "data.frame")
  expect_equal(nrow(result$descr), 3)

  # Levels should be in correct order
  expect_equal(result$descr$level, c("Low", "Medium", "High"))
})

test_that("report_cat_vars computes statistics correctly", {
  skip_if_not_installed("gt")

  # Create data with known statistics
  test_data <- data.frame(
    outcome = c(rep(10, 50), rep(20, 50)),
    group = rep(c("A", "B"), each = 50)
  )

  result <- report_cat_vars(test_data, outcome, group)

  # Check means
  expect_equal(result$descr$M[result$descr$level == "A"], 10)
  expect_equal(result$descr$M[result$descr$level == "B"], 20)

  # Check SDs (should be 0 for constant values)
  expect_equal(result$descr$SD[result$descr$level == "A"], 0)
  expect_equal(result$descr$SD[result$descr$level == "B"], 0)

  # Check Ns
  expect_equal(result$descr$N[result$descr$level == "A"], 50)
  expect_equal(result$descr$N[result$descr$level == "B"], 50)

  # Check shares
  expect_equal(result$descr$Share[result$descr$level == "A"], 0.5)
  expect_equal(result$descr$Share[result$descr$level == "B"], 0.5)
})

test_that(".add_css helper function works correctly", {
  skip_if_not_installed("gt")

  # Create simple HTML with CSS class
  html_code <- "<style>.test { color: blue }</style>"

  result <- timesaveR:::.add_css(html_code, ".test", "background: red")

  # Should add the new CSS property
  expect_true(grepl("background: red", result))
  expect_true(grepl("color: blue", result))
})
