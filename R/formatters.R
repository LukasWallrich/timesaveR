#' Round all numeric columns in dataframe
#'
#' Rounds all numeric columns in dataframe. By default, uses "round half to even"
#' (aka banker's rounding), but the rounding method can be changed via the
#' `timesaveR.round_method` option (see Details).
#'
#' @param df Dataframe to be rounded
#' @param digits Number of digits, defaults to 2
#' @return The input dataframe, with all numeric columns rounded to `digits` digits.
#' Non-numeric columns are returned unchanged.
#'
#' @details
#' The rounding method is controlled by the `timesaveR.round_method` option:
#' \itemize{
#'   \item `"to_even"` (default): Round half to even (banker's rounding)
#'   \item `"default"`: Standard R rounding (round half away from zero)
#' }
#' Set via `options(timesaveR.round_method = "default")` to change for your session.
#'
#' @source https://stackoverflow.com/questions/9063889/how-to-round-a-data-frame-in-r-that-contains-some-character-variables
#' @examples
#' round_df(mtcars[1:3, 1:3], digits = 1)
#' @export

round_df <- function(df, digits = 2) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[, nums] <- lapply(df[, nums, drop = FALSE], .round_dispatch, digits = digits)
  df
}

# Internal helper for round half to even
.round_half_to_even <- function(x, digits) {
  pow <- 10^digits
  x_shifted <- x * pow

  # Find values that are exactly at .5 (within floating point tolerance), excluding NAs
  frac <- abs(x_shifted - round(x_shifted))
  is_half <- abs(frac - 0.5) < 1e-10
  is_half[is.na(is_half)] <- FALSE

  rounded <- numeric(length(x))

  # For values exactly at .5, round to even
  if (any(is_half)) {
    floored <- floor(x_shifted[is_half])
    rounded[is_half] <- ifelse(floored %% 2 == 0, floored, floored + 1) / pow
  }

  # For all other values (excluding NAs), use standard rounding
  not_half_not_na <- !is_half & !is.na(x)
  if (any(not_half_not_na)) {
    rounded[not_half_not_na] <- round(x[not_half_not_na], digits)
  }

  rounded[is.na(x)] <- NA
  rounded
}

# Internal rounding dispatcher - respects package option
.round_dispatch <- function(x, digits) {
  method <- getOption("timesaveR.round_method", "to_even")
  if (method == "to_even") {
    .round_half_to_even(x, digits)
  } else if (method == "default") {
    round(x, digits)
  } else {
    cli::cli_warn("Invalid timesaveR.round_method option: {method}. Using 'to_even'.")
    .round_half_to_even(x, digits)
  }
}

#' Format p-value in line with APA standard (no leading 0)
#'
#' Formats p-value in line with APA standard, returning it without leading 0 and
#' as < .001 (or below the smallest value expressible with the given number of
#' significant digits) and > .99 when it is extremely small or large.
#'
#' Respects the `timesaveR.round_method` option for consistent rounding behavior
#' (see [round_()] for details).
#'
#' @param p_value Numeric, or a vector of numbers
#' @param digits Number of significant digits, defaults to 3
#' @param equal_sign Logical. Should *p*-values be prefixed with `=` unless they
#' are reported as `<` Defaults to TRUE, for use in text that reports result,
#' FALSE is particularly useful for tables.
#' @return A character vector of formatted *p*-values, the same length as `p_value`.
#' `NA` inputs return `NA_character_`.
#' @examples
#' fmt_p(0.04355)
#' fmt_p(0.0001)
#' fmt_p(c(0.04355, 0.0001, 0.654), equal_sign = FALSE)
#' @export

fmt_p <- function(p_value, digits = 3, equal_sign = TRUE) {

  # Allow bare logical NA (as produced by `NA`) to be treated as numeric,
  # consistent with fmt_pct()/round_() which accept it.
  if (is.logical(p_value) && all(is.na(p_value))) {
    p_value <- as.numeric(p_value)
  }

  assert_numeric(p_value)
  assert_integerish(digits)
  assert_logical(equal_sign)

  if (!is.numeric(p_value)) {
    cli::cli_abort("p_value input must be numeric.")
  }

  # Warn about invalid p-values
  if (any(p_value > 1 | p_value < 0, na.rm = TRUE)) {
    cli::cli_warn("p-values should be between 0 and 1. Invalid values detected.")
  }

  lower_cutoff <- 10^(-digits)
  # The upper cutoff is .99 by default (as before), but for low `digits` (e.g.
  # digits = 1) that fixed cutoff is coarser than the rounding step itself, so
  # values below .99 could still round up to display as "1.0". Scaling the
  # cutoff down to 1 - 10^(-digits) whenever that is stricter than .99 avoids
  # that, while leaving the default `digits = 3` behaviour unchanged (.99).
  upper_cutoff <- min(0.99, 1 - 10^(-digits))

  fmt <- paste0("%.", digits, "f")
  fmt_p <- function(x) {
    # Round using dispatcher first
    rounded <- .round_dispatch(x, digits)
    paste0(if(equal_sign == TRUE) "= " else "", sprintf(fmt, rounded)) %>%
      stringr::str_replace("0.", ".")
  }
  exact <- !(p_value < lower_cutoff | p_value > upper_cutoff) & p_value >= 0 & p_value <= 1
  exact[is.na(exact)] <- TRUE
  out <- p_value
  out[exact] <- purrr::map_chr(out[exact], fmt_p)
  small_threshold <- paste0(if(equal_sign == TRUE) "< " else "< ",
                           sprintf(paste0("%.", digits, "f"), lower_cutoff) %>%
                           stringr::str_replace("0.", "."))
  out[p_value < lower_cutoff & p_value >= 0] <- small_threshold
  # Trim the upper cutoff to the minimal number of decimals needed (no
  # trailing zeroes), so e.g. .99 stays "> .99" and .9 becomes "> .9".
  upper_txt <- sub("0+$", "", sprintf("%.10f", upper_cutoff))
  upper_txt <- sub("\\.$", "", upper_txt)
  upper_txt <- sub("^0\\.", ".", upper_txt)
  large_threshold <- paste0("> ", upper_txt)
  large <- p_value > upper_cutoff & p_value <= 1
  out[large] <- large_threshold
  out[p_value > 1] <- "> 1 (!!)"
  out[p_value < 0] <- "< 0 (!!)"
  out[is.na(p_value)] <- NA
  attributes(out) <- attributes(p_value)
  out
}


#' Format fraction as percentage string
#'
#' Formats fractions as percentages, i.e. multiplying them by 100 and adding '%'.
#' Respects the `timesaveR.round_method` option for consistent rounding behavior
#' (see [round_()] for details).
#'
#' @param x Numeric, or a vector of numbers
#' @param digits Number of significant digits, defaults to 1
#' @return A character vector of formatted percentages, the same length as `x`.
#' `NA` values return `NA_character_`.
#' @examples
#' fmt_pct(0.127)
#' fmt_pct(c(0.127, 0.456, 0.789), digits = 2)
#' @export

fmt_pct <- function(x, digits = 1) {
  assert_numeric(x)

  # Round using dispatcher first
  rounded <- .round_dispatch(x * 100, digits)

  fmt <- paste0("%1.", digits, "f%%")
  out <- sprintf(fmt, rounded)
  attributes(out) <- attributes(x)
  out[is.na(x)] <- NA
  out
}

#' Format number as correlation coefficient
#'
#' Format numbers as correlation coefficients, by rounding them and removing the leading 0.
#' Respects the `timesaveR.round_method` option for consistent rounding behavior
#' (see [round_()] for details).
#'
#' @param cor_value Numeric, or a vector of numbers
#' @param digits Number of significant digits, defaults to 2
#' @return A character vector of formatted correlation coefficients (without leading
#' zero), the same length as `cor_value`. `NA` values return `NA_character_`.
#' @examples
#' fmt_cor(0.127)
#' fmt_cor(c(0.456, -0.789, 0.023))
#' @export

fmt_cor <- function(cor_value, digits = 2) {
  qassert(cor_value, "r[-1, 1]")

  # Round using dispatcher first
  rounded <- .round_dispatch(cor_value, digits)

  fmt <- paste0("%.", digits, "f")
  out <- sprintf(fmt, rounded) %>%
    stringr::str_replace(stringr::fixed("0."), ".")
  attributes(out) <- attributes(cor_value)
  out[is.na(cor_value)] <- NA
  out
}

#' Format confidence interval based on the bounds
#'
#' Constructs a confidence intervals from upper and lower bounds,
#' placing them in between square brackets
#'
#' @param lower Lower bound(s) of confidence interval(s). Numeric, or a vector of numbers
#' @param upper Lower bound(s) of confidence interval(s). Numeric, or a vector of numbers
#' @param digits Number of significant digits, defaults to 2
#' @param drop_0 Logical. Should leading 0 be dropped, e.g., when reporting correlation coefficients.
#' Note that this only works when all values are between -1 and 1 (inclusive)
#' @return A character vector of formatted confidence intervals, e.g. `"[1.23, 2.68]"`,
#' the same length as `lower`/`upper`. `NA` is returned where either bound is `NA`.
#' @examples
#' fmt_ci(1.23, 2.68)
#' fmt_ci(c(0.12, 0.45), c(0.34, 0.67), drop_0 = TRUE)
#' @export

fmt_ci <- function(lower, upper, digits = 2, drop_0 = FALSE) {
  assert_numeric(lower)
  assert_numeric(upper)
  if (!(length(lower) == length(upper))) cli::cli_abort("lower and upper must have the same length.")
  if (any(lower > upper, na.rm = TRUE)) cli::cli_abort("All values in lower must be smaller than the corresponding values in upper.")
  assert_count(digits)
  if (drop_0) {
    if (!(test_numeric(lower, lower = -1, upper = 1) & test_numeric(upper, lower = -1, upper = 1))) {
      cli::cli_abort("drop_0 can only be TRUE if both lower and upper do not contain any values greater than 1 or lower than -1.")
    }
    out <- paste0("[", fmt_cor(lower, digits), ", ", fmt_cor(upper, digits), "]")
    
  } else {
    out <- paste0("[", round_(lower, digits), ", ", round_(upper, digits), "]")
  }
  out[is.na(lower) | is.na(upper)] <- NA
  out
}

#' Round function that returns trailing zeroes
#'
#' Particularly when creating tables, it is often desirable to keep
#' all numbers to the same width. `round()` and similar functions drop
#' trailing zeros - this version keeps them and thus rounds 1.201 to 1.20
#' rather than 1.2 when 2 digits are requested.
#'
#' By default, uses "round half to even" (banker's rounding), but the rounding
#' method can be changed via the `timesaveR.round_method` option (see Details).
#'
#' @param x Numeric vector to be rounded
#' @param digits Number of significant digits
#' @return Character vector of rounded values, with trailing zeroes as needed to show `digits` figures after the decimal point
#'
#' @details
#' The rounding method is controlled by the `timesaveR.round_method` option:
#' \itemize{
#'   \item `"to_even"` (default): Round half to even (banker's rounding)
#'   \item `"default"`: Standard R rounding (round half away from zero)
#' }
#' Set via `options(timesaveR.round_method = "default")` to change for your session.
#'
#' @examples
#' round_(1.201, 2)
#' round_(c(1.2, 3.456, 7.8), digits = 2)
#' @export

round_ <- function(x, digits = 2) {
  checkmate::assert_numeric(x)
  checkmate::assert_integerish(digits)

  # Use dispatcher to respect package option
  rounded <- .round_dispatch(x, digits)

  # Format with trailing zeros
  fmt <- paste0("%.", digits, "f")
  out <- sprintf(fmt, rounded)
  attributes(out) <- attributes(x)
  out[is.na(x)] <- NA
  out
}
