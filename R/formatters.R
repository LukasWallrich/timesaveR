#' Round all numeric columns in dataframe
#'
#' Rounds all numeric columns in dataframe, using "round half to even" (aka banker's rounding)
#'
#' @param df Dataframe to be rounded
#' @param digits Number of digits, defaults to 2
#' @source https://stackoverflow.com/questions/9063889/how-to-round-a-data-frame-in-r-that-contains-some-character-variables
#' @export

round_df <- function(df, digits = 2) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[, nums] <- lapply(df[, nums, drop = FALSE], .round_half_to_even, digits = digits)
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

#' Format p-value in line with APA standard (no leading 0)
#'
#' Formats p-value in line with APA standard, returning it without leading 0 and
#' as < .001 (or below the smallest value expressible with the given number of
#' significant digits) and > .99 when it is extremely small or large.
#'
#' @param p_value Numeric, or a vector of numbers
#' @param digits Number of significant digits, defaults to 3
#' @param equal_sign Logical. Should *p*-values be prefixed with `=` unless they
#' are reported as `<` Defaults to TRUE, for use in text that reports result,
#' FALSE is particularly useful for tables.
#' @examples
#' fmt_p(0.04355)
#' fmt_p(0.0001)
#' fmt_p(c(0.04355, 0.0001, 0.654), equal_sign = FALSE)
#' @export

fmt_p <- function(p_value, digits = 3, equal_sign = TRUE) {

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

  fmt <- paste0("%.", digits, "f")
  fmt_p <- function(x) {
    paste0(if(equal_sign == TRUE) "= " else "", sprintf(fmt, x)) %>%
      stringr::str_replace("0.", ".")
  }
  exact <- !(p_value < 10^(-digits) | p_value > .99) & p_value >= 0 & p_value <= 1
  exact[is.na(exact)] <- TRUE
  out <- p_value
  out[exact] <- purrr::map_chr(out[exact], fmt_p)
  small_threshold <- paste0(if(equal_sign == TRUE) "< " else "< ",
                           sprintf(paste0("%.", digits, "f"), 10^(-digits)) %>%
                           stringr::str_replace("0.", "."))
  out[p_value < 10^(-digits) & p_value >= 0] <- small_threshold
  large <- p_value > .99 & p_value <= 1
  out[large] <- "> .99"
  out[p_value > 1] <- "> 1 (!!)"
  out[p_value < 0] <- "< 0 (!!)"
  out[is.na(p_value)] <- NA
  attributes(out) <- attributes(p_value)
  out
}


#' Format fraction as percentage string
#'
#' Formats fractions as percentages, i.e. multiplying them by 100 and adding '%'
#'
#' @param x Numeric, or a vector of numbers
#' @param digits Number of significant digits, defaults to 1
#' @examples
#' fmt_pct(0.127)
#' fmt_pct(c(0.127, 0.456, 0.789), digits = 2)
#' @export

fmt_pct <- function(x, digits = 1) {
  assert_numeric(x)
  fmt <- paste0("%1.", digits, "f%%")
  out <- sprintf(fmt, x * 100)
  attributes(out) <- attributes(x)
  out[is.na(x)] <- NA
  out
}

#' Format number as correlation coefficient
#'
#' Format numbers as correlation coefficients, by rounding them and removing the leading 0
#'
#' @param cor_value Numeric, or a vector of numbers
#' @param digits Number of significant digits, defaults to 2
#' @examples
#' fmt_cor(0.127)
#' fmt_cor(c(0.456, -0.789, 0.023))
#' @export

fmt_cor <- function(cor_value, digits = 2) {
  qassert(cor_value, "r[-1, 1]")
  fmt <- paste0("%.", digits, "f")
  out <- sprintf(fmt, cor_value) %>%
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
#' @param x Numeric vector to be rounded
#' @param digits Number of significant digits
#' @return Character vector of rounded values, with trailing zeroes as needed to show `digits` figures after the decimal point
#' @examples
#' round_(1.201, 2)
#' round_(c(1.2, 3.456, 7.8), digits = 2)
#' @export

round_ <- function(x, digits = 2) {
  checkmate::assert_numeric(x)
  checkmate::assert_integerish(digits)

  # Use internal helper for proper round half to even
  rounded <- .round_half_to_even(x, digits)

  # Format with trailing zeros
  fmt <- paste0("%.", digits, "f")
  out <- sprintf(fmt, rounded)
  attributes(out) <- attributes(x)
  out[is.na(x)] <- NA
  out
}
