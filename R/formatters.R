#' Round all numeric columns in dataframe
#'
#' Rounds all numeric columns in dataframe, using the R default "half to even"
#'
#' @param df Dataframe to be rounded
#' @param digits Number of digits, defaults to 2
#' @source https://stackoverflow.com/questions/9063889/how-to-round-a-data-frame-in-r-that-contains-some-character-variables
#' @export


round_df <- function(df, digits = 2) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[, nums] <- round(df[, nums], digits = digits)
  df
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
#' @export

fmt_p <- function(p_value, digits = 3, equal_sign = TRUE) {
  fmt <- paste0("%.", digits, "f")
  fmt_p <- function(x) {
    paste0(if(equal_sign == TRUE) "= " else "", sprintf(fmt, x)) %>%
      stringr::str_replace("0.", ".")
  }
  exact <- !(p_value < 10^(-digits) | p_value > .99)
  exact[is.na(exact)] <- TRUE
  out <- p_value
  out[exact] <- purrr::map_chr(out[exact], fmt_p)
  out[!exact] <- paste("<", sprintf(paste0("%.", digits, "f"), 10^(-digits)) %>%
                         stringr::str_replace("0.", "."))
  large <- p_value > .99
  out[large] <- "> .99"
  out[p_value > 1] <- "> 1 (!!)"
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
#' Note that this only makes sense (and only works) when the values are between -1 and 1
#' 
#' @export

fmt_ci <- function(lower, upper, digits = 2, drop_0 = FALSE) {
  assert_numeric(lower)
  assert_numeric(upper)
  if (any(lower > upper)) stop("All values in lower must be smaller than the corresponding values in upper.")
  if (!(length(lower) == length(upper))) stop("lower and upper must have the same length.")
  assert_count(digits)
  if (drop_0) {
    if (!(test_numeric(lower, lower = -(1-5/10^(digits+1)), upper = (1-5/10^(digits+1))) & test_numeric(upper, lower = -(1-5/10^(digits+1)), upper = (1-5/10^(digits+1))))) {
      stop("drop_0 can only be TRUE if both lower and upper do not contain any values greater than 1 (or lower than -1), or values that would be rounded to +/-1 with the given digits.")
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
#' @export

round_ <- function(x, digits = 2) {
  checkmate::assert_numeric(x)
  checkmate::assert_integerish(digits)
  fmt <- paste0("%.", digits, "f")
  out <- sprintf(fmt, x)
  attributes(out) <- attributes(x)
  out[is.na(x)] <- NA
  out
}
