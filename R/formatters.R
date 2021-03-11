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
#' as < .001 when it is that small.
#'
#' @param p_value Numeric, or a vector of numbers
#' @param digits Number of significant digits, defaults to 3
#' @export

fmt_p <- function(p_value, digits = 3) {
  fmt <- paste0("%.", digits, "f")
  fmt_p <- function(x) {
    paste0("= ", sprintf(fmt, x)) %>%
      stringr::str_replace(" 0.", " .")
  }
  exact <- ifelse(p_value < .001, FALSE, TRUE)
  exact[is.na(exact)] <- TRUE
  out <- p_value
  out[exact] <- purrr::map_chr(out[exact], fmt_p)
  out[!exact] <- "< .001"
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
#' @param x Numeric, or a vector of numbers
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
#' Rounds numbers to consistent length and place in square brackets.
#'
#' @param x Numeric, or a vector of numbers
#' @param digits Number of significant digits, defaults to 2
#' @export

fmt_ci <- function(lower, upper, digits = 2) {
  assert_numeric(lower)
  assert_numeric(upper)
  assert_count(digits)
  out <- paste0("[", round_(lower, digits), ", ", round_(upper, digits), "]")
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