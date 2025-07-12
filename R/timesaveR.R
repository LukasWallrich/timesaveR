
.onAttach <- function(libname, pkgname) {
  cli::cli_inform("Note re timesaveR: Many functions in this package are alpha-versions - please treat results with care and report bugs and desired features.")
}

#' Functions to Accelerate (Academic) Data Analysis and Reporting
#'
#' Functions and templates in this package facilitate common tasks
#' in the course of most research projects in social psychology and related
#' fields. These include creating scales, reporting descriptive statistics,
#' correlations and distributions, reporting linear regression models with
#' standardized coefficients and F-change comparisons, as well as plotting
#' mediation models.
#' @docType package
#' @name timesaveR

"_PACKAGE"

globalVariables(".")

#' @importFrom magrittr %<>%
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @importFrom stats as.formula cor.test sd t.test lm p.adjust.methods quantile coef
#' @importFrom here here
#' @importFrom utils getFromNamespace
#' @import checkmate
#' @importFrom generics tidy
# ## broom needed to get tidy.lm, tidy.polr etc
#' @importFrom broom tidy 
#' @importFrom generics glance
#' @export
broom::tidy #with generics::tidy, model summary tables fail
generics::glance

.check_req_packages <- function(x, note = "") {
  res <- suppressWarnings(lapply(x, requireNamespace, quietly = TRUE)) %>% unlist()
  if (!all(res)) {
    if (!interactive()) {
      cli::cli_abort(c(
        "{note}Some required packages are not installed.",
        "Make sure you have these packages: {paste0(x[!res], collapse = ', ')}"
      ))
    }
    op <- options("warn")
    on.exit(options(op))
    options(warn = 1)
    cli::cli_warn(c(
      "{note}The following packages are required for this function but cannot be loaded:",
      "{paste0(x[!res], collapse = ', ')}"
    ))
    choice <- readline(prompt = "Should I try to install these packages? (Y/N)")
    if (choice %in% c("Y", "y")) {
      utils::install.packages(x[!res])
      res <- suppressWarnings(lapply(x, requireNamespace, quietly = TRUE)) %>% unlist()
      if (!all(res)) {
        cli::cli_abort(c(
          "Not all packages could be installed successfully.",
          "The following could still not be loaded: {paste0(x[!res], collapse = ', ')}"
        ))
      }
      return(TRUE)
    }
    cli::cli_abort("Cannot proceed without these packages.")
  }
}


#' Renders HTML code for Viewer pane
#'
#' Various functions in this package return HTML code, mostly for tables. This
#' function allows for them to rendered and shown in the Viewer. It can also be
#' called manually to render a character vector x that contains HTML code.
#'
#' @param x Either a character vector containing HTML code
#' or a list with a html_code element
#' @param ... Additional arguments passed to print.
#' @exportS3Method print timesaveR_raw_html

print.timesaveR_raw_html <- function(x, ...) {
  if ("html_code" %in% names(x)) {
    res <- x$html_code
  } else {
    res <- x
  }
  if (interactive()) {
    suppressMessages(print(htmltools::browsable(htmltools::HTML(res)))) 
  } else (
    suppressMessages(print(htmltools::HTML(res))) 
  )
}

#' Knitr S3 method to print tables
#'
#' These S3 methods are necessary to allow custom tables to print themselves in
#' knitr/rmarkdown documents.
#'
#' @param x Object to knit_print
#' @param ... Additional knit_print arguments
#' @exportS3Method knitr::knit_print timesaveR_raw_html

knit_print.timesaveR_raw_html <- function(x, ...) {
  if ("html_code" %in% names(x)) {
    res <- x$html_code
  } else {
    res <- x
  }
  res <- stringr::str_remove(res, "<!DOCTYPE html>")
  res <- stringr::str_remove(res, "<html>")
  res <- stringr::str_remove(res, "</html>")
  
  structure(res, class = "knit_asis")
}
