
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Note re timesaveR: Many functions in this package are alpha-versions - please treat results with care and report bugs and desired features.")
}

.onLoad <- function(...) {
  registerMethods(list(
    # c(package, genname, class)
    c("knitr", "knit_print", "timesaveR_raw_html")
  ))
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

#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @importFrom stats as.formula cor.test sd t.test lm p.adjust.methods
#' @importFrom generics tidy
#' @importFrom generics glance
#' @importFrom here here
#' @importFrom utils getFromNamespace
#' @import checkmate
#' @export
generics::tidy
generics::glance
#'
#'
"_PACKAGE"

.check_req_packages <- function(x, note = "") {
  res <- suppressWarnings(lapply(x, requireNamespace, quietly = TRUE)) %>% unlist()
  if (!all(res)) {
    if (!interactive()) {
      stop(paste0(note, "Some required packages are not installed. Make sure you have
               these packages: ", paste0(x[!res], collapse = ", ")),
        call. = FALSE
      )
    }
    op <- options("warn")
    on.exit(options(op))
    options(warn = 1)
    warning(paste0(note, "The following packages are required for this function but
                   cannot be loaded: ", paste0(x[!res], collapse = ", ")),
      call. = FALSE
    )
    choice <- readline(prompt = "Should I try to install these packages? (Y/N)")
    if (choice %in% c("Y", "y")) {
      utils::install.packages(x[!res])
      res <- suppressWarnings(lapply(x, requireNamespace, quietly = TRUE)) %>% unlist()
      if (!all(res)) {
        stop("Not all packages could be installed successfully. The following could still not be loaded: ", paste0(x[!res], collapse = ", "),
          call. = FALSE
        )
      }
      return(TRUE)
    }
    stop("Cannot proceed without these packages.", call. = FALSE)
  }
}

#' Sample data: health data from the European Social Survey Wave 7
#'
#' This dataset contains data from the ESS Wave 7 (2014) for Germany,
#' the UK and France that describes respondents physical and mental
#' health as well as specific health behaviours. It can be used to
#' demonstrate most functions of this package.
#'
#' @format A dataframe with 7,226 rows and 23 columns
#' \describe{
#'  \item{cntry}{Country}
#'  \item{gndr}{Gender (1 = male, 2 = female)}
#'  \item{agea}{Age}
#'  \item{eisced}{Education}
#'  \item{pweight}{Population weight - adjust for different population sizes between countries}
#'  \item{pspwght}{Post-stratification weight - adjusts for bias in national samples due to sampling error and non-response (considering gender, age, education and region) AND for unequal inclusion probabilities due to survey design}
#'  \item{health}{Overall health, from 1 = very good to 5 = very bad}
#'  \item{height}{Height (cm)}
#'  \item{weight}{Weight (kg)}
#'  \item{icbrnct}{Respondent born in country, 1 = yes, 2 = no}
#'  \item{etfruit}{How often eat fruit, excluding drinking juice, from 1 = three times or more per day to 7 = never}
#'  \item{eatveg }{How often eat vegetables or salad, excluding potatoes,  from 1 = three times or more per day to 7 = never}
#'  \item{dosprt }{Do sports or other physical activity, how many of last 7 days}
#'  \item{cgtsmke}{Cigarettes smoking behaviour, from 1 = I smoke daily to 5 = I have never smoked}
#'  \item{alcfreq}{How often drink alcohol, from 1 = every day to 5  = never}
#'  \item{fltdpr}{Felt depressed in past week, from 1 = none/almost none of the time to 4 = all/almost all of the time (same for next 7 variables)}
#'  \item{flteeff}{Felt everything did as effort}
#'  \item{slprl}{Sleep was restless}
#'  \item{wrhpp}{Were happy}
#'  \item{fltlnl}{Felt lonely}
#'  \item{enjlf}{Enjoyed life}
#'  \item{fltsd}{Felt sad}
#'  \item{cldgng}{Could not get going}
#' }
#'
#' @name ess_health
#' @docType data
#' @author ESS-ERIC, selected by package author
#' @references \url{https://www.europeansocialsurvey.org/data/download.html?r=7}
#' @keywords data
#'
NULL


# Code from htmltools package -
# https://github.com/rstudio/htmltools/blob/42712f7f1ba560faf16fe1ee709afb328996bb81/R/zzz.R

# Reusable function for registering a set of methods with S3 manually. The
# methods argument is a list of character vectors, each of which has the form
# c(package, genname, class).
registerMethods <- function(methods) {
  lapply(methods, function(method) {
    pkg <- method[[1]]
    generic <- method[[2]]
    class <- method[[3]]
    func <- get(paste(generic, class, sep = "."))
    if (pkg %in% loadedNamespaces()) {
      registerS3method(generic, class, func, envir = asNamespace(pkg))
    }
    setHook(
      packageEvent(pkg, "onLoad"),
      function(...) {
        registerS3method(generic, class, func, envir = asNamespace(pkg))
      }
    )
  })
}

#' Renders HTML code for Viewer pane
#'
#' Various functions in this package return HTML code, mostly for tables. This
#' function allows for them to rendered and shown in the Viewer. It can also be
#' called manually to render a character vector x that contains HTML code.
#'
#' @param x Either a character vector containing HTML code
#' or a list with a html_code element
#' @inheritDotParams print
#' @export

print.timesaveR_raw_html <- function(x, ...) {
  if (interactive() && length(as.environment("tools:rstudio")$.rs.S3Overrides) > 0L) {
    knit_print.timesaveR_raw_html(x)
  } else {
    if ("html_code" %in% names(x)) {
      res <- x$html_code
    } else {
      res <- x
    }

    if (interactive()) {
      tempDir <- tempfile()
      dir.create(tempDir)
      htmlFile <- file.path(tempDir, "index.html")
      writeLines(res, htmlFile)
      viewer <- getOption("viewer")
      viewer(htmlFile)
    } else {
      cat(res, "\n", sep = "")
      invisible(res)
    }
  }
}

#' @exportS3Method knitr::knit_print timesaveR_raw_html
knit_print.timesaveR_raw_html <- function(x, ...) {
  # if (is_html_output) {
  if ("html_code" %in% names(x)) {
    res <- x$html_code
  } else {
    res <- x
  }
  res <- stringr::str_remove(res, "<!DOCTYPE html>")
  res <- stringr::str_remove(res, "<html>")
  res <- stringr::str_remove(res, "</html>")
  browser()
  knitr::asis_output(x)


  # knitr::asis_output(res)
  # } else {
  #
  # }
}
