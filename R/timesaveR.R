
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

  knitr::asis_output(res)
}
