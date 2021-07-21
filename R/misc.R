#' Rename variables and/or their levels
#'
#' Renaming categorical variables and their levels, for instance for summary tables, can be fiddly. This
#' function accepts tibbles containing the old and new names for arguments and levels, and returns a dataframe
#' (or list of dataframes, if one is passed) with variables and levels renamed.
#'
#'
#' @param dat A dataframe or list of dataframes (e.g., from multiple imputation) contains the variables. If a list is passed, it must have class "list"
#' @param ... The variables to be renamed
#' @param var_names A tibble containing `old` and `new` names for the variables. If NULL, only levels are renamed.
#' @param level_names A tibble containing old `var` names and `level_old` and `level_new` names. If NULL, only variables are renamed.
#'
#' @return The dataframe or list of dataframes passed to dat, with variables and/or levels renamed
#' @export

rename_cat_variables <- function(dat, ..., var_names = NULL, level_names = NULL) {
  if (!"list" %in% class(dat)) dat <- list(dat)
  vars <- rlang::enquos(...)

  if (!is.null(level_names)) {
    level_names_lst <- split(level_names, level_names$var)

    relevel <- function(dat, var, levels_old, levels_new) {
      var <- var[1]
      names(levels_old) <- levels_new
      dat <- dat %>% dplyr::mutate(!!var := forcats::fct_recode(as.factor(!!rlang::sym(var)), !!!levels_old))
      dat
    }

    for (i in seq_along(level_names_lst)) {
      dat <- purrr::map(dat, relevel, level_names_lst[[i]]$var, level_names_lst[[i]]$level_old, level_names_lst[[i]]$level_new)
    }
  }

  if (!is.null(var_names)) {
    var_names_chr <- var_names$old
    names(var_names_chr) <- var_names$new
    dat <- purrr::map(dat, dplyr::rename, !!!var_names_chr)
  }

  if (length(dat) == 1) dat <- dat[[1]] # To return dataframe if dataframe was passed
  dat
}

std_stars <- c(`&dagger;` = .1, `*` = 0.05, `**` = 0.01, `***` = 0.001)
std_stars_pad <- c(`&nbsp;&nbsp;&nbsp;` = 1, `&dagger;&nbsp;&nbsp;` = .1, `*&nbsp;&nbsp;` = 0.05, `**&nbsp;` = 0.01, `***` = 0.001)
# TK - change padding so that coefficients are aligned


#' Significance stars for p-values
#'
#' Function returns significance stars for \emph{p}-values, most likely for use
#' in tables that report the results of multiple statistical tests. An empty
#' string is returned for NAs, unless that behaviour is overwritten.
#'
#'  Symbols and thresholds are *** \emph{p} < .001,  ** \emph{p} < .01, * \emph{p}
#'  < .05 and † \emph{p} < .1. The symbols can be changed by passing a named character vector sorted
#'  descendingly to the \code{stars} argument. For the default, the argument would be
#' \code{stars <- c(`&dagger;` = .1, `*` = 0.05, `**` = 0.01, `***` = 0.001)}
#'
#' @encoding UTF-8
#' @param p A \emph{p}-value or (more commonly) a vector of \emph{p}-values
#' @param stars A character vector to change the significance symbols (see details in `sigstars`)
#' @param ns Logical. Should non-significant values be highlighted as "ns"?
#' @param pad_html Should all results be padded right to the same width with HTML non-breaking spaces?
#' @param return_NAs Logical. Should NAs be returned? If not, empty strings are returned instead.
#' @return A character vector of significance stars for each \emph{p}-value,
#' each padded with spaces to be four characters long
#' @source Adapted from
#'  http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
#' @export

sigstars <- function(p, stars = NULL, pad_html = FALSE, ns = FALSE, return_NAs = FALSE) {
  if (is.null(stars)) stars <- c(`&dagger;` = .1, `*` = 0.05, `**` = 0.01, `***` = 0.001)
  ns <- ifelse(ns == TRUE, "<sup>ns</sup>", "")
  if (pad_html) {
    .check_req_packages(c("xml2"), note = "Trying to add HTML-padding to sigstars.")
    stars2 <- names(stars)
    if (any(stringr::str_detect(names(stars), "&"))) stars2 <- purrr::map_chr(names(stars), .unescape_html)
    nchars <- purrr::map_int(stars2, nchar)
    len <- max(nchars)
    stars3 <- purrr::map_chr(stars2, .pad, len)
    stars3 %>% stringr::str_replace_all(stringr::fixed(stars2), names(stars))
    names(stars) <- stars3
    ns <- paste0(ns, rep("&nbsp;", len - nchar(ns)), collapse = "")
  }

  out <- rep(ns, length(p))

  for (n in names(stars)) {
    out <- ifelse(p < stars[n], n, out)
  }

  if (!return_NAs) out[is.na(out)] <- ""

  out
}

.make_stars_note <- function(stars = NULL, markdown = TRUE) {
  if (is.null(stars)) stars <- c(`&dagger;` = .1, `*` = .05, `**` = .01, `***` = .001)
  out <- stars
  if (markdown == TRUE) {
    out <- paste0(names(out), " *p* < ", sub(".", "", out))
  } else {
    out <- paste0(names(out), " p < ", sub(".", "", out))
  }
  out <- paste0(out, collapse = ", ")

  return(out)
}


.pad <- function(x, len, padding = "&nbsp;") {
  x <- as.character(x)
  n <- nchar(x)
  if (n < len) {
    return(paste0(x, paste0(rep(padding, len - n), collapse = "")))
  }
  x
}

.unescape_html <- function(str) {
  purrr::map_chr(str, function(x) {
    xml2::xml_text(xml2::read_html(paste0("<x>", x, "</x>")))
  })
}


#' Cut a continuous variable into given proportions
#'
#' \code{cut()} and similar functions can cut continuous variables by quantile;
#' other helper functions exist to cut variables into groups of the same size
#' or width. This function cuts a continuous variable into given proportions.
#'
#' Ties within the continuous variable are allocated randomly - so this function
#' should not be used if there are many ties. The number of observations per
#' group is rounded up for even-numbered levels (second, fourth, etc) and
#' rounded down for others (expect for the last level that is used to balance).
#' For large numbers of observations, the distribution will be very close to
#' what is desired, for very small numbers of observations, it should be checked.
#'
#' @param x A numeric variable that is to be cut into categories
#' @param p The proportion of cases to be allocated to each category, in
#' ascending order. Should add up to one, otherwise, it will be scaled accordingly
#' @param ties.method Currently accepts only "random" - could be expanded in the
#' future, though it is unclear what a better method would be
#' @param fct_levels Character vector with names for levels. If it is NULL, the
#' groups will be labeled with their number and the cut-points employed.
#' @param verbose Should boundaries of groups be reported as message?
#' @return Factor variable with x cut into length(p) categories in given
#' proportions
#' @examples
#' cut_p(iris$Sepal.Length, p = c(.25, .50, .25), fct_levels = c("short", "middling", "long"))
#' @export

cut_p <- function(x, p, ties.method = "random", fct_levels = NULL, verbose = TRUE) {
  if (!ties.method == "random") stop('Currently, only "random" is accepted as ties.method.', call. = FALSE)
  if (sum(p) != 1) {
    message("p should be probabilities that add up to 1 - will be scaled accordingly")
    p <- p / sum(p)
  }

  xNA <- x
  x <- x[!is.na(x)]

  ranks <- rank(x, na.last = "keep", ties.method)
  start <- min(x)
  end <- x[match(.floor_ceiling(p[1] * length(x), 1), ranks)]
  out <- rep(paste0("Group ", 1, " (", start, " to ", end, ")"), ceiling(p[1] * length(x)))
  for (i in seq.int(2, length(p) - 1, 1)) {
    start <- x[match(.floor_ceiling(cumsum(p)[i - 1] * length(x) + 1, i - 1), ranks)]
    end <- x[match(.floor_ceiling(cumsum(p)[i] * length(x), i), ranks)]
    out <- c(out, rep(paste0("Group ", i, " (", start, " to ", end, ")"), .floor_ceiling(p[i] * length(x), i)))
  }
  start <- x[match(.floor_ceiling(cumsum(p)[length(p) - 1] * length(x) + 1, length(p) - 1), ranks)]
  end <- max(x)
  out <- c(out, rep(paste0("Group ", length(p), " (", start, " to ", end, ")"), length(x) - length(out)))

  out <- factor(out)

  if (!is.null(fct_levels)) {
    if (!length(fct_levels) == length(p)) {
      stop("Arguments fct_levels and p need to have same length", call. = FALSE)
    }
    if(verbose) {
      message("Factor levels were assigned as follows:\n", paste(levels(out), fct_levels, sep = ": ", collapse = "\n"))
    }
    levels(out) <- fct_levels
  }

  xNA[!is.na(xNA)] <- out[ranks]
  xNA <- factor(xNA)
  if (!is.null(fct_levels)) {
    if (!length(fct_levels) == length(p)) {
      stop("Arguments fct_levels and p need to have same length", call. = FALSE)
    }
    levels(xNA) <- fct_levels
  }
  xNA
}

#' Helper function to round up and down in turn
#'
#' Iterates between floor() and ceiling()
#'
#' @param x Numeric, to be rounded
#' @param i Iterator. floor() will be used on x for odd i, ceiling() for even i
#' @keywords internal

.floor_ceiling <- function(x, i) {
  if (i %% 2 == 1) {
    return(ceiling(x))
  }
  floor(x)
}


#' Scales a vector and returns it without attributes
#'
#' The `base::scale()` function adds attributes to the output that can lead to
#' problems later on. This function scales a vector and strips the attributes.
#'
#' @inheritParams base::scale
#' @keywords internal

scale_blank <- function(x, center = TRUE, scale = TRUE) {
  as.numeric(scale(x))
}





#' Convert a tibble/dataframe to tribble code
#'
#' Tribbles are an easy way to legibly input data, and therefore helpful for teaching
#' and interactive work. This function takes
#' a tibble and returns code that can recreate it. Note that this function converts
#' "NA" to NA and converts factors to characters to retain the levels.
#'
#' @param x The tibble/dataframe to be converted into tribble code
#' @param show Logical. Print code (otherwise, returned - print with `cat()` to get linebreaks etc)
#' @param digits Number of digits to round numeric columns to.
#' @examples
#' to_tribble(mtcars, show = TRUE)
#' @export

to_tribble <- function(x, show = FALSE, digits = 5) {
  assert_data_frame(x)
  no_cols <- ncol(x)
  x %<>% dplyr::mutate_if(is.factor, as.character)
  x %<>% dplyr::mutate_if(is.character, function(x) paste0('"', x, '"'))
  x %<>% dplyr::mutate_if(is.numeric, function(x) round(x, digits))

  lengths <- pmax(
    purrr::map_int(x, ~ max(nchar(.x, keepNA = FALSE))),
    purrr::map_int(names(x), ~ nchar(.x))
  )
  if (sum(lengths) + no_cols * 3 > 80) message("Some entries are too long for the tibble code to be well formatted")
  vars <- names(x)


  code <- "tibble::tribble(
  "
  for (i in seq_len(length(vars))) {
    code <- glue::glue("{code}~{vars[i]}, {paste0(collapse = '', '', rep(' ', lengths[i] - nchar(vars[i])))}")
  }
  for (j in seq_len(nrow(x))) {
    code %<>% paste0("\n  ")
    for (i in seq_len(length(vars))) {
      code <- glue::glue("{code} {x[[j, i]]}, {paste0(collapse = '', '',rep(' ',  lengths[i] - nchar(x[[j, i]], keepNA = FALSE)))}")
    }
  }

  code %<>% stringr::str_replace_all('"NA"', "NA")

  code %<>% stringr::str_trim() %>%
    substr(1, nchar(.) - 1) %>%
    paste0("\n)\n")

  if (show) {
    cat(code)
    return(invisible(code))
  }
  code
}

#' Get code to generate tibbles to rename categorical variables and their levels
#'
#' Renaming categorical variables and their levels, for instance for summary tables, can be fiddly. This
#' function generates code in which only the new names need to be modified, and which can then be passed
#' to either \code{\link{rename_cat_variables}} or directly to \code{\link{report_cat_vars}}
#'
#' Only categorical variables should be passed to the function if code for levels is
#' requested. If a variable has more than 20 distinct values, it is dropped from the levels-tribble-code,
#' unless the max_levels argument is increased.
#'
#'
#' @param df A dataframe that contains the variables - only used to extract their possible levels.
#' @param ... The variables to be included in the rename tribbles. If none are
#' specified, all are included, unless there are more than `max_level` variables in the df
#' @param show Logical - should the output be printed to the console. In any case, it is returned invisibly
#' @param which Should tribble code be generated for variables (\code{"vars"}), levels (\code{"levels"}) or both (\code{"both"}) (default)
#' @param max_levels The maximum number of levels before a variable is dropped from the levels_tribble. Defaults to 20
#'
#' @return Code to be edited and passed to tibble::tribble() to create var_names and level_names arguments for
#' \code{\link{rename_cat_variables}} and \code{\link{report_cat_vars}}. 'New' variable and level names are minimally
#' transformed, but are primarily intended to be placeholders.
#'
#' @examples
#' get_rename_tribbles(iris)
#' @export

get_rename_tribbles <- function(df, ..., show = TRUE, which = c("both", "vars", "levels"), max_levels = 20) {
  assert_data_frame(df)
  assert_choice(which[1], c("both", "vars", "levels"))
  assert_count(max_levels)
  assert_logical(show)

  vars <- rlang::enquos(...)
  if (length(vars) == 0) {
    vars_chr <- names(df)
    if(length(vars_chr)>max_levels) stop("No variables were specified for renaming.",
                                         " Would usually include all, but when there are more than",
                                         " specified in max_levels, that seems excessive. Please ",
                                         "specify variables or increase that number.")
  } else {
    vars_chr <- purrr::map_chr(vars, dplyr::as_label)
  }

  out <- list()
  if (which[1] %in% c("both", "vars")) {
    vars_tribble <- tibble::tibble(old = vars_chr, new = vars_chr %>% stringr::str_replace_all("_", " ") %>% stringr::str_to_title()) %>% to_tribble(show = show)
    out <- c(out, rename_vars = vars_tribble)
  }
  if (which[1] %in% c("both", "levels")) {
    get_levels <- function(x, df) {
      df[[x]] %>%
        factor() %>%
        levels()
    }
    levels_list <- purrr::map(vars_chr, get_levels, df)
    names(levels_list) <- vars_chr
    levels_list <- Filter(function(x) length(x) <= max_levels, levels_list)
    if (length(levels_list) >= 1) {
      mt <- function(x, name) {
        tibble::tibble(var = name, level_old = x, level_new = x %>% stringr::str_replace_all("_", " ") %>% stringr::str_to_title())
      }
      levels_tribble <- purrr::lmap(levels_list, function(x) purrr::map(x, mt, names(x))) %>%
        purrr::map_dfr(rbind) %>%
        tibble::as_tibble() %>%
        to_tribble(show = show)
      out <- c(out, rename_levels = levels_tribble)
    }
  }

  out
}


#' Get code to rename model coefficients in summary tables
#'
#' Usually, the coefficients in a statistical model such as those returned from `lm()`
#' need to be renamed for reporting, particularly when dummy/indicator variables are involved.
#' This function returns the code to set up a rename tribble that can then be adjusted
#' and passed, for instance, to the \code{\link{report_lm_with_std}} function. 
#'
#' @param mod A model (e.g., returned from lm). coef() is called to extract the coefficient names.
#' @param show Logical - should the output be printed to the console. In any case, it is returned invisibly
#'
#' @return Code to be edited and passed to tibble::tribble() to create the coef_renames argument for
#' \code{\link{report_lm_with_std}} and similar functions. 'New' coefficient names are minimally
#' transformed, but are primarily intended to be placeholders.
#'
#' @examples
#' mod <- lm(mpg ~ wt, mtcars)
#' get_coef_rename_tribble(mod)
#' @export

get_coef_rename_tribble <- function(mod, show = TRUE) {
  coefs <- try(stats::coef(mod), silent = TRUE)
  if (class(coefs) == "try-error") stop("coef() could not extract coefficients from mod argument.")
  coefs <- names(coefs)
  assert_logical(show)

  tibble::tibble(old = coefs, new = coefs %>% stringr::str_replace_all("_", " ") %>% stringr::str_to_title()) %>% to_tribble(show = show)
}


#' Copy data to clipboard to paste into Excel
#'
#' This function copies a dataframe into the clipboard, so that it can be
#' pasted into excel.
#'
#' @param df Dataframe to be copied.
#' @param row_names Logical. Should row names be copied?
#' @param col_names Logical. Should column names be copied?
#' @param ... Further arguments passed to `write.table`
#' @source https://www.r-bloggers.com/copying-data-from-excel-to-r-and-back/
#' @export

clip_excel <- function(df, row_names = FALSE, col_names = TRUE, ...) {
  utils::write.table(df, "clipboard", sep = "\t", row.names = row_names, col.names = col_names, ...)
}


#' Dump objects to clipboard (to transfer them between R sessions)
#'
#' This function calls dump on one or several R objects, which creates code that recreates them from the console.
#' It then copies this code to the clipboard. This can be used to quickly copy (small) objects between R sessions,
#' for instance during package development and testing, or - of course - to paste the dump code into a forum post.
#'
#' @param objects A character vector containing the names of one or more objects in the current session.
#' @export

dump_to_clip <- function(objects) {
  .check_req_packages("clipr")

  if (!interactive()) stop("Clipboard access only supported in interactive sessions")
  if (!is.character(objects)) stop("'objects' need to be a character vector with the names of one or more R objects")
  utils::capture.output(dump(objects, file = "")) %>% clipr::write_clip()
}

#' Turn line of items separated by whitespace into vector or code for c()
#'
#' This takes a line of items separated by spaces, tabs or newlines and returns a c()
#' vector command - with the items quoted or not - or a vector created by that command. By default, the line
#' is read from the clipboard and a character vector returned, and only the highest-level
#' separator present in x is used to split. Note that this makes it possible to copy values
#' in most spreadsheet programs such as Excel and use this function to pull them
#' from the clipboard and turn them into code that creates a vector.
#'
#' If x is not split by spaces, stringr::str_trim() is used to trim whitespace from the start
#' and end of each element of the vector.
#'
#' @param x Character string of desired vector items, separated by spaces, tabs or linebreaks. If NULL, it will be read from the clipboard.
#' @param strings Should vector items be considered as strings, i.e. quoted.
#' @param separators Which separator should x be split by. This defaults to the "top-level" found in x, i.e., newlines, if there are any, and otherwise tabs or spaces. Can also be "all".
#' @param to_clip Should code for vector be copied into clipboard? Defaults to TRUE in interactive use, but only works when \code{clipr} package is installed
#' @param return Should code or a vector be returned? Defaults to code
#' @examples
#' line_to_vector("a b c", strings = TRUE)
#'
#' line_to_vector("1 2 3", st = FALSE, return = "vector")
#'
#' line_to_vector("Hello World!
#'                 How are the bananas today?
#'                 Thanks for being here.")
#'
#' weekend <- line_to_vector("Friday Saturday Sunday", return = "vector")
#' @export

line_to_vector <- function(x = readLines("clipboard", warn = FALSE), strings = TRUE, separators = c("top-level", "all"), to_clip = interactive(), return = c("code", "vector")) {
  assert_character(x)
  assert_choice(separators[1], c("all", "top-level"))
  assert_choice(return[1], c("code", "vector"))

  if (separators[1] == "all") {
    x <- strsplit(x, " |\\n|\\t") %>% unlist()
  } else {
    if (!length(x) > 1) { #Otherwise, multiple lines have been read
    sep <- dplyr::case_when(
      stringr::str_detect(x, "\\n") ~ "\\n",
      stringr::str_detect(x, "\\t") ~ "\\t",
      TRUE ~ " "
    )
    x <- strsplit(x, sep) %>% unlist()
  }}
  x <- x[!x == ""]
  if (strings) {
    x <- stringr::str_trim(x)
    out <- paste0('c("', paste0(x, collapse = '", "'), '")')
  } else {
    out <- paste0("c(", paste0(x, collapse = ", "), ")")
  }
  if (length(x) == 1) message("No separators found - returning single item.")
  if (to_clip) {
    if (!requireNamespace("clipr", quietly = TRUE)) {
      warning("clipr package is needed to write to clipboard, but is not available.")
    } else {
      clipr::write_clip(out)
    }
  }
  if (return[1] == "vector") {
    return(eval(parse(text = out)))
  }
  out
}

l2v <- line_to_vector

#' Calculate share of NA-values in vector
#'
#' Returns share of NA values in vector by wrapping `sum(is.na(x)) / length(x)`
#' and rounding the result
#'
#' @param x Vector
#' @param round Number of digits to round result to
#' @examples
#' x <- c(NA, 1, 2)
#' na_share(x)
#' @export

na_share <- function(x, round = 2) {
  (sum(is.na(x)) / length(x)) %>% round(round)
}

rm_na <- function(x) {
  x[!is.na(x)]
}

#' Add class
#'
#' This function adds a given class to an object, so that
#' different S3 methods can be called (e.g., `tidy.exp` to get OR for logistic regression models
#' (e.g., in `modelsummary::msummary`)
#'
#'
#' @param x An object
#' @param class_to_add String of the class to add, defaults to "exp"
#' @keywords internal

add_class <- function(x, class_to_add = "exp") {
  class(x) <- c(class_to_add, class(x))
  x
}

#' Tests whether a column in dataframe, specified by string, is numeric
#'
#' @param col Character indicating column name
#' @param df Dataframe that contains `col`
#' @keywords internal

.is.numeric_col <- function(col, df) {
  is.numeric(magrittr::extract2(df, col))
}

#' Tidy function to exponentiate coefficients
#'
#' This function calls the tidy method based on the second class of the
#' object (i.e. after removing the "exp" class that led to it being called),
#' and then exponentiates the returned estimates and confidence intervals (if any)
#' in the tibble. This is usually used to turn coefficients of logistic
#' regression models into Odds Ratios.
#'
#'
#' @param x An object, usually containing a logistic regression model. Should
#' have the class "exp" as the first of its classes, and then a class that dispatches
#' it to an appropriate `generics::tidy`` function
#' @param ... Arguments passed on to the appropriate `tidy` function
#' @export

tidy.exp <- function(x, ...) {
  class(x) <- class(x)[-1]
  if ("polr" %in% class(x)) {
  out <- generics::tidy(x, p.values = TRUE, ...)
  } else {
  out <- generics::tidy(x, ...)
  }
  out$estimate <- exp(out$estimate)
  if ("conf.high" %in% names(out)) {
    out$conf.high <- exp(out$conf.high)
    out$conf.low <- exp(out$conf.low)
  }
  out
}