#' Rename variables and/or their levels
#'
#' Renaming categorical variables and their levels, for instance for summary tables, can be fiddly. This
#' function accepts tibbles containing the old and new names for arguments and levels, and returns a dataframe
#' (or list of dataframes, if one is passed) with variables and levels renamed.
#'
#' @param data A dataframe or list of dataframes (e.g., from multiple imputation) contains the variables. If a list is passed, it must have class "list"
#' @param var_names A tibble containing `old` and `new` names for the variables. If NULL, only levels are renamed.
#' @param level_names A tibble containing old `var` names and `level_old` and `level_new` names. If NULL, only variables are renamed.
#'
#' @return The dataframe or list of dataframes passed to data, with variables and/or levels renamed. 
#' Any variables where levels are renamed will be converted to factors.
#' @export

rename_cat_variables <- function(data, var_names = NULL, level_names = NULL) {
  
  drop_list <- FALSE
  if (!inherits(data, "list")) {
    data <- list(data)
    drop_list <- TRUE
  }  

  
  if (!is.null(level_names)) {
    level_names_lst <- split(level_names, level_names$var)

    relevel <- function(data, var, levels_old, levels_new) {
      var <- var[1]
      #Convert to allow to rename numeric values here
      levels_old <- as.character(levels_old)    
      names(levels_old) <- levels_new
      data <- data %>% dplyr::mutate(!!var := forcats::fct_recode(as.factor(!!rlang::sym(var)), !!!levels_old))
      data
    }

    
    for (i in seq_along(level_names_lst)) {
      data <- purrr::map(data, relevel, level_names_lst[[i]]$var, level_names_lst[[i]]$level_old, level_names_lst[[i]]$level_new)
    }
  }

  if (!is.null(var_names)) {
    var_names_chr <- var_names$old
    names(var_names_chr) <- var_names$new
    data <- purrr::map(data, dplyr::rename, !!!var_names_chr)
  }

  if (drop_list) data <- data[[1]]
  data
}

std_stars <- c(`&dagger;` = .1, `*` = 0.05, `**` = 0.01, `***` = 0.001)
std_stars_pad <- c(`&nbsp;&nbsp;&nbsp;` = 1, `&dagger;&nbsp;&nbsp;` = .1, `*&nbsp;&nbsp;` = 0.05, `**&nbsp;` = 0.01, `***` = 0.001)

# TODO - change padding so that coefficients are aligned


#' Significance stars for p-values
#'
#' Function returns significance stars for *p*-values, most likely for use
#' in tables that report the results of multiple statistical tests. An empty
#' string is returned for NAs, unless that behaviour is overwritten.
#'
#'  Symbols and thresholds are *** *p* < .001,  ** *p* < .01, * *p*
#'  < .05 and † *p* < .1. The symbols can be changed by passing a named character vector sorted
#'  descendingly to the `stars` argument. For the default, the argument would be
#' `stars <- c(`&dagger;` = .1, `*` = 0.05, `**` = 0.01, `***` = 0.001)`
#'
#' @encoding UTF-8
#' @param p A *p*-value or (more commonly) a vector of *p*-values
#' @param stars A character vector to change the significance symbols (see details below)
#' @param ns Logical. Should non-significant values be highlighted as "ns"?
#' @param pad_html Should all results be padded right to the same width with HTML non-breaking spaces?
#' @param return_NAs Logical. Should NAs be returned? If not, empty strings are returned instead.
#' @return A character vector of significance stars for each *p*-value,
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
#' `cut()` and similar functions can cut continuous variables by quantile;
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
    if (verbose) {
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
#' @param to_clip Should code for vector be copied into clipboard? Defaults to TRUE in interactive use, but only works when `clipr` package is installed
#' @examples
#' to_tribble(mtcars[1:5, 1:3], show = TRUE)
#' @export

to_tribble <- function(x, show = FALSE, digits = 5, to_clip = interactive()) {
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

  code %<>% stringr::str_replace_all(stringr::fixed('"NA"'), "NA")

  code %<>% stringr::str_trim() %>%
    substr(1, nchar(.) - 1) %>%
    paste0("\n)\n")

  if (to_clip) {
    if (!requireNamespace("clipr", quietly = TRUE)) {
      warning("clipr package is needed to write to clipboard, but is not available.")
    } else {
      clipr::write_clip(code)
    }
  }
  
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
#' to either [rename_cat_variables()] or directly to [report_cat_vars()]
#'
#' Only categorical variables should be passed to the function if code for levels is
#' requested. If a variable has more than 20 distinct values, it is dropped from the levels-tribble-code,
#' unless the max_levels argument is increased.
#'
#'
#' @param data A dataframe that contains the variables - only used to extract their possible levels.
#' @param ... The variables to be included in the rename tribbles. If none are
#' specified, all are included, unless there are more than `max_level` variables in the data
#' @param show Logical - should the output be printed to the console. In any case, it is returned invisibly
#' @param which Should tribble code be generated for variables (`"vars"`), levels (`"levels"`) or both (`"both"`) (default)
#' @param max_levels The maximum number of levels before a variable is dropped from the levels_tribble. Defaults to 20
#'
#' @return Code to be edited and passed to tibble::tribble() to create var_names and level_names arguments for
#' [rename_cat_variables()] and [report_cat_vars()]. 'New' variable and level names are minimally
#' transformed, but are primarily intended to be placeholders.
#'
#' @examples
#' get_rename_tribbles(iris)
#' @export

get_rename_tribbles <- function(data, ..., show = TRUE, which = c("both", "vars", "levels"), max_levels = 20) {
  assert_data_frame(data)
  assert_choice(which[1], c("both", "vars", "levels"))
  assert_count(max_levels)
  assert_logical(show)

  vars <- rlang::enquos(...)
  if (length(vars) == 0) {
    vars_chr <- names(data)
    if (length(vars_chr) > max_levels) stop("No variables were specified for renaming.",
                                            " Would usually include all, but when there are more than",
                                            " specified in max_levels, that seems excessive. Please ",
                                            "specify variables or increase that number.")
  } else {
    vars_chr <- purrr::map_chr(vars, dplyr::as_label)
  }

  out <- list()
  if (which[1] %in% c("both", "vars")) {
    vars_tribble <- tibble::tibble(old = vars_chr, 
                                   new = vars_chr %>% stringr::str_replace_all(stringr::fixed("_"), " ") %>% stringr::str_to_title()) %>% 
      to_tribble()
    out <- c(out, rename_vars = paste("var_names <- ", vars_tribble))
  }
  if (which[1] %in% c("both", "levels")) {
    get_levels <- function(x, data) {
      data[[x]] %>%
        factor() %>%
        levels()
    }
    levels_list <- purrr::map(vars_chr, get_levels, data)
    names(levels_list) <- vars_chr
    levels_list <- Filter(function(x) length(x) <= max_levels, levels_list)
    if (length(levels_list) >= 1) {
      mt <- function(x, name) {
        tibble::tibble(var = name, level_old = x, level_new = x %>% stringr::str_replace_all("_", " ") %>% stringr::str_to_title())
      }
      levels_tribble <- purrr::lmap(levels_list, function(x) purrr::map(x, mt, names(x))) %>%
        purrr::map_dfr(rbind) %>%
        tibble::as_tibble() %>%
        to_tribble()
      out <- c(out, rename_levels = paste("level_names <- ", levels_tribble))
    }
  }

  if (show) {
    purrr::walk(out, cat)
    invisible(out)
  } else {
    out
  }
}


#' Get code to rename model coefficients in summary tables
#'
#' Usually, the coefficients in a statistical model such as those returned from `lm()`
#' need to be renamed for reporting, particularly when dummy/indicator variables are involved.
#' This function returns the code to set up a rename tribble that can then be adjusted
#' and passed, for instance, to the [report_lm_with_std()] function. 
#'
#' @param mod A model (e.g., returned from lm). coef() is called to extract the coefficient names.
#' @param show Logical - should the output be printed to the console. In any case, it is returned invisibly
#'
#' @return Code to be edited and passed to tibble::tribble() to create the coef_renames argument for
#' [report_lm_with_std()] and similar functions. 'New' coefficient names are minimally
#' transformed, but are primarily intended to be placeholders.
#'
#' @examples
#' mod <- lm(mpg ~ wt, mtcars)
#' get_coef_rename_tribble(mod)
#' @export

get_coef_rename_tribble <- function(mod, show = TRUE) {
  coefs <- try(stats::coef(mod), silent = TRUE)
  if (inherits(coefs, "try-error")) stop("coef() could not extract coefficients from mod argument.")
  coefs <- names(coefs)
  assert_logical(show)

  tibble::tibble(old = coefs, new = coefs %>% stringr::str_replace_all(stringr::fixed("_"), " ") %>% stringr::str_to_title()) %>% to_tribble(show = show)
}


#' Copy data to clipboard to paste into Excel
#'
#' This function copies a dataframe into the clipboard, so that it can be
#' pasted into excel.
#'
#' @param data Dataframe to be copied.
#' @param row_names Logical. Should row names be copied?
#' @param col_names Logical. Should column names be copied?
#' @param ... Further arguments passed to `write.table`
#' @source https://www.r-bloggers.com/copying-data-from-excel-to-r-and-back/
#' @export

clip_excel <- function(data, row_names = FALSE, col_names = TRUE, ...) {
  utils::write.table(data, "clipboard", sep = "\t", row.names = row_names, col.names = col_names, ...)
}


#' Dump objects to clipboard (to transfer them between R sessions)
#'
#' This function calls dump on one or several R objects, which creates code that recreates them from the console.
#' It then copies this code to the clipboard. This can be used to quickly copy (small) objects between R sessions,
#' for instance during package development and testing, or - of course - to paste the dump code into a forum post.
#'
#' @param x An object to dump to the clipboard, or a character vector of object names. 
#' To enable the use in pipelines, objects that are passed directly will be named `dumped_object`
#' in the resulting code.
#'  
#' @examples
#' if (interactive()) {
#'   outcome <- "mpg"
#'   mod <- lm(mpg ~ wt, mtcars)
#'   mod %>% dump_to_clip() # will be named dumped_object
#'   dump_to_clip(c("outcome", "mod")) # will retain variable names
#' }
#' 
#' @export

dump_to_clip <- function(x) {
  .check_req_packages("clipr")
  if (!interactive()) stop("Clipboard access only supported in interactive sessions")
  
  if (!is.character(x)) {
    dumped_object <- x
    
    # Dump the object to clipboard
    utils::capture.output(dump("dumped_object", file = stdout())) %>%
      clipr::write_clip()
    
  } else {
    existing_objects <- sapply(x, exists, envir = .GlobalEnv)
    
    if (all(existing_objects)) {
      # If all objects exist, dump them by name
      utils::capture.output(dump(x, file = stdout())) %>%
        clipr::write_clip()
      
    } else {
      if (any(existing_objects)) {
        message("Some but not all elements of `x` exist in the global environment. Thus, I am unsure whether you mean",
                " to dump a character vector, or a set of objects. I will dump the character vector. If you want to ",
                "dump objects, ensure that all exist or remove the missing one(s) from the vector: ", paste0(x[!existing_objects], collapse = ", "), 
                ".")
      }
      
      dumped_object <- x
      
      # Dump the object to clipboard
      utils::capture.output(dump("dumped_object", file = stdout())) %>%
        clipr::write_clip()
    }
  }  
}

#' Turn a Line of Items into a Vector or c() Code
#'
#' This function takes a line of items separated by spaces, tabs, or newlines and 
#' returns either a `c()` vector command (with items quoted or not) or the vector 
#' created by that command. By default, the line is read from the clipboard and 
#' a character vector is returned. The function uses only the highest-level 
#' separator present in `x` unless specified otherwise. This functionality is 
#' particularly useful for copying values from spreadsheet programs like Excel 
#' (or from R console output) and converting them into R vectors seamlessly.
#'
#' If `x` is not split by spaces, `stringr::str_trim()` is used to trim 
#' whitespace from the start and end of each element of the vector.
#'
#' @param x Character string of desired vector items, separated by spaces, tabs, 
#' or line breaks. If `NULL`, the function will attempt to read from the clipboard.
#' @param strings Logical or `NULL`. Determines whether vector items should be 
#' treated as strings (i.e., quoted). Defaults to `TRUE` unless all items are numeric.
#' @param separators Character string indicating which separator should be used 
#' to split `x`. Defaults to the highest-level separator found in `x` (i.e., 
#' newlines if present). Can also be `"all"` to split by all supported separators 
#' (i.e. spaces: `" "`, commas: `","`, tabs: `"\t"`, newlines: `"\n"`).
#' @param to_clip Logical. Indicates whether the generated code for the vector 
#' should be copied to the clipboard. Defaults to `TRUE` in interactive sessions. 
#' Requires the `clipr` package to be installed.
#' @param return_type Character string specifying the type of return value. 
#' Choose `"code"` to return the `c()` command as a string or `"vector"` to 
#' return the actual R vector. Defaults to `"code"`. Abbreviated values like 
#' `"c"` or `"v"` are accepted.
#' @param keep_blank_as_na Logical. If `TRUE`, empty values are kept as `NA`. 
#' If `FALSE` (default), empty values are omitted from the resulting vector.
#'
#' @return A character string representing the `c()` command or the actual R 
#' vector, depending on the `return_type` parameter.
#'
#' @examples
#' 
#' line_to_vector("a b c", strings = TRUE)
#' # c("a", "b", "c")
#' 
#' 
#' line_to_vector("1 2 3", return_type = "vector")
#' # [1] 1 2 3
#' 
#' # Can abbreviate return argument
# line_to_vector("Friday Saturday Sunday", return_type = "v")
#' # [1] "Friday"   "Saturday" "Sunday"
#'
#' @export


line_to_vector <- function(x = NULL, strings = NULL, separators = c("top-level", "all"), 
                           to_clip = interactive(), return = c("code", "vector"), keep_blank_as_na = FALSE) {
  
  matchArg(return, c("code", "vector"), several.ok = TRUE, .var.name = "return")
  matchArg(separators, c("top-level", "all", " ", ",", "\t", "\n"), several.ok = TRUE,  .var.name = "separators")
  
  return <- return[1]
  
  if (is.null(x)) {
    if (!requireNamespace("clipr", quietly = TRUE)) {
      stop("The 'clipr' package is required to read from the clipboard, but is not available. Please specify 'x' or install the package.")
    } else {
      x <- clipr::read_clip()
      if(is.null(x)) return(NULL)
    }
  }
  
  # Ensure x is atomic vector and not NULL
  assert_atomic_vector(x)
  
  # Handle blank clipboard content
  if (all(x == "")) {
    stop("Clipboard content is empty or inappropriate. Please provide valid content.")
  }
  
  # Handle separators
  if (separators[1] == "all") {
    x <- strsplit(x, "[ ,\n\t]") %>% unlist()
  } else if (separators[1] == "top-level") {
      sep <- dplyr::case_when(
        stringr::str_detect(x, "\\n") ~ "\\n",
        stringr::str_detect(x, "\\t") ~ "\\t",
        stringr::str_detect(x, ",") ~ ",",   # Add comma as a valid separator
        TRUE ~ " "
      )
      x <- strsplit(x, sep) %>% unlist()
    } else {
      x <- strsplit("a,b\nc\td", paste0("[", paste(separators, collapse = ""), "]")) %>% unlist()
  }
  
  # Trim whitespace
  x <- stringr::str_trim(x)
  
  # Handle blank entries, with option to keep as NA
  if (keep_blank_as_na) {
    x[x == ""] <- NA
  } else {
    x <- x[!x == ""]
  }
  
  if (is.null(strings)) strings <- any(stringr::str_detect(x, "^-?\\d+\\.?\\d*$") == FALSE)
  
  # Prepare output
  if (strings) {
    quote_element <- function(el) {
      # To ensure NA is not returned as literal "NA"
      if (is.na(el)) {
        "NA"
      } else {
        # Escape existing double quotes
        el_escaped <- gsub('"', '\\\\"', el)
        paste0('"', el_escaped, '"')
      }
    }
    
    quoted_x <- sapply(x, quote_element)
    
    out <- paste0("c(", paste(quoted_x, collapse = ", "), ")")
  } else {
    out <- paste0("c(", paste(x, collapse = ", "), ")")
  }
  
  # If no separators found and only one item
  if (length(x) == 1) {
    message("No separators found - returning single item.")
  }
  
  # Copy to clipboard if requested
  if (to_clip) {
    if (!requireNamespace("clipr", quietly = TRUE)) {
      warning("The 'clipr' package is required to write to clipboard, but is not available.")
    } else {
      clipr::write_clip(out)
    }
  }
  
  # Return vector or code
  if (return[1] == "vector") {
    return(eval(parse(text = out)))
  }
  
  return(out)
}

#' @rdname line_to_vector
#' @export
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

#' Set variable to NA when it has specific values
#'
#' This function replaces specific values in a variable with NA. It would most commonly be used
#' to remove missing values indicated with codes such as "NA", -999 or "none". 
#' (NB: It is very similar to dplyr's `na_if` but accepts more than one value.) 
#'
#' @param x The variable to transform.
#' @param replace One or more values to replace by NA.
#' 
#' @export
#' @examples
#' library(dplyr)
#' 
#' ess_health %>% 
#'     mutate(eisced = na_ifs(eisced, c(7, 55)))
  

na_ifs <- function(x, replace) {
  purrr::map(replace, function(z) {
    x[x == z] <<- NA
  })
  x
}

#' Set variable to NA based on conditions
#'
#' This function sets a variable to NA based on one or several logical conditions. 
#' It would most naturally be used inside a dplyr-mutate call.
#' By default, the conditions are combined with a logical OR, yet this can be changed
#' to AND by setting operator = "&".
#' 
#' Note that the function is called `na_when` to prevent clashing with dplyr's `na_if` ... 
#' even though the latter might be the more intuitive name.
#'
#' @param x The variable to transform.
#' @param ... One or more logical conditions, involving x or other variables.
#' @param operator Name of a logical operator to combine the conditions. Defaults to `"|"` (or),
#' `"&"` (and) is the other common choice, though `"xor"` would also work.
#' 
#' @export
#' @examples
#' library(dplyr)
#' 
#' ess_health %>% 
#'     mutate(eisced = na_when(eisced, cntry == "DE", agea < 21))
#'     
#' mtcars %>% 
#'     mutate(carb = na_when(carb, cyl > 6, mpg < 19, operator = "&"))     

na_when <- function(x, ..., operator = "|") {
  condition <- Reduce(operator, list(...))
  ifelse(condition, NA, x)
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
#' @param data Dataframe that contains `col`
#' @keywords internal

.is.numeric_col <- function(col, data) {
  is.numeric(magrittr::extract2(data, col))
}

#' Run a code snippet and copy formatted code and output to clipboard
#'
#' This builds on the [reprex::reprex()] function to take a chunk of code,
#' run it and copy the formatted code and output to the clipboard. Note that 
#' this does NOT run in a clean session and is thus unlikely to create *reproducible*
#' examples. Instead, it is intended to be used to quickly get a formatted bit 
#' of code and output that is not self-contained, e.g., for teaching materials.
#' 
#' @param code The code to run - can be NULL, then the code is retrieved from the clipboard.
#' @examples 
#' if (interactive()) {
#'   name <- "Lukas"
#'   run_and_format(paste("My name is", name))
#' }
#' @export


run_and_format <- function (code = NULL) {
  
  .check_req_packages("reprex")
  
  reprex_internal <- get("reprex_impl", envir = asNamespace("reprex"),
                         inherits = FALSE)
  
  reprex_internal(
    x_expr = substitute(code),
    input = NULL,
    wd = NULL,
    venue = "gh",
    
    render = TRUE,
    new_session = FALSE,
    
    advertise       = FALSE,
    session_info    = FALSE,
    style           = TRUE,
    html_preview    = TRUE,
    comment         = "#>",
    tidyverse_quiet = TRUE,
    std_out_err     = FALSE
  )
  
}

#' Create a list with items named based on the names of variables passed to the function
#' 
#' Not infrequently, you might find yourself writing things like `list(var1 = var1, var2 = var2)`.
#' This function makes this easier by creating a list with items named based on the variables 
#' passed into it.
#' 
#' @param ... One or more variables to be included into a list. Note that you can pass values as 
#' well, but they are then used as both the name and the value of the respective list item.
#' @return A named list, with names taken from the variable names in the input
#' @examples 
#' name <- "Paul"
#' age <- 10
#' named_list(name, age)
#' 
#' # Note that this can be combined with named arguments
#' named_list(name, age, place = "Berlin")
#' 
#' @export
#' @keywords internal


named_list <- function(...) {
  out <- list(...)
  new_names <- match.call() %>%
    as.list() %>%
    .[-1]
  given_names <- names(new_names) 
  if (!is.null(given_names)) {
    new_names <- given_names %>% dplyr::na_if("") %>% 
      dplyr::coalesce(as.character(new_names))
  }
  if (as.character(new_names[[1]]) == ".") {
    warning("First list item will be named '.' - that is most likely because you used the %>% operator. To do that successfully, you usually need to name the corresponding argument, e.g., var1 %>% named_list(var1 = ., var2)")
  }
  names(out) <- new_names
  out
}

#' Paste arguments while ignoring NAs
#'
#' This function behaves like base::paste(), but replaces any NA elements with an empty string.
#' It also supports vector recycling and accepts the 'sep' and 'collapse' arguments.
#'
#' @param ... The arguments to be concatenated. These can be vectors.
#' @param sep The string that separates the concatenated elements. Defaults to a single space.
#' @param collapse An optional string to separate the results. NULL by default, which means no separation.
#'
#' @return A character vector of concatenated strings.
#' @examples
#' paste_("hello", NA, "you") 
#' # returns "hello you"
#' 
#' paste_(c("hello", "world"), c(NA, "everyone"), c("you", NA)) 
#' # returns c("hello you", "world everyone")
#'
#' @export

paste_ <- function(..., sep = " ", collapse = NULL) {
  args <- list(...)
  
  if (length(args) == 0) {
    return(ifelse(is.null(collapse), "", collapse))
  }
  
  # Convert all arguments to character vectors and replace NAs with ""
  args <- lapply(args, function(x) ifelse(is.na(x), "", as.character(x)))
  
  # Find the maximum length among the arguments
  max_length <- max(sapply(args, length))
  
  # Ensure all arguments are of the same length by repeating them as needed
  args <- lapply(args, function(x) rep(x, length.out = max_length))
  
  # Concatenate corresponding elements from each list element
  result <- apply(do.call(cbind, args), 1, function(row) {
    output <- paste(row[row != ""], collapse = sep)
    if (output == "") return("")
    output
  })
  
  # Apply collapse if needed
  if (!is.null(collapse)) {
    result <- paste(result[result != ""], collapse = collapse)
  }
  
  return(result)
}

#' Get duplicated items from a vector
#'
#' This function takes a numeric or character vector and returns a vector of the 
#' unique elements that appear more than once - so a simple shortcut for 
#' `unique(x[duplicated(x)])` that is particularly friendly in pipes
#'
#' @param x A numeric or character vector from which to identify duplicate elements.
#'
#' @return A vector containing unique elements that are duplicated in the original vector. 
#' Returns an empty vector if there are no duplicates.
#'
#' @examples
#' dupl_items(c(1, 2, 3, 3, 4, 5, 5, 5))
#' # Output: 3 5
#' dupl_items(c(1, 2, 3, 4))
#' # Output: numeric(0)
#' 
#' @export
#' @keywords internal

dupl_items <- function(x) {
  duplicated_elements <- x[duplicated(x)]
  unique(duplicated_elements)
}

