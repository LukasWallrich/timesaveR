#' Create a summary table comparing standardized and non-standardized linear models
#'
#' This function creates a summary table for lm models (including mice::mira objects
#' containing lm-models) that shows a standardized and non-standardized version of the model
#' side-by-side. Several pairs of such models can be compared side-by-side.
#'
#' @param mod A lm-model/mira object of lm models, with variables not standardized (or a list of such models)
#' @param mod_std A lm-model/mira object of lm models, with standardized variables. Can be
#' created with \code{\link{lm_std}} (or a list of such models)
#' @param conf_level Confidence level to use for confidence intervals, defaults to .95
#' @param coef_renames A named character vector with new names for the coefficients or a tibble as provided by \code{\link{get_coef_rename_tribble}}
#' for variables. If NULL, then the coefficients are not renamed.
#' @param filename the file name to create a HTML file on disk.
#' @param model_names If several pairs of models are to be plotted side by side, indicate the label for each *pair* here
#' @param show_nimp Logical - DEFUNCT. If mira objects are passed, this determines whether the number of imputations will be reported as a model statistic
#' @param R2_change Logical. Report R2 change and F-test to compare models. 
#' @param notes List of notes to append to bottom of table. An explanation of significance stars is automatically added. If the std models were run with a helper function in this package, a note regarding the standardization is also automatically added.
#' @param apa_style Logical, should APA-style formatting be applied
#' @param statistic_vertical Should standard errors and CIs be shown below coefficients? Defaults to horizontal layout
#' @param stars Named vector of significance stars and their thresholds, check `timesaveR:::std_stars_pad` for default.
#' @inheritParams modelsummary::modelsummary
#' @inheritDotParams modelsummary::modelsummary coef_map coef_omit coef_rename
#' @return A list with `gt_tab` (the gt-table object including the parts of the table
#' that can be created with gt. This can be post-processed and formatted with functions in
#' the gt-package, but does not include the lower part with model statistics, e.g., R^2.) and
#' `html_code` (the code that creates the full table, and is used to render it in
#' the Viewer).
#' @examples
#'
#' # Standard lm model
#' mod1 <- lm(mpg ~ hp + wt, mtcars)
#'
#' # Model with standardized coefficients
#'
#' mod2 <- lm_std(mpg ~ hp + wt, mtcars)
#'
#' report_lm_with_std(mod1, mod2)
#' @export

report_lm_with_std <- function(mod, mod_std, conf_level = .95, coef_renames = NULL, fmt = "%.2f", statistic_vertical = FALSE, filename = NULL, model_names = NULL, show_nimp = FALSE, R2_change = FALSE, notes = list(NULL), apa_style = TRUE, stars = std_stars_pad, ...) {
  .check_req_packages(c("modelsummary", "gt", "htmltools", "readr"))

  if (is.data.frame(coef_renames)) {
    assert_names(names(coef_renames), must.include = c("old", "new"))
    coef_renames <- coef_renames$new %>% magrittr::set_names(coef_renames$old)
  }

  if ((class(mod)[1] == "list" || class(mod_std)[1] == "list") && !(length(mod) == length(mod_std))) {
    stop("Same number of models need to be included in mod and mod_std arguments")
  }
  
  if (!is.null(model_names) && !length(model_names) == length(mod)) {
    stop("Length of model names needs to be the same as length of model")
  }

  if (!(is.list(mod))) mod <- list(mod)
  if (!(is.list(mod_std))) mod_std <- list(mod_std)

  #For simplicity, only class of first model is tested 
if (!("lm" %in% class(mod[[1]]) || ("mira" %in% class(mod[[1]]) &&
  "lm" %in% class(mod[[1]]$analyses[[1]])))) {
  stop("Models need to be of class lm or mira objects with lm-analyses")
}
if (!("lm" %in% class(mod_std[[1]]) || ("mira" %in% class(mod_std[[1]]) &&
  "lm" %in% class(mod_std[[1]]$analyses[[1]])))) {
  stop("Models need to be of class lm or mira objects with lm-analyses")
}
  
  if ("tsR_std" %in% class(mod_std[[1]]) || ("mira" %in% class(mod_std[[1]]) && "tsR_std" %in% class(mod_std[[1]]$analyses[[1]]))) {
    notes %<>% c("Given that dummy variables lose their interpretability when standardized (Fox, 2015), &beta; for dummy variables are semi-standardized, indicating the impact of that dummy on the standardized outcome variable.")
  }


  SEs <- list()
  CIs <- list()
  mods <- list()
  mod_tidy <- list()
  mod_std_tidy <- list()
  stat_list <- list()
  
  #Remove tidy.lm warning for mira objects
  for (i in seq_along(mod_std)) {
    if ("mira" %in% class(mod_std[[i]])) {
      for (j in seq_along(mod_std[[i]][["analyses"]])) {
        class(mod_std[[i]][["analyses"]][[j]]) <- "lm"
      }
    }
  }

  for (i in seq_len(length(mod))) {
    mod_tidy[[i]] <- tidy(mod[[i]])
    SEs[[i]] <- paste0("(", sprintf(fmt, mod_tidy[[i]]$std.error), ")")
    names(SEs[[i]]) <- mod_tidy[[i]]$term

    mod_std_tidy[[i]] <- tidy(mod_std[[i]], conf.int = TRUE, conf.level = conf_level)
    CIs[[i]] <- paste0("[", sprintf(fmt, mod_std_tidy[[i]]$conf.low), ", ", sprintf(fmt, mod_std_tidy[[i]]$conf.high), "]")
    names(CIs[[i]]) <- mod_std_tidy[[i]]$term

    mods[[i * 2 - 1]] <- mod[[i]]
    stat_list[[i * 2 - 1]] <- SEs[[i]]
    mods[[i * 2]] <- mod_std[[i]]
    stat_list[[i * 2]] <- CIs[[i]]
  }

  names(mods) <- paste0("Model", seq_len(length(mods)))

  col_labels <- rep(list(gt::md("*<center>B (SE)</center>*"), gt::md("*<center>&beta; [95% CI]</center>*")), times = length(mod)) %>% stats::setNames(names(mods))




  notes %<>% c(.make_stars_note())

  notes <- Filter(Negate(is.null), notes)

  if (statistic_vertical) {
    tab <- modelsummary::msummary(mods, output = "gt", vcov = stat_list, rep(c(
      "{estimate} {stars}", "{estimate}"
    ), length(mod)), fmt = fmt, gof_omit = ".*", stars = stars, coef_rename = coef_renames, ...) %>%
      gt::fmt_markdown(columns = gt::everything()) %>%
      gt::cols_label(.list = col_labels) %>%
      gt::cols_align("right", gt::everything()) %>%
      gt::cols_align("left", columns = 1) %>%
      gt:::dt_source_notes_set("") # Remove std star note
  } else {
    tab <- modelsummary::msummary(mods, output = "gt", statistic = NULL, estimate = rep(c(
      "{estimate} ({std.error}){stars}",
      "{estimate} [{conf.low}, {conf.high}]"
    ), length(mod)), fmt = fmt, gof_omit = ".*", stars = stars, coef_rename = coef_renames, ...) %>%
      gt::fmt_markdown(columns = gt::everything()) %>%
      gt::cols_label(.list = col_labels) %>%
      gt::cols_align("right", gt::everything()) %>%
      gt::cols_align("left", columns = 1) %>%
      gt:::dt_source_notes_set("") # Remove std star note
  }

  if (apa_style) tab <- tab %>% gt_apa_style()

  for (i in seq_along(notes)) {
    tab <- tab %>% gt::tab_source_note(gt::md(notes[[i]]))
  }


  if (length(mod) > 1) {
    if (is.null(model_names)) model_names <- paste0("Model ", seq_len(length(mod)))
    for (i in seq_len(length(mod))) {
      current_cols <- c((2 * i):(2 * i + 1))
      tab <- tab %>% gt::tab_spanner(gt::md(paste0("**", model_names[i], "**")), columns = current_cols)
    }
  }

  code <- character()


  row <- '<tr style="border-top-style: solid; border-top-width: 2px;">
    <td class="gt_row gt_left" rowspan="1" colspan="1">  <em>N     </td>'

  Ns <- purrr::map(mod, ~broom::glance(.x)["nobs"])
  
  sums <- paste(purrr::map(Ns, function(x) {
    glue::glue('<td class="gt_row gt_center" rowspan="1" colspan="2"> {x$nobs}   </td>')
  }), collapse = " ")

  code %<>% paste(row, sums, "</tr>", collapse = "")

  row <- '<tr>
    <td class="gt_row gt_left" rowspan="1" colspan="1">  <em>R<sup>2</sup>     </td>'

  R2s <- purrr::map(mod, ~broom::glance(.x)["r.squared"])
  
  sums <- paste(purrr::map(R2s, function(x) {
    glue::glue('<td class="gt_row gt_center" rowspan="1" colspan="2"> {fmt_cor(as.numeric(x$r.squared))}   </td>')
  }), collapse = " ")

  code %<>% paste(row, sums, "</tr>", collapse = "")

  Fs <- purrr::map(mod, .lm_F_test)

  row <- '<tr>
    <td class="gt_row gt_left" rowspan="1" colspan="1"><em>F</em>-tests</td>'

  sums <- paste(purrr::map(Fs, function(x) {
    glue::glue('<td class="gt_row gt_center" rowspan="1" colspan="2"> {gt:::md_to_html(x)}   </td>')
  }), collapse = " ")

  code %<>% paste(row, sums, "</tr>", collapse = "")

  if (R2_change == TRUE) {
    delta_R2 <- purrr::map_chr(R2s, function(x) {
      x %>%
        dplyr::pull(.data$r.squared)
    }) %>%
      as.numeric() %>%
      diff() %>%
      fmt_cor()
    
    rows <- ""

    for (i in seq_along(delta_R2)) {
      
      x <- stats::anova(mod[[i]], mod[[i + 1]])
      F_test <- glue::glue("<em>F</em>({x$Df[2]}, {x$Res.Df[2]}) = {x$F[2] %>% round_(2)}, <em>p</em> {x$`Pr(>F)`[2] %>% fmt_p()}")
  
      if (i == 1) {
        row <- glue::glue('<tr>
        <td class="gt_row gt_left" rowspan="1" colspan="1"><em>Change</em></td>
        <td class="gt_row gt_center" rowspan="1" colspan="4">&Delta;<em>R</em><sup>2</sup> =
                         {delta_R2[i]}, {F_test} </td></tr>')
      } else {
        row <- glue::glue('<tr>
        <td class="gt_row gt_left" rowspan="1" colspan="{1+(i-1)*2}">&nbsp;</td>
        <td class="gt_row gt_center" rowspan="1" colspan="4">&Delta;<em>R</em><sup>2</sup> =
                         {delta_R2[i]}, {F_test} </td></tr>')
        
      }

    rows %<>% paste(row, collapse = "")
    }
    
    code %<>% paste(rows, collapse = "")
    }

  temp_file <- tempfile()
  tab %>%
    htmltools::as.tags() %>%
    htmltools::save_html(temp_file)
  code <- readr::read_file(temp_file) %>% stringr::str_replace("</tbody>", paste(code, "</tbody>"))

  special_replace <- c("&beta;", "&dagger;", "&nbsp;") %>%
    magrittr::set_names(c(
      "\\u03b2", stringi::stri_unescape_unicode("\\u2020"),
      stringi::stri_unescape_unicode("\\u00a0")
    ))

  code %<>% stringr::str_replace_all(special_replace)

  out <- list(gt_tab = tab, html_code = code)
  out %<>% add_class("timesaveR_raw_html")

  if (!is.null(filename)) {
    readr::write_file(code, filename)
    return(invisible(out))
  }
  out
}

.lm_F_test <- function(mod) {
  if ("lm" %in% class(mod$analyses[[1]])) {
    return(mira.lm_F_test(mod))
  }
  model_summary <- summary(mod)
  f.stat <- model_summary$fstatistic[1]
  DoF <- model_summary$fstatistic[2]
  DoF_residual <- model_summary$fstatistic[3]

  p_value <- stats::pf(f.stat, DoF, DoF_residual,
    lower.tail = FALSE
  )

  fmt <- "%.2f"

  paste0(
    "*F*(", DoF, ", ", DoF_residual, ") = ", sprintf(fmt, f.stat), ", *p* ",
    fmt_p(p_value)
  )
}
#' Report F-test for significance of multiply imputed lm models
#'
#' Takes a mira object (list of lm models based on mice imputations) and returns
#' an F-test for their significance, based on \code{\link[miceadds]{micombine.F}}
#'
#' @param mod A mira object (list of lm models in `analyses` element)
#' @param return_list Logical. Should items of test be returned in a list?
#' Otherwise, a string for reporting is returned, with Markdown formatting for APA style
#' @export

mira.lm_F_test <- function(mod, return_list = FALSE) {
  .check_req_packages(c("miceadds"))
  extract_F <- function(x) {
    summary(x) %>%
      magrittr::extract2("fstatistic") %>%
      magrittr::extract(1)
  }
  Fs <- purrr::map_dbl(mod$analyses, extract_F)

  DoF <- summary(mod$analyses[[1]])$fstatistic[2]

  f.statistics <- miceadds::micombine.F(Fs, df1 = DoF, display = FALSE)
  f.stat <- f.statistics["D.numdf"]

  DoF_residual <- summary(mod$analyses[[1]])$fstatistic[3]

  p_value <- f.statistics["p.numdf"]

  if (return_list) {
    return(list(F = f.stat, DoF = DoF, DoF_residual = DoF_residual, p.value = p_value))
  }

  fmt <- "%.2f"

  paste0(
    "*F*(", DoF, ", ", DoF_residual, ") = ", sprintf(fmt, f.stat), ", *p* ",
    fmt_p(p_value)
  )
}

#' Create a summary table comparing standardized and non-standardized
#' proportional odd logistic regression models
#'
#' This function creates a summary table for polr models (including mice::mira objects
#' containing polr-models) that shows a standardized and non-standardized version of the model
#' side-by-side. Several pairs of such models can be compared side-by-side.
#' 
#' The R2 shown is the maximum likelihood pseudo R2 returned by \link[pscl]{pR2}.
#'
#' @param mod A polr-model/mira object of polr models, with variables not standardized (or a list of such models)
#' @param mod_std A polr-model/mira object of polr models, with standardized predictor variables (or a list of such models)
#' @param conf_level Confidence level to use for confidence intervals, defaults to .95
#' @param OR Logical. Should odds ratios be shown instead of typical coefficients. If TRUE, estimates are exponentiated
#' @param filename the file name to create on disk. Include '.html' extension to best preserve formatting (see gt::gtsave for details)
#' @param model_names If several pairs of models are to be plotted side by side, indicate the label for each *pair* here
#' @param show_nimp Logical. If mira objects are passed, this determines whether the number of imputations will be reported as a model statistic
#' @param notes List of notes to append to bottom of table. An explanation of significance stars is automatically added. A note is also added
#' stating that dummy variables were not scaled in standardization. If you approached standardisation differently, that should be removed.
#' @param apa_style Logical, should APA-style formatting be applied
#' @param stars Named vector of significance stars and their thresholds, check `timesaveR:::std_stars_pad` for default.
#' @param statistic_vertical Should standard errors and CIs be shown below coefficients? Defaults to horizontal layout
#' @inheritParams modelsummary::modelsummary
#' @inheritDotParams modelsummary::modelsummary -models -statistic -conf_level -stars
#' @examples 
#' library(MASS)
#' pov_att <- polr(poverty ~ religion + age + gender, data = WVS)
#' pov_att_std <- polr_std(poverty ~ religion + age + gender, data = WVS)
#' report_polr_with_std(pov_att, pov_att_std, coef_omit = "\\|")
#' @export

report_polr_with_std <- function(mod, mod_std, OR = TRUE, conf_level = .95, fmt = "%.2f", statistic_vertical = FALSE, filename = NULL, model_names = NULL, show_nimp = FALSE, notes = list(), apa_style = TRUE, stars = std_stars_pad, ...) {
  .check_req_packages(c("modelsummary", "gt", "htmltools", "readr", "pscl"))

  if (("list" %in% class(mod) || "list" %in% class(mod_std)) && !(length(mod) == length(mod_std))) {
    stop("Same number of models need to be included in mod and mod_std arguments")
  }

  if (!("list" %in% class(mod))) mod <- list(mod)
  if (!("list" %in% class(mod_std))) mod_std <- list(mod_std)

  if (!is.null(model_names) && !length(model_names) == length(mod)) {
    stop("Length of model names needs to be the same as length of model")
  }

  if ("tsR_std" %in% class(mod_std[[1]]) || ("mira" %in% class(mod_std[[1]]) && "tsR_std" %in% class(mod_std[[1]]$analyses[[1]]))) {
    notes %<>% c("Given that dummy variables lose their interpretability when standardized, only continuous predictors are standardized.")
  }
  

  mods <- list()

  for (i in seq_len(length(mod))) {
    mods[[i * 2 - 1]] <- mod[[i]]
    mods[[i * 2]] <- mod_std[[i]]
  }

  names(mods) <- paste0("Model", seq_len(length(mods)))

  if (OR) {
    col_labels <- rep(list(gt::md("*<center>OR [95% CI]</center>*"), gt::md("*<center>Stand. OR [95% CI]</center>*")), times = length(mod)) %>% stats::setNames(names(mods))
  } else {
    col_labels <- rep(list(gt::md("*<center>Coefs [95% CI]</center>*"), gt::md("*<center>Stand. Coefs [95% CI]</center>*")), times = length(mod)) %>% stats::setNames(names(mods))
  }

  notes %<>% c(.make_stars_note())

  if (statistic_vertical) {
    tab <- modelsummary::msummary(mods, output = "gt", estimate = "{estimate} {stars}", 
                                  statistic = "[{conf.low}, {conf.high}]", fmt = fmt, 
                                  gof_omit = ".*", stars = stars, p.values = TRUE, exponentiate = OR, ...) %>%
      gt::fmt_markdown(columns = gt::everything()) %>%
      gt::cols_label(.list = col_labels) %>%
      gt::cols_align("right", gt::everything()) %>%
      gt::cols_align("left", columns = 1) %>%
      gt:::dt_source_notes_set("") # Remove std star note
  } else {
    tab <- modelsummary::msummary(mods, output = "gt", statistic = NULL, 
                                  estimate = "{estimate} {stars} [{conf.low}, {conf.high}]", 
                                  fmt = fmt, gof_omit = ".*", stars = stars, p.values = TRUE, exponentiate = OR, ...) %>%
      gt::fmt_markdown(columns = gt::everything()) %>%
      gt::cols_label(.list = col_labels) %>%
      gt::cols_align("right", gt::everything()) %>%
      gt::cols_align("left", columns = 1) %>%
      gt:::dt_source_notes_set("") # Remove std star note
  }
  if (apa_style) tab <- tab %>% gt_apa_style()

  for (i in seq_along(notes)) {
    tab <- tab %>% gt::tab_source_note(gt::md(notes[[i]]))
  }


  if (length(mod) > 1) {
    if (is.null(model_names)) model_names <- paste0("Model ", seq_len(length(mod)))
    for (i in seq_len(length(mod))) {
      current_cols <- c((2 * i):(2 * i + 1))
      tab <- tab %>% gt::tab_spanner(gt::md(paste0("**", model_names[i], "**")), columns = current_cols)
    }
  }

  code <- character()


  row <- '<tr style="border-top-style: solid; border-top-width: 2px;">
    <td class="gt_row gt_left" rowspan="1" colspan="1">  <em>N     </td>'

  Ns <- purrr::map(mod, ~broom::glance(.x)["nobs"])
  
  sums <- paste(purrr::map(Ns, function(x) {
    glue::glue('<td class="gt_row gt_center" rowspan="1" colspan="2"> {x$nobs}   </td>')
  }), collapse = " ")

  code %<>% paste(row, sums, "</tr>", collapse = "")

  row <- '<tr>
    <td class="gt_row gt_left" rowspan="1" colspan="1">  <em>R<sup>2</sup>     </td>'

  R2s <- numeric()
  
  for (i in seq_along(mod)) {
    if (any(c("glm", "polr", "multinorm") %in% class(mod[[i]]))) {
      R2s[i] <- pscl::pR2(mod[[i]]) %>% magrittr::extract("r2ML")
    } else if ("mira" %in% class(mod[[i]])) {
      R2s[i] <- mean(purrr::map_dbl(mod[[i]]$analyses, function(x) pscl::pR2(x) %>% magrittr::extract("r2ML")))
    }
  }

  sums <- paste(purrr::map(R2s, function(x) {
    glue::glue('<td class="gt_row gt_center" rowspan="1" colspan="2"> {fmt_cor(as.numeric(x))}   </td>')
  }), collapse = " ")

  code %<>% paste(row, sums, "</tr>", collapse = "")


  temp_file <- tempfile()
  tab %>%
    htmltools::as.tags() %>%
    htmltools::save_html(temp_file)
  code <- readr::read_file(temp_file) %>% stringr::str_replace("</tbody>", paste(code, "</tbody>"))

  special_replace <- c("&beta;", "&dagger;", "&nbsp;") %>%
    magrittr::set_names(c(
      "\\u03b2", stringi::stri_unescape_unicode("\\u2020"),
      stringi::stri_unescape_unicode("\\u00a0")
    ))

  code %<>% stringr::str_replace_all(special_replace)

  out <- list(gt_tab = tab, html_code = code)
  class(out) <- c("timesaveR_raw_html", class(out))


  if (!is.null(filename)) {
    readr::write_file(code, filename)
    return(invisible(out))
  }
  return(out)
}

tidy_custom.polr <- function(x, ...) tidy(x, p.values = TRUE, ...)

#' @importFrom broom glance
#' @importFrom broom tidy
#' @export
broom::glance
broom::tidy

#' Tidy  multiple imputation models created with `mice`
#'
#' Note that the `mice` authors prefer to tidy `mipo` rather than `mira` objects and have now included `tidy.mipo` and `glance.mipo` into their package. The `mira` functions here are mostly retained for compatibility with my earlier code.
#'
#' @param x A `mira` object containing multiple models based on `mice` imputations.
#' @param conf.int Logical. Should confidence intervals be returned. Defaults to true.
#' @param conf.level Confidence level for intervals. Defaults to .95
#' @param ... extra arguments (not used)
#' @note
#' Available stats in result:
#' \itemize{
#'      \item estimate
#'      \item ubar
#'      \item b
#'      \item t
#'      \item dfcom
#'      \item df
#'      \item riv
#'      \item lambda
#'      \item fmi
#'      \item p.value
#'      \item conf.low (if called with conf.int = TRUE)
#'      \item conf.high (if called with conf.int = TRUE)
#' }
#' @export
tidy.mira <- function(x, conf.int = TRUE, conf.level = .95, ...) {
  out <- summary(mice::pool(x, ...), type = "all", conf.int = conf.int, conf.level = conf.level) %>%
    dplyr::mutate(term = as.character(.data$term)) %>%
    tibble::as_tibble()
  conf_vars <- names(out)[stringr::str_detect(names(out), "%")]
  names(out)[names(out) %in% conf_vars] <- c("conf.low", "conf.high")
  out <- out %>% dplyr::select(.data$term, order(names(.)))
  return(out)
}

#' Glance a multiple imputation `mice` pooled object
#'
#' Note that the `mice` authors prefer to tidy `mipo` rather than `mira` objects and have now included `tidy.mipo` and `glance.mipo` into their package. The `mira` functions here are mostly retained for compatibility with my earlier code.
#'
#' @param x An object with multiply-imputed models from `mice` (class: `mira`)
#' @param ... extra arguments (not used)
#' @return a tibble with one row
#'
#' @note If x contains `lm` models, R2 is included in the output
#'
#' @examples
#' \dontrun{
#' library(mice)
#' data <- airquality
#' data[4:10, 3] <- rep(NA, 7)
#' data[1:5, 4] <- NA
#' tmp <- mice(data, m = 5, seed = 500, printFlag = FALSE)
#' mod <- with(tmp, lm(Ozone ~ Solar.R + Wind))
#' glance(mod)
#' }
#' @export
glance.mira <- function(x, ...) {
  out <- tibble::tibble("nimp" = length(x$analyses))
  out$nobs <- tryCatch(stats::nobs(x$analyses[[1]]), error = function(e) NULL)
  if (class(x$analyses[[1]])[1] == "lm") {
    out$r.squared <- mice::pool.r.squared(x, adjusted = FALSE)[1]
    out$adj.r.squared <- mice::pool.r.squared(x, adjusted = TRUE)[1]
  }
  return(out)
}

#' Helper function to enable tidiers to be used on standardized models
#'
#' Strips tsR_std class and calls tidy() again 
#' (for use with lm_std() and polr_std() outputs)
#' 
#' @param x An object with class tsR_std
#' @param ... arguments passed on to tidy method
#' @export

tidy.tsR_std <- function(x, ...) {
  class(x) <- class(x)[class(x) != "tsR_std"]
  generics::tidy(x, ...)
}

#' Helper function to style gt-table in APA style
#'
#' This function takes a `gt` table object and changes font-type, borders etc
#' to align with APA style.
#'
#' @param gt_table A gt-table
#' @param fmt_labels_md Should row and column labels be formatted with markdown/HTML (Defaults to TRUE)
#' @source Created by Philip Parker, https://gist.github.com/pdparker/1b61b6d36d09cb295bf286a931990159. Slightly expanded here.
#' @export


gt_apa_style <- function(gt_table, fmt_labels_md = TRUE) {
  out <- gt_table %>%
    gt::opt_table_lines(extent = "none") %>%
    gt::tab_options(
      heading.border.bottom.width = 2,
      heading.border.bottom.color = "black",
      heading.border.bottom.style = "solid",
      table.border.top.color = "white",
      table_body.hlines.color = "white",
      table_body.border.top.color = "black",
      table_body.border.top.style = "solid",
      table_body.border.top.width = 1,
      heading.title.font.size = 12,
      table.font.size = 12,
      heading.subtitle.font.size = 12,
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = 1,
      table_body.border.bottom.style = "solid",
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = 1
    ) %>%
    gt::opt_table_font(font = "times")

  if (fmt_labels_md) out <- fmt_labels_md(out)
  out
}

#' A convenience function to render markdown to html in row and column labels
#'
#' @param tab a `gt` table object
#' @param position character string determines wither row, column or both
#'   labels should be rendered.
#' @note This function only works for HTML output, since the `gt` render tools
#' are less developed for LaTeX and RTF output.
#' @source Developed with Vincent Arel-Bundock and first included in `modelsummary`-package
#' @keywords internal

fmt_labels_md <- function(tab, position = c("both", "row", "column")) {
  out <- tab
  if (match.arg(position) %in% c("both", "row")) {
    out <- gt::fmt_markdown(out, columns = 1)
  }
  if (match.arg(position) %in% c("both", "column")) {
    f <- function(x) stats::setNames(lapply(unlist(x$`_boxhead`$column_label), gt::md), names(x$`_data`))
    out <- gt::cols_label(out, .list = f(out))
  }
  return(out)
}
