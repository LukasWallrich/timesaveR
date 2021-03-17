#' Create a summary table for categorical variables and their
#' relationship with a continuous outcome variable
#'
#' This takes a set of categorical variables and a continuous dependent variable
#' and produces a table that shows the distribution of data across the levels of
#' the categorical variables, as well as the mean of the dependent variable for
#' each level and the significance of pairwise comparisons between these means.
#'
#' @param df Dataframe containing the variables specified
#' @param dv The continuous dependent variable to be presented alongside the
#' levels of categorical variables
#' @param ... Categorical variables to be included
#' @param var_names Tibble of old and new variable names, if variables are to be
#' renamed for display. See \code{\link{get_rename_tribbles}} for required format
#' @param level_names Tibble of old and new level names, if levels are to be
#' renamed for display. See \code{\link{get_rename_tribbles}} for required format
#' @param alpha_level The level of significance for the pairwise comparisons
#' (after p.adjust). Defaults to .05
#' @param p_adjust One of p_adjust.methods, defaults to Holm
#' @param notes List of notes to append to bottom of table.
#' @param dv_name Optional. A different name to use for the dependent variable
#' in the automatic table footnote explaining the M(SD) column. Defaults to dv.
#' @param bold_vars Should rows with variable names be bold. Defaults to TRUE
#' @param apa_style Logical, should APA-style formatting be applied
#' @param css_tags List of css tags to be added, each named with the class that
#' the tag should be added to.
#' @param na.rm Should missing values be dropped in the dv when summary statistics are calculated?
#' @param exclude_na Should cases that are NA on a categorical variable be dropped from that section?
#' @param filename If provided, the table will be saved as an HTML file with this name.
#' @return A list including a tibble of descriptive statistics (`descr`), the
#' `gt`-table (`tab`) and the HTML code (`html_code`) with `css_tags` added
#' @examples
#'
#' report_cat_vars(iris, Sepal.Length, Species)
#'
#' # You can rename variables and levels reasonably easily
#' # Use get_rename_tribbles() to get the basis of these tibbles
#'
#' var_renames <- tibble::tribble(
#'  ~old,     ~new,     
#'  "gndr",   "Gender",  
#'  "cntry",  "Country"
#'  )
#'
#' level_renames <- tibble::tribble(
#'  ~var,     ~level_old, ~level_new, 
#'  "gndr",   "1",        "male",       
#'  "gndr",   "2",        "female",       
#'  "cntry",  "DE",       "Germany",      
#'  "cntry",  "FR",       "France",      
#'  "cntry",  "GB",       "UK"
#'  )
#'  
#' report_cat_vars(ess_health, health, gndr, cntry, var_names = var_renames, 
#'              level_names = level_renames)
#' @export

# TK - implement dv = NULL to only show distribution

report_cat_vars <- function(df, dv, ..., var_names = NULL, level_names = NULL,
                          p_adjust = p.adjust.methods, alpha_level = .05,
                          filename = NULL, notes = list(), dv_name = NULL,
                          bold_vars = TRUE, apa_style = TRUE, css_tags = list(), 
                          na.rm = TRUE, exclude_na = FALSE) {
  assert_data_frame(df)
  if (!is.null(var_names)) {
    assert_data_frame(var_names)
    assert_names(names(var_names), must.include = c("old", "new"))
  }
  if (!is.null(level_names)) {
    assert_data_frame(level_names)
    assert_names(names(level_names), must.include = c("level_old", "level_new"))
  }

  assert(p_adjust[1] %in% p.adjust.methods)

  assert_character(dv_name, null.ok = TRUE)

  qassert(alpha_level, "R1(0,1)")
  qassert(bold_vars, "B1")
  qassert(na.rm, "B1")
  qassert(exclude_na, "B1")

  assert(
    check_null(css_tags),
    check_list(css_tags, names = "named")
  )

  vars <- rlang::enquos(...)
  dv <- rlang::enquo(dv)

  df <- rename_cat_variables(df, ..., var_names = var_names, level_names = level_names)

  if (!is.null(var_names)) {
    var_names_chr <- var_names$new
    names(var_names_chr) <- var_names$old

    vars <- purrr::map(vars, function(x) {
      x %>%
        dplyr::as_label() %>%
        stringr::str_replace_all(var_names_chr) %>%
        rlang::sym()
    })
  }

  descr <- purrr::map(vars, function(x) {
    df %>%
      dplyr::rename(level = !!x) %>%
      dplyr::filter(!exclude_na | !is.na(.data$level)) %>%
      dplyr::group_by(.data$level) %>%
      dplyr::summarise(
        M = mean(!!dv, na.rm = na.rm),
        SD = sd(!!dv, na.rm = na.rm),
        N = dplyr::n()
      ) %>%
      dplyr::mutate(
        Share = .data$N / sum(.data$N),
        group_var = rlang::as_name(x),
        level = as.character(.data$level)
      )
  })

  tests <- purrr::map(vars, function(x) {
    stats::pairwise.t.test(df %>% dplyr::select(!!dv) %>% dplyr::pull(),
      df %>% dplyr::select(!!x) %>% dplyr::pull(),
      p.adjust.method = p_adjust[1]
    ) %>%
      get_pairwise_letters(alpha_level = alpha_level) %>%
      dplyr::select(.data$level, .data$letters)
  })

  descr <- purrr::map2(descr, tests, function(x, y) {
    dplyr::left_join(x, y, by = "level") %>%
      dplyr::select(
        .data$group_var, .data$level, .data$N, .data$Share, .data$M,
        .data$SD, .data$letters
      )
  }) %>%
    purrr::map_dfr(rbind)

  descr_formatted <- descr %>%
    dplyr::mutate(
      `*M (SD)*` = paste0(round_(.data$M, 2), " (", round_(.data$SD, 2), ")"),
      N = round_(.data$N), `*M (SD)*` = paste0(.data$`*M (SD)*`, " <sup>", .data$letters, "</sup>")
    ) %>%
    dplyr::select(.data$group_var, .data$level, .data$N, .data$Share, .data$`*M (SD)*`)

  f <- function(x) {
    stats::setNames(lapply(
      names(x$`_data`),
      gt::md
    ), names(x$`_data`))
  }
  tab <- descr_formatted %>%
    gt::gt(rowname_col = "level", groupname_col = "group_var") %>%
    gt::fmt_markdown(columns = dplyr::everything()) %>%
    gt::fmt_percent(columns = "Share", decimals = 1) %>%
    gt::cols_label(.list = f(.))

  if (bold_vars) {
    tab <- tab %>% gt::tab_style(
      style = list(gt::cell_text(weight = "bold")),
      locations = gt::cells_row_groups()
    )
  }

  auto_notes <- list()

  if (is.null(dv_name)) dv_name <- dplyr::as_label(dv)

  auto_notes %<>% c(glue::glue("*M* and *SD* are used to represent mean and \\
                               standard deviation for {dv_name} for that group, \\
                               respectively.<br>"))

  p_note <- ifelse(p_adjust[1] == "none", "", glue::glue("(*p*-values were \\
                              adjusted using the {stringr::str_to_sentence(p_adjust[1])}-method.)"))

  auto_notes %<>% c(glue::glue("Within each variable, the means of groups with \\
                               different superscripts differ with *p* < .05 <br> {p_note}"))

  notes <- c(auto_notes, notes)

  for (i in seq_along(notes)) {
    tab <- tab %>% gt::tab_source_note(gt::md(notes[[i]]))
  }
  
  if (apa_style) tab <- tab %>% gt_apa_style()
  
  temp_file <- tempfile()
  tab %>%
    htmltools::as.tags() %>%
    htmltools::save_html(temp_file)
  code <- readr::read_file(temp_file)

  for (i in seq_along(css_tags)) {
    code <- .add_css(code, names(css_tags)[i], css_tags[[i]])
  }

  out <- list(descr = descr, tab = tab, html_code = code)
  out %<>% add_class("timesaveR_raw_html")

  if (!is.null(filename)) {
    readr::write_file(code, filename)
    return(invisible(out))
  }
  out
}

.add_css <- function(code, class, add) {
  css <- paste0("\n", add, ";\n\n")
  class %<>% stringr::str_replace(stringr::fixed("."), stringr::fixed("\\."))
  stringr::str_replace(
    code, paste0("(", class, " \\{[\\s\\S]*?(?=\\}))"),
    paste0("\\1", css)
  )
}
