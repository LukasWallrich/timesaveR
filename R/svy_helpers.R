#' Create overview over missing data in survey object
#'
#' This function creates a tibble that summarises missing data 
#' with survey weights for missing counts and proportions. (Apart from
#' that, it recreates the \code{naniar::miss_var_summary()}
#' output.)
#'
#' @param svy_df A survey object created with the survey package
#' @param ... Variables to consider. By default this looks at the whole dataset. 
#' Otherwise, this should be one or more unquoted expressions separated by commas, or
#' a tidyselect helper function (e.g., `starts_with()` or `where(is.numeric)`). 
#' @param .any_missing Should a row be shown at the start of the return with the
#' number and share of (weighted) responses that have missing data on at least one
#' of the variables considered? Defaults to TRUE.
#' @param .include_complete Should variables with no missing data be included
#' in the returned tibble? Defaults to FALSE.
#' @examples
#' library(survey)
#' data(api)
#' svy_df <- svydesign(id = ~1, strata = ~stype, weights = ~pw, 
#'                    data = apistrat, fpc = ~fpc)
#'
#' svy_miss_var_summary(svy_df, acs.core, target, name)    

#' @export

svy_miss_var_summary <- function(svy_df, ..., .any_missing = TRUE, .include_complete = FALSE) {
  .check_req_packages(c("survey"))

  assert_class(svy_df, "survey.design")
  
  svy_df <- srvyr::as_survey(svy_df)

  # Complications to enable use of tidyselect functions
  if (missing(...)) vars <- rlang::expr(dplyr::everything())
  if (!missing(...)) vars <- rlang::expr(c(...))

  loc <- tidyselect::eval_select(vars, svy_df$variables)
  vars <- rlang::syms(names(svy_df$variables)[loc])

  any_miss_fun <- function(x) {
    apply(data.frame(x), MARGIN = 1, FUN = function(x) anyNA(x))
  }

  any_miss <- svy_df$variables %>%
    dplyr::select(!!!vars) %>%
    dplyr::mutate(any_miss_vars = any_miss_fun(.)) %>%
    dplyr::pull()
  
  svy_df_NAs <- svy_df %>% srvyr::mutate(srvyr::across(c(!!!vars), is.na))

  svy_df_NAs$variables$.any_missing <- any_miss


  res <- purrr::map_dfr(vars, function(x) {
    tbl <- survey::svytable(as.formula(paste0("~", rlang::as_name(x))), svy_df_NAs)
    tibble::tibble(
      variable = rlang::as_name(x), n_miss = round(tbl["TRUE"]),
      pct_miss = prop.table(tbl)["TRUE"]
    )
  })

  tbl <- survey::svytable(as.formula("~.any_missing"), svy_df_NAs)

  res[is.na(res)] <- 0
  res %<>% dplyr::arrange(dplyr::desc(.data$n_miss))

  if(!.include_complete) res %<>% dplyr::filter(.data$n_miss>0)
  
  if (.any_missing) {
    res <- dplyr::bind_rows(tibble::tibble(
      variable = ".any_missing", n_miss = round(tbl["TRUE"]),
      pct_miss = prop.table(tbl)["TRUE"]
    ), res)
  }

  res
}
