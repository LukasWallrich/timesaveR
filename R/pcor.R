#' Calculates a partial correlation matrix controlling for one or more variables
#'
#' This returns a matrix aligned with the `cor_matrix()` function after parceling
#' out the effect of one or more other variables. This function requires complete data;
#' incomplete cases are dropped with a warning.
#' 
#' @inheritParams cor_matrix 
#' @param given A character vector with one or multiple variables in data. It/they will be parceled out from all other variables in data,
#' before the correlation table is calculated.
#' @inheritDotParams cor_matrix -missing
#' @examples
#' 
#' # One might want to estimate correlations between health and possible predictors in the ESS
#' # after parceling out / controling for key demographic attributes:
#' pcor_matrix(ess_health, given = c("agea", "gndr"), 
#'    var_names = c("health" = "Health", "weight" = "Weight", "dosprt" = "Sport")) %>% 
#'    tidy(both_directions = FALSE)
#' 
#' @export 

pcor_matrix <- function(data, given, ...) {
  
  args <- list(...)
  
  data %<>% dplyr::select_if(is.numeric)

  if (!is.null(args$var_names)) {
    if (is.data.frame(args$var_names)) {
      assert_names(names(var_names), must.include = c("old", "new"))
      var_names <- args$var_names$old
    } else {
      var_names <- names(args$var_names)
    }
    data %<>% dplyr::select(dplyr::any_of(c(given, var_names)))
  }
  
  if (!is.null(args$missing)) {
   stop("`missing` argument cannot be set - pcor_matrix only supports listwise deletion.") 
  }
  
  if (anyMissing(data)) {
    full <- nrow(data)
    data <- data %>% tidyr::drop_na()
    warning("Dropped ", full - nrow(data), " rows with missing data.")
  }
  
  for (v in setdiff(names(data), given)) {
    formula <- formula(paste(v, "~", paste(given, collapse = " + ")))
    data[[v]] <- stats::residuals(lm(formula, data))
  }
  
  data %<>% dplyr::select(-dplyr::all_of(given))
  
  class(data) <- c("resid_df", class(data))
  
  args$data <- data
  
  do.call(cor_matrix, args)
  
}

