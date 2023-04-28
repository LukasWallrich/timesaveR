#' Calculates a partial correlation matrix controlling for one or more variables
#'
#' This returns a matrix aligned with the `cor_matrix()` function after parceling
#' out the effect of one or more other variables.
#' 
#' @inheritParams cor_matrix 
#' @param given A character vector with one or multiple variables in data. It/they will be parceled out from all other variables in data,
#' before the correlation table is calculated.
#' @inheritDotParams cor_matrix
#' @export 

pcor_matrix <- function(data, given, ...) {
  
  data %<>% dplyr::select_if(is.numeric)
  
  for (v in setdiff(names(data), given)) {
    formula <- formula(paste(v, "~", paste(given, collapse = " + ")))
    data[[v]] <- stats::residuals(lm(formula, data))
  }
  
  data %<>% dplyr::select(-dplyr::all_of(given))
  
  class(data) <- c("resid_df", class(data))
  
  cor_matrix(data, ...)
  
}

