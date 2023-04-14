#' Calculates a partial correlation matrix controlling for one or more variables
#'
#' This returns a matrix aligned with the `cor_matrix()` function after parceling
#' out the effect of one or more other variables.
#' 
#' @inheritParams cor_matrix 
#' @param given One or multiple variables in data. It/they will be parceled out from all other variables in data,
#' before the correlation table is calculated.
#' @inheritDotParams cor_matrix
#' @export 

pcor_matrix <- function(data, given, ...) {
  
  data %<>% dplyr::select_if(is.numeric)
  
  given_names <- rlang::enexprs(given) %>% purrr::map_chr(rlang::as_name)
  
  for (v in setdiff(names(data), given_names)) {
    formula <- formula(paste(v, "~", paste(given_names, collapse = " + ")))
    data[[v]] <- residuals(lm(formula, data))
  }
  
  data %<>% dplyr::select(-dplyr::all_of(given_names))
  
  class(data) <- c("resid_df", class(data))
  
  cor_matrix(data, ...)
  
}

