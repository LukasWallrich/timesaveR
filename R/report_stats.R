#' Report (model comparison) ANOVA tests
#'
#' This is under development and thus not yet exported.
#'
#' @param x An object returned from an anova() function
#' 
#' @return A character string reporting the anova in APA style
#' @keywords internal

report_anova <- function(x) {

    if (is.null(attributes(x)$heading)) 
  stop("This type of comparison is not yet supported by this function. You are welcome to request its addition or propose an implementation.")      

  if (nrow(x) != 2) stop("Currently, report_anova only supports comparisons between two models - so x should have two rows.")

  # Likely from anova() of two lm() models
  if (stringr::str_detect(attributes(x)$heading[1], "Analysis of Variance Table")) {
    return(glue::glue("<em>F</em>({x$Df[2]}, {x$Res.Df[2]}) = {x$F[2] %>% round_(2)}, <em>p</em> {x$`Pr(>F)`[2] %>% fmt_p()}"))
  }
  
  # Likely from anova() of two lavaan models
  if (stringr::str_detect(attributes(x)$heading[1], "Chi-Squared Difference Test")) {
    return(glue::glue("<em>&chi;</em><sup>2</sup>({x$`Df diff`[2]}) = {x$`Chisq diff`[2] %>% round_(2)}, <em>p</em> {x$`Pr(>Chisq)`[2] %>% fmt_p()}"))
  }  
  
  stop("This type of comparison is not yet supported by this function. You are welcome to request its addition or propose an implementation.")      
      
}
