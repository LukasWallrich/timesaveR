#' Report (model comparison) ANOVA tests
#'
#' This is under development - contributions to support more types of ANOVAs are most welcome.
#'
#' @param x An object returned from an anova() function (or car::linearHypothesis)
#' 
#' @return A character string reporting the anova in APA style
#' @examples 
#' mod1 <- lm(mpg ~ wt + am, mtcars)
#' mod2 <- lm(mpg ~ wt * am, mtcars)
#' report_anova(anova(mod1, mod2))
#' @export

report_anova <- function(x) {

    if (is.null(attributes(x)$heading)) 
  cli::cli_abort("This type of comparison is not yet supported by this function. You are welcome to request its addition or propose an implementation.")      

  if (nrow(x) != 2) cli::cli_abort("Currently, {.fn report_anova} only supports comparisons between two models - so {.arg x} should have two rows.")

  # Likely from anova() of two lm() models
  if (stringr::str_detect(attributes(x)$heading[1], "Analysis of Variance Table")) {
    return(glue::glue("<em>F</em>({x$Df[2]}, {x$Res.Df[2]}) = {x$F[2] %>% round_(2)}, <em>p</em> {x$`Pr(>F)`[2] %>% fmt_p()}"))
  }
  
  # Likely from car::linearHypothesis()
  if (stringr::str_detect(attributes(x)$heading[1], "Linear hypothesis test")) {
    return(glue::glue("<em>F</em>({x$Df[2]}, {x$Res.Df[2]}) = {x$F[2] %>% round_(2)}, <em>p</em> {x$`Pr(>F)`[2] %>% fmt_p()}"))
  }
  
  # Likely from anova() of two lavaan models
  if (stringr::str_detect(attributes(x)$heading[1], "Chi-Squared Difference Test")) {
    return(glue::glue("<em>&chi;</em><sup>2</sup>({x$`Df diff`[2]}) = {x$`Chisq diff`[2] %>% round_(2)}, <em>p</em> {x$`Pr(>Chisq)`[2] %>% fmt_p()}"))
  }  
  
  cli::cli_abort("This type of comparison is not yet supported by this function. You are welcome to request its addition or propose an implementation.")      
      
}
