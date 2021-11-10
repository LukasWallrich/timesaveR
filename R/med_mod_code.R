#' Conduct (parallel) mediation analysis
#'
#' Runs mediation analysis with one or more parallel mediators (using lavaan). The
#' results can then be plotted with `plot_mediation()`.
#' 
#' Note that covariates (if given) are used in predicting each mediator and the outcome. The
#' coefficients for covariates are returned as an attribute to the main results,
#' given that they are not always reported. To access them, use 
#' `attr(res, "CV_coefficients")` if you have saved the return of this function in `res`
#' 
#'
#' @encoding UTF-8
#' @param df Dataframe 
#' @param X Predictor variable (all variables can be passed as character or 'bare')
#' @param Y Outcome variable - can be passed as character or 'bare'
#' @param Ms Mediator variable(s)
#' @param CVs Covariates (in predicting mediators and outcomes) 
#' @param standardized_all Should all coefficients (paths, direct and indirect effects) be standardised
#' @param conf.level The confidence level to be used for confidence intervals. Must be between 0 and 1 
#' (exclusive), defaults to .95, which corresponds to 95% confidence intervals.
#' @param mc_pvalues Should p-values for indirect effects be returned?
#' @param seed Random seed, set to get reproducible results. If you do not want to set a fixed seed,
#' you can use seed = sample(1:1e6, 1)
#' @param bootstraps Number of bootstraps, defaults to 5000.
#' @param ... Options passed on to lavaan::sem. 
#' @return Tibble with direct, total and indirect effects, based on bootstrap resamples. In addition, 'a' coefficients for paths from 
#' X to mediators and 'b' coefficients for paths from mediators to Y are returned. Coefficients for CVs
#' are returned as an attribute - see below.
#' 
#' @export
#' @examples
#' 
#' # Might link between depression and self-reported health be partly explained
#' # by reductions in physical activity level, when holding age constant?
#'  
#'  res <- run_mediation(ess_health, fltdpr, health, dosprt, agea, bootstraps = 100) 
#' 
#'  res
#'  
#'  attr(res, "CV_coefficients")
#'  
#' # NB: bootstraps = 100 only set to reduce running time - should be 1000+

run_mediation <- function(df, X, Y, Ms, CVs = NULL, standardized_all = TRUE,
                          conf.level = .95, mc_pvalues = TRUE, seed = 987654321,
                          bootstraps = 5000, ...) {
  .check_req_packages(c("lavaan"))


  # Convert arguments to strings
  X <- as.character(rlang::ensym(X))
  Y <- as.character(rlang::ensym(Y))

  Ms_string <- all.vars(rlang::enquo(Ms))
  if (length(Ms_string) > 0) Ms <- Ms_string

  CVs_string <- all.vars(rlang::enquo(CVs))
  if (length(CVs_string) > 0) CVs <- CVs_string
  CVs_string <- CVs

  if (is.null(CVs)) {
    CVs <- ""
  } else {
    CVs <- paste("+", paste(CVs, collapse = " + "))
  }
  # Run mediation model

  M_letter <- letters[1:length(Ms)]
  M_codes <- paste0("M", 1:length(Ms))

  mod <- c("    #Mediators", purrr::map2(Ms, M_letter, function(x, y) glue::glue("{x} ~ {y}*{X} {CVs}"))) %>%
    paste("    ", collapse = "\n") %>%
    paste(collapse = "\n", "\n", glue::glue("
      #Outcome
      {Y} ~ cdash*{X} + {paste0(M_letter, M_letter, '*', Ms, collapse=' + ')} {CVs}

       
       #Direct effects cond on moderator
      direct := cdash

")) %>%
    paste(paste(purrr::pmap(list(Ms, M_letter, M_codes), function(x, y, z) glue::glue("

       #Indirect effects of {x} 
      {z}.indirect := {y} * {y}{y}
   ")), collapse = "\n"), paste(glue::glue("
      
      #Total effects 
      total := direct + {purrr::map(M_codes, paste0, '.indirect') %>% paste(collapse = ' + ')}
      indirect_total := total - direct 
      #Named params
      {paste(purrr::map(M_letter, function(x) glue::glue('.{x} := {x}')), collapse = '\n')}
      {paste(purrr::map(M_letter, function(x) glue::glue('.{x}{x} := {x}{x}')), collapse = '\n')}
      
      
      "), collapse = "\n"), collapse = "\n")


  set.seed(seed)

  # Lavaan arguments - set defaults if not overridden in dots
  # Thanks to https://stackoverflow.com/a/35587633/10581449
  # capture ... in a list
  dots <- list(...)
  # default arguments with their values
  def.vals <- list(
    model = mod,
    estimator = "MLR",
    fixed.x = FALSE,
    missing = "direct",
    data = df,
    std.ov = standardized_all
  )
  # find elements in dots by names of def.vals. store those that are NULL
  ind <- unlist(lapply(dots[names(def.vals)], is.null))
  # fill empty elements with default values
  dots[names(def.vals)[ind]] <- def.vals[ind]
  # run model
  lavaansem <- function(...) {
    lavaan::sem(...)
  }


  fit <- do.call(lavaansem, dots)

  bs <- lavaan::bootstrapLavaan(fit, R = bootstraps, FUN = "coef", parallel = "snow") %>%
    data.frame()

  bs_CVs <- dplyr::select(bs, dplyr::matches(paste0(paste0("\\.", CVs_string), collapse = "|")), -dplyr::matches("\\.\\.")) %>%
    t() %>%
    data.frame()

  bs <- bs %>%
    dplyr::select(-dplyr::matches("\\."), -dplyr::matches("\\~")) %>%
    dplyr::rename(M_letter %>% magrittr::set_names(paste0("a_", M_letter))) %>%
    dplyr::rename(paste0(M_letter, M_letter) %>% magrittr::set_names(paste0("b_", M_letter)))

  char_mutate <- function(df, s) {
    q <- quote(dplyr::mutate(df, z = s))
    eval(parse(text = sub("z = s", s, deparse(q))))
  }

  purrr::map(M_letter, function(x) glue::glue("indirect_{x} = a_{x}*b_{x}")) %>% purrr::walk(function(x) {
    bs <<- char_mutate(bs, x)
  })

  bs <- char_mutate(bs, paste0("total = cdash + ", paste0("indirect_", M_letter, collapse = " + "))) %>% dplyr::rename(direct = cdash)


  res <- bs %>%
    t() %>%
    data.frame() %>%
    tibble::rownames_to_column("parameter") %>%
    tidyr::gather(-parameter, key = "rep", value = "coef") %>%
    dplyr::group_by(parameter) %>%
    dplyr::summarise(est = mean(coef), se = sd(coef), pvalue = ifelse(est > 0, mean(coef < 0) * 2, mean(coef > 0)) * 2, ci.lower = quantile(coef, (1 - conf.level) / 2), ci.upper = quantile(coef, 1 - (1 - conf.level) / 2)) %>%
    tidyr::separate(parameter, c("type", "mediator"), fill = "right") %>%
    dplyr::mutate(mediator = stringr::str_replace_all(mediator, Ms %>% magrittr::set_names(M_letter)))

  CV_res <- bs_CVs %>%
    tibble::rownames_to_column("parameter") %>%
    tidyr::gather(-parameter, key = "rep", value = "coef") %>%
    dplyr::group_by(parameter) %>%
    dplyr::summarise(est = mean(coef), se = sd(coef), pvalue = ifelse(est > 0, mean(coef < 0) * 2, mean(coef > 0)) * 2, ci.lower = quantile(coef, (1 - conf.level) / 2), ci.upper = quantile(coef, 1 - (1 - conf.level) / 2)) %>%
    tidyr::separate(parameter, c("DV", "CV"), fill = "right")

  attr(res, "CV_coefficients") <- CV_res

  res
}
