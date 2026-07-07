#' Conduct (parallel) mediation analysis
#'
#' Runs mediation analysis with one or more parallel mediators (using the `lavaan`
#' package). The results can then be plotted with `plot_mediation()`.
#'
#' Note that covariates (if given) are used in predicting each mediator and the outcome. The
#' coefficients for covariates are returned as an attribute to the main results,
#' given that they are not always reported. To access them, use `attr(res, "CV_coefficients")`
#' if you have saved the return of this function in `res`. The lavaan code to estimate the model is
#' also returned as an attribute, access it with `attr(res, "lavaan_code")`.
#'
#' @details Multiple mediators (`Ms`) are always modeled as PARALLEL mediators - i.e., there
#' are no paths estimated between mediators (no serial mediation). If you need moderation of
#' the first stage (X -> M) and/or the direct path (X -> Y) with a single mediator, see
#' [run_moderated_mediation()] instead.
#'
#' @param data Data frame 
#' @param X Predictor variable (all variables should be passed 'bare' in tidyverse style)
#' @param Y Outcome variable 
#' @param Ms Mediator variable(s)
#' @param CVs Covariates (in predicting mediators *and* outcomes) 
#' @param standardized_all Logical. Should all coefficients (paths, direct and indirect effects) be standardized?
#' @param conf_level The confidence level to be used for confidence intervals. Must be between 0 and 1 
#' (exclusive), defaults to .95 (i.e. 95% confidence intervals).
#' @param seed Random seed. You should set this to get reproducible results. 
#' @param bootstraps Number of bootstraps, defaults to 5000.
#' @param cores Number of CPU cores to use for bootstrapping. Defaults to 1
#' (sequential processing); set higher to speed up bootstrapping on multi-core machines.
#' @param ... Options passed on to [lavaan::sem()]. 
#' @return Tibble with direct, total and indirect effects, based on bootstrap resamples. In addition, 'a' coefficients for paths from 
#' X to mediators and 'b' coefficients for paths from mediators to Y are returned. Coefficients for covariates
#' are returned as an attribute - see the example.
#' @export
#' @examples
#' # Might the link between depression and self-reported health be partly explained
#' # by reductions in physical activity level, when holding age constant?
#'  
#'  set.seed(4321)
#'  res <- run_mediation(ess_health, fltdpr, health, dosprt, agea, bootstraps = 50) 
#'  # Note that high number of bootstraps fails - in that case, CIs might not be reliable - 
#'  # this is likely due to the limited range of the variables here
#'  
#'  res
#'  
#'  attr(res, "CV_coefficients")
#'  
#' # NB: bootstraps = 50 only set to reduce running time - should be 1000+

run_mediation <- function(data, X, Y, Ms, CVs = NULL, standardized_all = TRUE,
                          conf_level = .95, seed = NULL,
                          bootstraps = 5000, cores = 1, ...) {

  .check_req_packages(c("lavaan"))

  # Check for specific lavaan version with bootstrap bug (only 0.6-13)
  if (utils::packageVersion("lavaan") == "0.6.13") {
    cli::cli_warn('lavaan 0.6-13 has a bootstrap bug that may cause errors with {.code estimator = "MLR"} and {.code missing = "direct"}.
            See {.url https://github.com/yrosseel/lavaan/issues/275}.
            Consider updating lavaan or specify {.code missing = "listwise"} or {.code estimator = "ML"} as a workaround.')
  }
  
  args <- as.list(match.call(expand.dots = TRUE))[-1]
  if ("conf.level" %in% names(args)) {
    cli::cli_abort("The confidence level needs to be specified as {.arg conf_level}, NOT {.arg conf.level}")
  }
    
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
  
  if (any(stringr::str_detect(c(X, Y, CVs, Ms), "__"))) 
    cli::cli_abort("This function does not support variable names that contain two {.code _} in a row. Please rename.")
  
  # Run mediation model

  M_letter <- letters[seq_along(Ms)]
  M_codes <- paste0("M", seq_along(Ms))

  mod <- c("    #Mediators", purrr::map2(Ms, M_letter, function(x, y) glue::glue("{x} ~ {y}*{X} {CVs}"))) %>%
    paste("    ", collapse = "\n") %>%
    paste(collapse = "\n", "\n", glue::glue("
      #Outcome
      {Y} ~ cdash*{X} + {paste0(M_letter, M_letter, '*', Ms, collapse=' + ')} {CVs}

       
       #Direct effects
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
    data = data,
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

  bs <- lavaan::bootstrapLavaan(fit, R = bootstraps, FUN = "coef",
                                parallel = ifelse(cores > 1, "snow", "no"),
                                ncpus = cores) %>%
    data.frame() %>% 
    # Drop unsuccessful bootstraps
    tidyr::drop_na()

  bs_CVs <- bs %>%
    dplyr::select(dplyr::matches(paste0(paste0("\\.", CVs_string, "$"), collapse = "|")), -dplyr::matches("\\.\\.")) %>%
    t() %>%
    data.frame()

  bs <- bs %>%
    dplyr::select(-dplyr::matches("\\."), -dplyr::matches("\\~")) %>%
    dplyr::rename(M_letter %>% magrittr::set_names(paste0("a__", M_letter))) %>%
    dplyr::rename(paste0(M_letter, M_letter) %>% magrittr::set_names(paste0("b__", M_letter)))

  # Execute dplyr::mutate command based on character string
  char_mutate <- function(data, s) {
    q <- quote(dplyr::mutate(data, z = s))
    eval(parse(text = sub("z = s", s, deparse(q))))
  }

  purrr::map(M_letter, function(x) glue::glue("indirect__{x} = a__{x}*b__{x}")) %>% 
    purrr::walk(function(x) {
    bs <<- char_mutate(bs, x)
  })

  bs <- bs %>% char_mutate(paste0("total = cdash + ", paste0("indirect__", M_letter, collapse = " + "))) %>% 
    dplyr::rename(direct = "cdash")

  res <- bs %>%
    t() %>%
    data.frame() %>%
    tibble::rownames_to_column("parameter") %>%
    tidyr::pivot_longer(cols = -"parameter", names_to = "rep", values_to = "coef") %>%
    dplyr::group_by(.data$parameter) %>%
    dplyr::summarise(est = mean(.data$coef), se = sd(.data$coef),
                     pvalue = ifelse(.data$est > 0, mean(.data$coef < 0) * 2, mean(.data$coef > 0) * 2),
                     ci.lower = quantile(.data$coef, (1 - conf_level) / 2),
                     ci.upper = quantile(.data$coef, 1 - (1 - conf_level) / 2)) %>%
    tidyr::separate("parameter", c("type", "mediator"), fill = "right", sep = "__") %>%
    dplyr::mutate(mediator = stringr::str_replace_all(.data$mediator, Ms %>% magrittr::set_names(M_letter)))

  CV_res <- bs_CVs %>%
    tibble::rownames_to_column("parameter") %>%
    tidyr::pivot_longer(cols = -"parameter", names_to = "rep", values_to = "coef") %>%
    dplyr::group_by(.data$parameter) %>%
    dplyr::summarise(est = mean(.data$coef), se = sd(.data$coef),
                     pvalue = ifelse(.data$est > 0, mean(.data$coef < 0) * 2, mean(.data$coef > 0) * 2),
                     ci.lower = quantile(.data$coef, (1 - conf_level) / 2),
                     ci.upper = quantile(.data$coef, 1 - (1 - conf_level) / 2)) %>%
    tidyr::separate("parameter", c("DV", "CV"), fill = "right", sep = "\\.")

  attr(res, "CV_coefficients") <- CV_res
  attr(res, "lavaan_code") <- mod

  res
}


#' Conduct first-stage (and, optionally, direct-path) moderated mediation analysis
#'
#' Runs a moderated mediation analysis with a single mediator (using the `lavaan`
#' package): the path from `X` to `M` (the *a*-path) is moderated by `W`, and,
#' optionally, so is the direct path from `X` to `Y` (the *c*-path). The path from
#' `M` to `Y` (the *b*-path) is not moderated. This is exactly the model drawn by
#' [plot_moderated_mediation()], and the result of this function can be passed
#' straight to `plot_moderated_mediation(data = ...)` to create an annotated
#' diagram of the results.
#'
#' `W` is assumed to be continuous and is mean-centered (or standardized, see
#' `standardized_all`) internally before the product term `X:W` is formed - this is
#' standard practice to keep the lower-order coefficients (`a`, `c`) interpretable
#' as conditional effects at the mean of the moderator, and to reduce (non-essential)
#' multicollinearity between the product term and its components.
#'
#' Note that this function only supports a single mediator, moderated first-stage and
#' (optionally) direct path - i.e. it implements the model most commonly described as
#' "moderated mediation" in introductory treatments (e.g. Hayes' PROCESS model 7 or 8).
#' For (unmoderated) mediation with one or more parallel mediators, see [run_mediation()].
#'
#' As with `run_mediation()`, covariates (if given) are used in predicting both `M` and
#' `Y`, and their coefficients are returned as an attribute - see `attr(res, "CV_coefficients")`.
#' The lavaan code used to estimate the model is returned as `attr(res, "lavaan_code")`.
#'
#' Since `coef()` (used internally to bootstrap the raw path coefficients) does not
#' return `lavaan`'s defined quantities (`:=`), the index of moderated mediation and the
#' conditional indirect/direct effects are calculated from the bootstrapped raw
#' coefficients directly in R - exactly analogous to how [run_mediation()] reconstructs
#' indirect effects from bootstrapped `a` and `b` coefficients.
#'
#' @param data Data frame
#' @param X Predictor variable (all variables should be passed 'bare' in tidyverse style)
#' @param M Mediator variable
#' @param W Moderator variable (assumed continuous, centered/standardized internally)
#' @param Y Outcome variable
#' @param CVs Covariates (in predicting `M` *and* `Y`)
#' @param mod_direct_path Logical. Should the direct path from `X` to `Y` also be
#' moderated by `W`? Defaults to `TRUE`. If `FALSE`, only the first-stage (`X` to `M`)
#' path is moderated.
#' @param standardized_all Logical. Should all variables (`X`, `M`, `W`, `Y` and any `CVs`)
#' be standardized (z-scored) before the product term is formed and the model is estimated?
#' If `FALSE`, only `X` and `W` are mean-centered before the product term is formed - this
#' is the minimum needed for interpretable conditional effects, but paths remain in the
#' original (unstandardized) metric of `M`, `Y` and any `CVs`.
#' @param conf_level The confidence level to be used for confidence intervals. Must be between 0 and 1
#' (exclusive), defaults to .95 (i.e. 95% confidence intervals).
#' @param seed Random seed. You should set this to get reproducible results. If `NULL` (default),
#' the seed is not set by this function, and results will thus vary between runs.
#' @param bootstraps Number of bootstraps, defaults to 5000.
#' @param cores Number of CPU cores to use for bootstrapping. Defaults to 1
#' (sequential processing); set higher to speed up bootstrapping on multi-core machines.
#' @param ... Options passed on to [lavaan::sem()].
#' @return A tibble (with S3 class `c("timesaveR_mod_med", class(tibble))`) with one
#' row per parameter, `type` being one of:
#' \describe{
#'  \item{a}{X -> M path}
#'  \item{a_mod}{Moderation of the X -> M path by W (i.e., the X:W product term predicting M)}
#'  \item{w_m}{W -> M path}
#'  \item{b}{M -> Y path}
#'  \item{c}{X -> Y direct path (at the mean of W, since W is centered)}
#'  \item{c_mod}{Moderation of the X -> Y path by W (only if `mod_direct_path = TRUE`)}
#'  \item{w_y}{W -> Y path}
#'  \item{imm}{Index of moderated mediation (`a_mod * b`)}
#'  \item{ind_low}{Conditional indirect effect of X on Y through M, at W = mean - 1 SD}
#'  \item{ind_mean}{Conditional indirect effect of X on Y through M, at the mean of W}
#'  \item{ind_high}{Conditional indirect effect of X on Y through M, at W = mean + 1 SD}
#'  \item{dir_low}{Conditional direct effect of X on Y, at W = mean - 1 SD (only if `mod_direct_path = TRUE`)}
#'  \item{dir_mean}{Conditional direct effect of X on Y, at the mean of W (only if `mod_direct_path = TRUE`)}
#'  \item{dir_high}{Conditional direct effect of X on Y, at W = mean + 1 SD (only if `mod_direct_path = TRUE`)}
#' }
#' Further columns are `est` and `se` (mean and SD of the bootstrap draws), `pvalue`
#' (for the raw lavaan parameters - `a`, `a_mod`, `w_m`, `b`, `c`, `c_mod`, `w_y` - this is
#' the Wald-test p-value returned by `lavaan`; for the derived quantities - `imm` and the
#' conditional indirect/direct effects - this is a bootstrap-based two-sided p-value,
#' `2 * min(mean(draws <= 0), mean(draws >= 0))`, since no analytical p-value is available
#' for them), and `ci.lower`/`ci.upper` (bootstrap percentile confidence intervals for all
#' parameters). Coefficients for covariates are returned as the `CV_coefficients` attribute,
#' and the `lavaan` model code is returned as the `lavaan_code` attribute - see the example.
#' @export
#' @examples
#' \donttest{
#' # Might the link between depression and self-reported health, through reduced
#' # physical activity, be stronger for older participants?
#'
#' set.seed(4321)
#' res <- run_moderated_mediation(ess_health, X = fltdpr, M = dosprt, W = agea, Y = health,
#'                                bootstraps = 50)
#' # NB: bootstraps = 50 only set to reduce running time - should be 1000+
#'
#' res
#'
#' attr(res, "CV_coefficients")
#'
#' # The result can be passed straight to plot_moderated_mediation() for a diagram
#' # annotated with the estimated coefficients - see ?plot_moderated_mediation
#' }
run_moderated_mediation <- function(data, X, M, W, Y, CVs = NULL,
                                    mod_direct_path = TRUE,
                                    standardized_all = TRUE,
                                    conf_level = .95, seed = NULL,
                                    bootstraps = 5000, cores = 1, ...) {

  .check_req_packages(c("lavaan"))

  # Check for specific lavaan version with bootstrap bug (only 0.6-13)
  if (utils::packageVersion("lavaan") == "0.6.13") {
    cli::cli_warn('lavaan 0.6-13 has a bootstrap bug that may cause errors with {.code estimator = "MLR"} and {.code missing = "direct"}.
            See {.url https://github.com/yrosseel/lavaan/issues/275}.
            Consider updating lavaan or specify {.code missing = "listwise"} or {.code estimator = "ML"} as a workaround.')
  }

  args <- as.list(match.call(expand.dots = TRUE))[-1]
  if ("conf.level" %in% names(args)) {
    cli::cli_abort("The confidence level needs to be specified as {.arg conf_level}, NOT {.arg conf.level}")
  }

  checkmate::assert_data_frame(data)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  checkmate::assert_flag(mod_direct_path)
  checkmate::assert_flag(standardized_all)
  checkmate::assert_count(bootstraps, positive = TRUE)
  checkmate::assert_count(cores, positive = TRUE)

  # Convert arguments to strings
  X <- as.character(rlang::ensym(X))
  M <- as.character(rlang::ensym(M))
  W <- as.character(rlang::ensym(W))
  Y <- as.character(rlang::ensym(Y))

  CVs_string <- all.vars(rlang::enquo(CVs))
  if (length(CVs_string) > 0) CVs <- CVs_string
  CVs_string <- CVs

  if (any(stringr::str_detect(c(X, M, W, Y, CVs), "__")))
    cli::cli_abort("This function does not support variable names that contain two {.code _} in a row. Please rename.")

  # Safe internal column name for the product term
  int_col <- ".XW_int"
  if (int_col %in% c(X, M, W, Y, CVs, names(data))) {
    cli::cli_abort("Please rename the column {.val {int_col}} in your data - it clashes with a column name used internally by this function.")
  }

  CVs_part <- if (is.null(CVs)) "" else paste("+", paste(CVs, collapse = " + "))

  # Build analysis dataset
  dat <- data %>%
    dplyr::select(dplyr::all_of(c(X, M, W, Y, CVs))) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

  if (standardized_all) {
    dat <- dat %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.numeric(scale(.x))))
  } else {
    dat[[X]] <- as.numeric(scale(dat[[X]], scale = FALSE))
    dat[[W]] <- as.numeric(scale(dat[[W]], scale = FALSE))
  }

  dat[[int_col]] <- dat[[X]] * dat[[W]]
  sd_w <- stats::sd(dat[[W]], na.rm = TRUE)

  # Run moderated mediation model
  mod <- glue::glue("
    #First stage (moderated) and W -> M
    {M} ~ a*{X} + a_mod*{int_col} + w_m*{W} {CVs_part}

    #Direct path (optionally moderated) and W -> Y
    {Y} ~ b*{M} + c*{X} {if (mod_direct_path) glue::glue('+ c_mod*{int_col}') else ''} + w_y*{W} {CVs_part}
  ")

  if (!is.null(seed)) set.seed(seed)

  # Lavaan arguments - set defaults if not overridden in dots
  dots <- list(...)
  def.vals <- list(
    model = mod,
    estimator = "MLR",
    fixed.x = FALSE,
    missing = "direct",
    data = dat
  )
  ind <- unlist(lapply(dots[names(def.vals)], is.null))
  dots[names(def.vals)[ind]] <- def.vals[ind]
  lavaansem <- function(...) {
    lavaan::sem(...)
  }
  fit <- do.call(lavaansem, dots)

  raw_params <- c("a", "a_mod", "w_m", "b", "c", "w_y")
  if (mod_direct_path) raw_params <- c(raw_params, "c_mod")

  # Point estimates, SEs and Wald-test p-values for the raw parameters, straight
  # from the fitted lavaan model (only the CIs for these are bootstrap-based)
  pe <- lavaan::parameterEstimates(fit)
  raw_est <- tibble::tibble(
    type = raw_params,
    est = pe$est[match(raw_params, pe$label)],
    se = pe$se[match(raw_params, pe$label)],
    pvalue = pe$pvalue[match(raw_params, pe$label)]
  )

  bs_full <- lavaan::bootstrapLavaan(fit, R = bootstraps, FUN = "coef",
                                     parallel = ifelse(cores > 1, "snow", "no"),
                                     ncpus = cores) %>%
    data.frame() %>%
    # Drop unsuccessful bootstraps
    tidyr::drop_na()

  bs_CVs <- bs_full %>%
    dplyr::select(dplyr::matches(paste0(paste0("\\.", CVs_string, "$"), collapse = "|")), -dplyr::matches("\\.\\.")) %>%
    t() %>%
    data.frame()

  bs <- bs_full %>%
    dplyr::select(dplyr::all_of(raw_params))

  # Compute derived quantities from the bootstrapped raw coefficients
  bs <- bs %>%
    dplyr::mutate(
      imm = .data$a_mod * .data$b,
      ind_low = (.data$a + .data$a_mod * (-sd_w)) * .data$b,
      ind_mean = .data$a * .data$b,
      ind_high = (.data$a + .data$a_mod * (sd_w)) * .data$b
    )

  if (mod_direct_path) {
    bs <- bs %>%
      dplyr::mutate(
        dir_low = .data$c - .data$c_mod * sd_w,
        dir_mean = .data$c,
        dir_high = .data$c + .data$c_mod * sd_w
      )
  }

  # Bootstrap-based CIs for all parameters (and est/se/pvalue for the derived
  # quantities only, since no analytical equivalent is available for them)
  res_boot <- bs %>%
    t() %>%
    data.frame() %>%
    tibble::rownames_to_column("type") %>%
    tidyr::pivot_longer(cols = -"type", names_to = "rep", values_to = "coef") %>%
    dplyr::group_by(.data$type) %>%
    dplyr::summarise(est = mean(.data$coef), se = sd(.data$coef),
                     pvalue = ifelse(.data$est > 0, mean(.data$coef < 0) * 2, mean(.data$coef > 0) * 2),
                     ci.lower = quantile(.data$coef, (1 - conf_level) / 2),
                     ci.upper = quantile(.data$coef, 1 - (1 - conf_level) / 2))

  # For the raw (non-derived) parameters, use the point estimate, SE and
  # p-value from the fitted model rather than the bootstrap distribution -
  # only the CI is taken from the bootstrap
  res <- res_boot
  raw_rows <- match(raw_est$type, res$type)
  res$est[raw_rows] <- raw_est$est
  res$se[raw_rows] <- raw_est$se
  res$pvalue[raw_rows] <- raw_est$pvalue

  type_order <- c(raw_params[raw_params != "c_mod"], if (mod_direct_path) "c_mod",
                  "imm", "ind_low", "ind_mean", "ind_high",
                  if (mod_direct_path) c("dir_low", "dir_mean", "dir_high"))
  res <- res[match(type_order, res$type), ]
  rownames(res) <- NULL

  CV_res <- bs_CVs %>%
    tibble::rownames_to_column("parameter") %>%
    tidyr::pivot_longer(cols = -"parameter", names_to = "rep", values_to = "coef") %>%
    dplyr::group_by(.data$parameter) %>%
    dplyr::summarise(est = mean(.data$coef), se = sd(.data$coef),
                     pvalue = ifelse(.data$est > 0, mean(.data$coef < 0) * 2, mean(.data$coef > 0) * 2),
                     ci.lower = quantile(.data$coef, (1 - conf_level) / 2),
                     ci.upper = quantile(.data$coef, 1 - (1 - conf_level) / 2)) %>%
    tidyr::separate("parameter", c("DV", "CV"), fill = "right", sep = "\\.")

  attr(res, "CV_coefficients") <- CV_res
  attr(res, "lavaan_code") <- mod
  class(res) <- c("timesaveR_mod_med", class(res))

  res
}