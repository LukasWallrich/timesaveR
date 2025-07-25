#' Paired t.test with Cohen's d
#'
#' This function takes two variables that are representing paired data and
#' calculates a paired samples `t.test`. It then also calculates and prints
#' Cohen's d as a measure of effect size and shows a clearer data label than
#' the t.test function.
#'
#' @param data A dataframe
#' @param x,y Character strings indicating the names of the two variables
#' @return Invisibly returns a list including the t.test() output and Cohen's D
#' @examples 
#' paired_t_test_d(iris, "Sepal.Width", "Petal.Length")
#' @export

paired_t_test_d <- function(data, x, y) {
  t.test_result <- t.test(x = data[[x]], y = data[[y]], paired = TRUE)
  t.test_result$data.name <- paste(x, "vs.", y)
  print(t.test_result)
  cohens_d <- t.test_result$estimate /
    (sqrt(t.test_result$parameter + 1) * t.test_result$stderr)
  print(paste("Cohen's d:", round_(cohens_d, 3)))
  invisible(list(t_test = t.test_result, d = unname(cohens_d)))
}

#' t.test for survey object with Cohen's d
#'
#' This function calculates a t.test() for two groups in a `srvyr` survey
#' object. It is particularly helpful when the grouping variable has more than
#' two levels and you just want to compare two of them.
#'
#' @param data A survey object
#' @param dv Character. Name of the dependent variable for the t.test (numeric)
#' @param iv Character. Name of the grouping variable for the t.test (factor)
#' @param pair Character vector of length 2. Levels of iv to
#'    be compared in t.test. Can be NULL if iv only has two distinct values.
#' @param ttest Logical. Should t.test be run? Otherwise, only
#'   Cohen's d is calculated. Defaults to TRUE.
#' @param print Logical. Should results be printed.   
#' @return Returns a one-row tibble with tidy results of t-test and Cohen's d. 
#' If print is TRUE, results are returned invisibly. 
#' @examples 
#' library(srvyr)
#'
#' #Create weights (consists of two variables in ESS)
#' ess_health$svy_weight <- ess_health$pspwght * ess_health$pweight
#'
#' ess_survey <- as_survey(ess_health,
#'                        weights = svy_weight)   
#'
#' svy_cohen_d_pair(ess_survey, "health", "cntry", pair = c("DE", "GB"))
#'   
#' @export

svy_cohen_d_pair <- function(data, dv, iv, pair = NULL, ttest = TRUE, print = FALSE) {
  assert_class(data, "survey.design")
  if (is.null(pair)) {
    l <- data$variables[[iv]] %>% unique()
    if (length(l) == 2) {
      pair <- l
    } else {
      cli::cli_abort("pair must not be NULL unless iv has exactly two distinct values.")
    }
  }
  data <- eval(parse(text = paste0(
    "update(data, filt = factor(data$variables$",
    iv, ", levels = c('", paste0(pair,
      collapse = "', '"
    ), "')))"
  )))
  t.test_result <- NULL
  if (ttest) {
    t.test_result <- eval(parse(text = paste0(
      "survey::svyttest(", dv, " ~ ", iv,
      ", subset(data, !is.na(filt)))"
    )))
  }
  # Calculate Cohen's d
  means <- survey::svyby(~ get(dv), ~filt, data, survey::svymean, na.rm = TRUE)[1:2]
  names(means) <- c(dv, "mean")
  vars <- survey::svyby(~ get(dv), ~filt, data, survey::svyvar, na.rm = TRUE)[1:2]
  names(vars) <- c(dv, "var")
  cohens_d <- (means[1, 2] - means[2, 2]) / sqrt((vars[1, 2] + vars[2, 2]) / 2)
  
  if (print) {
    print(paste0("T-Test for ", paste0(pair, collapse = " & "), ":"))
    print(t.test_result)
  print(paste0(
    "Cohen's d for pair ", paste0(pair, collapse = " & "), " is: ",
    round_(cohens_d, 3)
  ))
  }
  
  d <- tibble::tibble(pair = paste0(pair, collapse = " & "), d = cohens_d)
  
  if (!ttest && print) invisible(d)
  if (!ttest) return(d)
  
  t <- broom::tidy(t.test_result) %>% 
    dplyr::transmute(pair = paste0(pair, collapse = " & "), t = .data$estimate, 
                     df = .data$parameter, .data$p.value, mean_diff = .data$estimate, 
                     mean_diff_ci.low = .data$conf.low, mean_diff_ci.high = .data$conf.high)

  if (sign(t$t) != sign(d$d)) t <- t %>% 
    dplyr::mutate(dplyr::across(c(.data$t, .data$mean_diff, .data$mean_diff_ci.low, .data$mean_diff_ci.high), ~.x * -1))
  
  if (print) invisible(dplyr::left_join(t, d, by = "pair"))
  
  dplyr::left_join(d, t, by = "pair")
}

#' Pairwise t.tests with effect sizes and survey weights
#'
#' This function calculates a t.test() for any pair of levels in a
#' `srvyr` survey object. It does currently not do any p-value adjustment
#' for multiple comparisons, and print rather than returns the results.
#'
#' @param cats Character vector of factor levels to be included in the
#' pairwise tests. If set to NULL, all levels are used.
#' @param p.adjust Method to adjust p-values for multiple comparisons. One of 
#' "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr" or "none".
#' @inheritDotParams svy_cohen_d_pair -pair
#' @inheritParams svy_cohen_d_pair
#' @return A tibble with t-test results and Cohen's d for each pair
#' @export


svy_pairwise_t_test <- function(data, dv, iv, cats, p.adjust = "holm", ...) {
  
  assert_class(data, "survey.design")
  assert_choice(p.adjust, p.adjust.methods)
  
  if (is.null(cats)) {
    cats <- eval(parse(text = paste0("levelsdf$variables$", iv)))
  }

  data2 <- purrr::cross_df(data.frame(cats, cats, stringsAsFactors = FALSE),
    .filter =
      function(x, y) as.character(x) <= as.character(y)
  )
  x <- purrr::map_dfr(purrr::pmap(data2, c), function(x) {
    svy_cohen_d_pair(data, iv = iv, dv = dv, pair = x, print = FALSE, ...)
  })

  if ("p.value" %in% (names(x))) x$p.value <- p.adjust(x$p.value, method = p.adjust)
  
  x
}

#' lm() with standardised continuous variables
#'
#' This runs lm() after standardising all continuous variables, while leaving
#' factors intact.
#'
#' Beware when using weights and calculating standardized coefficients. In line with
#' other implementations, this function scales the variables without taking the weights
#' into account - but that means that coefficients represent the estimated change in SD of Y
#' per SD of X *in the sample* and explicitly *not* in the population. Weighted scaling might
#' be more appropriate, yet is not implemented here (and does not appear to be commonly used).
#' Also, in the model call, the weights variable will always be called `.weights`. This might
#' pose a problem when you update the model later on, for the moment the only workaround
#' is to rename the weights variable accordingly (or to fix it and contribute a PR on
#' Github).
#'
#' @inheritParams stats::lm
#' @param ... Additional arguments passed to `lm()`. Note that `subset` cannot be used here and should be applied to `data` beforehand.
#' @references See (Fox, 2015) for an argument why dummy variables should never
#' be standardised. If you want to run a model with all variables standardised,
#' one option is `QuantPsyc::lm.beta()`
#' @return An `lm` object with standardized coefficients.
#' @examples
#' # Basic usage with mtcars dataset
#' lm_std(mpg ~ cyl + disp, data = mtcars)
#'
#' # Using weights
#' mtcars$wt_sample <- runif(nrow(mtcars), 1, 3)
#' lm_std(mpg ~ cyl + disp, data = mtcars, weights = wt_sample)
#'
#' # Handling variables with few levels
#' mtcars$am_factor <- factor(mtcars$am)
#' lm_std(mpg ~ cyl + disp + am_factor, data = mtcars)

lm_std <- function(formula, data = NULL, weights = NULL, weighted_standardize = FALSE, ...) {
  if (any(stringr::str_detect(as.character(formula), "factor\\("))) {
    cli::cli_abort("Functions in the formula are applied after standardising - thus factor() needs to be used before lm_std() is called")
  }
  
  if (!is.null(weights) && is.logical(weighted_standardize) && isFALSE(weighted_standardize)) {
    cli::cli_warn(
      c(
        "Standardised coefficients from a model with weights are now returned based on unweighted standardisation of the variables.",
        "i" = "While that is common (and thus the default), it is unclear whether it is appropriate.",
        "i" = "Check ?lm_std, and consider changing weighted_standardize.",
        "i" = "This is displayed once per session when using weighted_standardize = FALSE.",
        "i" = "Use weighted_standardize = 'no' to use unweighted standardisation and omit the warning."
      ),
      .frequency = "once"
    )
  }
  
  vars <- all.vars(formula)
  extras <- list(...)
  
  if ("subset" %in% names(extras)) {
    cli::cli_abort("Cannot subset in this function as that would happen after standardisation - please subset first.")
  }
  
  if (is.null(data)) {
    data <- NULL
    for (i in seq_along(vars)) {
      # Odd extraction to work with with() call environments
      x <- get(vars[i], pos = parent.frame())
      data <- dplyr::bind_cols(data, tibble::tibble(!!vars[i] := x))
    }
    if (!is.null(weights)) data$.weights <- weights
  }
  
  if (!is.null(data)) {
    weights <- rlang::enquo(weights)
    
    if (!rlang::quo_is_null(weights)) {
      if (rlang::as_label(weights) %in% names(data)) {
        data <- dplyr::rename(data, .weights = !!weights)
      } else {
        data$.weights <- rlang::eval_tidy(weights)
      }
      data <- data[c(vars, ".weights")]
    } else {
      data <- data[vars]
    }
  }
  
  vars_num <- vars[purrr::map_lgl(data, is.numeric)]
  vars_num <- vars_num[!is.na(vars_num)]
  
  vars_dummies <- vars_num[purrr::map_lgl(vars_num, ~ dplyr::n_distinct(data[[.x]]) < 3)]
  
  if (length(vars_dummies) > 0) {
    cli::cli_warn("The following variables have fewer than three distinct values but are of type numeric: {vars_dummies}. Check whether they should not be factors instead. As it stands, they are standardised, which is typically not recommended.")
  }
  
  data %<>% dplyr::mutate(dplyr::across(dplyr::all_of(vars_num), scale_blank))
  
  formula <- Reduce(paste, deparse(formula))
  
  if (!rlang::quo_is_null(weights)) {
    mod <- eval(parse(text = glue::glue("lm({formula}, data = data, weights = .weights, ...)")))
  } else {
    mod <- eval(parse(text = glue::glue("lm({formula}, data = data, ...)")))
  }
  
  attr(mod, "standardized") <- TRUE
  class(mod) <- c("lm_std", class(mod))
  mod
}




#' t-test() on multiply-imputed data (accepts survey weights)
#'
#' This runs t-test (based on lm(), therefore assuming equal variances) on multiple imputations,
#' with the ability to take into account survey weights.
#'
#' @param mi_list A list of dataframes, each consisting of a multiply imputed dataset
#' @param dv The dependent variable for the t.test (must be in mi_list)
#' @param groups The grouping variable (must have only two values, be in mi_list)
#' @param weights The variable containing survey weights, must be in mi_list
#'
#' @return A one-row tibble containing the result of the t-test
#' 
#' @examples 
#' #Create list with imputed data
#' library(mice)
#' library(dplyr)
#' imp <- mice(nhanes2)
#' imp_list <- complete(imp, action="long") %>%
#'    dplyr::group_split(.imp)
#'
#' t_test_mi(imp_list, bmi, hyp)
#'
#' @export

t_test_mi <- function(mi_list, dv, groups, weights = NULL) {
  dv <- rlang::enquo(dv)
  groups <- rlang::enquo(groups)
  weights <- rlang::enquo(weights)

  mi_list <- purrr::map(mi_list, dplyr::select, wt = !!weights, dv = !!dv, g = !!groups)

  out <- .run_t_test_mi(mi_list)

  out$group_var <- dplyr::as_label(groups)

  out
}

.run_t_test_mi <- function(mi_list) {
  tests <- purrr::map(mi_list, do.call, what = .wtd_t_test_lm) %>% mice::pool()

  res <- summary(tests)

  if (nrow(res) > 2) cli::cli_abort("Group should only have two levels - subset data or use pairwise_t_test_mi instead")

  groups <- mi_list[[1]]$g %>%
    unique() %>%
    as.character()

  out <- tibble::tibble(x = groups[1], y = groups[2], mean_diff = res[2, "estimate"], t_value = res[2, "statistic"], df = res[2, "df"], p_value = res[2, "p.value"])

  out
}

#' Pairwise t-tests() on multiply-imputed data (accepts survey weights)
#'
#' This runs pairwise t-tests (based on lm(), therefore assuming equal variances)
#' on multiple imputations, with the ability to take into account survey weights.
#'
#' @inheritParams t_test_mi
#' @param groups The grouping variable (each distinct value will be treated as a level)
#' @param p.adjust.method The method to adjust p-values for multiple comparison (see [stats::p.adjust()])
#' @return A tibble containing the results of the t-tests with one test per row
#' 
#' @examples
#' #Create list with imputed data
#' library(mice)
#' library(dplyr)
#' imp <- mice(nhanes2)
#' imp_list <- complete(imp, action="long") %>%
#'    group_split(.imp)
#'
#' #Specify dependent variable and grouping factor
#' pairwise_t_test_mi(imp_list, bmi, age)
#'
#' @export

pairwise_t_test_mi <- function(mi_list, dv, groups, weights = NULL, p.adjust.method = p.adjust.methods) {
  
    dv <- rlang::enquo(dv)
    groups <- rlang::enquo(groups)
    weights <- rlang::enquo(weights)

  pairs <- mi_list[[1]] %>%
    dplyr::select(!!groups) %>%
    dplyr::pull() %>%
    unique() %>%
    as.character() %>%
    utils::combn(2) %>%
    split(col(.))
  mi_list_sel <- purrr::map(mi_list, dplyr::select, wt = !!weights, dv = !!dv, g = !!groups)

    out <- purrr::map_df(pairs, function(x) {
    dat <- purrr::map(mi_list_sel, dplyr::filter, .data$g %in% x)
    .run_t_test_mi(dat)
  })

  out$p_value %<>% stats::p.adjust(p.adjust.method)

  out$group_var <- dplyr::as_label(groups) %>% stringr::str_remove_all('^"|"$')

  out
}

.wtd_t_test_lm <- function(dv, g, wt = NULL, ...) {
  lm(dv ~ g, weights = wt, ...)
}

#' Get letters to indicate results of multiple comparisons/post-hoc tests
#'
#' This takes the results of multiple comparisons and returns a set of letters
#' that can be used to indicate which differences are significant. The difference
#' between levels that are assigned the same letter are *not* statistically different.
#'
#' @param tests Either a tibble with the result of comparisons, including x and y
#' (the levels/groups that were compared) and p_value for the comparison or an object
#' of class pairwise.htest, for example returned from pairwise.t.test()
#' @param alpha_level The level of significance for the test
#' @param p.adjust.method One of p.adjust.methods, defaults to none as p-values will
#' typically have been adjusted when carrying out the pairwise comparisons/post-hoc tests
#'
#' @return A tibble with columns that indicate which letter has been assigned to each
#' group/level
#' @source Algorithm based on https://www.tandfonline.com/doi/abs/10.1198/1061860043515
#'
#' @examples
#' data("airquality")
#' airquality$month <- factor(airquality$Month, labels = month.abb[5:9])
#' x <- pairwise.t.test(airquality$Ozone, airquality$Month)
#'
#' get_pairwise_letters(x)
#' @export


get_pairwise_letters <- function(tests,
                                 alpha_level = .05,
                                 p.adjust.method = "none") {
  if ("pairwise.htest" %in% class(tests)) {
    tests <- tests$p.value
    dat_levels <- c(colnames(tests), rownames(tests)) %>% unique()
    p <- cbind(rbind(NA, tests), NA)
    diag(p) <- 1
    p[upper.tri(p)] <- t(p)[upper.tri(p)]

    colnames(p) <- dat_levels
    rownames(p) <- dat_levels
    tests <- dat_levels %>%
      utils::combn(2) %>%
      split(col(.)) %>%
      purrr::map_df(function(a) tibble::tibble(x = a[1], y = a[2]))

    tests$p_value <- NA
    tests <- purrr::pmap_dfr(tests, function(...) {
      current <- tibble::tibble(...)
      current %>% dplyr::mutate(p_value = p[.data$x, .data$y])
    })
  }

  dat_levels <- c(tests$x, tests$y) %>%
    as.character() %>%
    unique()

  tests$p_value %<>% stats::p.adjust(p.adjust.method)

  tests %<>% dplyr::filter(.data$p_value < alpha_level)
  dat_letters <- tibble::tibble(dat_level = dat_levels)

  if (nrow(tests) == 0) { # If no comparisons are significant
    dat_letters[2] <- TRUE
  } else {
    dat_letters[2:(nrow(tests) + 2)] <- FALSE
    dat_letters[2] <- TRUE

    n <- 2
    for (i in seq_len(nrow(tests))) {
      for (j in 2:n) {
        if (dat_letters[dat_letters$dat_level == tests$x[i], j][[1]] &&
          dat_letters[dat_letters$dat_level == tests$y[i], j][[1]]) {
          n <- n + 1
          dat_letters[n] <- dat_letters[j]
          dat_letters[dat_letters$dat_level == tests$x[i], j] <- FALSE
          dat_letters[dat_letters$dat_level == tests$y[i], n] <- FALSE
        }
      }
    }

    n <- 1
    absorb <- numeric()

    for (i in 2:(ncol(dat_letters) - 1)) {
      for (j in (i + 1):ncol(dat_letters)) {
        if (min(dat_letters[i] - dat_letters[j]) >= 0) {
          absorb[n] <- j
          n <- n + 1
        }
      }
    }

    if (length(absorb > 0)) dat_letters <- dat_letters[-absorb]


    n <- 1
    absorb <- numeric()

    for (i in (ncol(dat_letters):3)) {
      for (j in 2:(i - 1)) {
        if (min(dat_letters[i] - dat_letters[j]) >= 0) {
          absorb[n] <- j
          n <- n + 1
        }
      }
    }

    if (length(absorb > 0)) dat_letters <- dat_letters[-absorb]
  }
  
  #Sort letters
  letter_order <- purrr::map_int(dat_letters[-1], ~min(which(.x == TRUE))) %>% sort()
  dat_letters <- cbind(dat_letters[1], dat_letters[names(letter_order)])
  
  for (i in 2:ncol(dat_letters)) {
    dat_letters[letters[i - 1]] <- NA_character_
    dat_letters[letters[i - 1]][dat_letters[[i]], ] <-
      letters[i - 1]
  }

  dat_letters %<>% dplyr::select(-dplyr::matches("^\\.")) %>%
    tidyr::unite("letters", -"dat_level", sep = "", remove = FALSE, na.rm = TRUE) %>%
    dplyr::rename(level = "dat_level")

  return(tibble::as_tibble(dat_letters))
}

#' Pairwise t-tests() returned in tidy dataframe
#'
#' This runs pairwise independent-samples t-tests (assuming unequal variance by default, but can be changed)
#' and returns the results and effect sizes in a tidy dataframe. Beware: It will automatically omit missing values.
#'
#' @param data A dataframe containing the outcome and grouping variable
#' @param outcome The outcome variable in dataframe, or a formula of the form `outcome ~ group`. If a formula is provided, the group needs to be empty.
#' @param groups The grouping variable (each distinct value will be treated as a level)
#' @param na.rm Logical. Should missing values be removed. If NULL, they are removed with a warning.
#' @param p.adjust.method The method to adjust p-values for multiple comparison (see [stats::p.adjust()])
#' @param conf_level confidence level of the interval.
#' @param var_equal a logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled
#' variance is used to estimate the variance otherwise the Welch (or Satterthwaite)
#' approximation to the degrees of freedom is used.
#' @return A tibble containing the results of the t-tests with one test per row, including a column (`apa`) formatted for reporting
#' @examples
#' \dontrun{
#' pairwise_t_tests(mtcars, wt, cyl)
#' pairwise_t_tests(mtcars, wt ~ cyl)
#' }
#' @export

pairwise_t_tests <- function(data, outcome, groups = NULL, p.adjust.method = p.adjust.methods, 
                             conf_level = .95, var_equal = FALSE, na.rm = NULL) {
  
  if (is.character(rlang::enexpr(outcome))) {
    cli::cli_warn("Outcome and groups should be provided as raw variable names, not a string, as shown in the examples.")
    outcome <- rlang::sym(outcome)
    groups <- rlang::sym(groups)
  }
  
  # Check if the data is a data frame
  assert_data_frame(data, min.rows = 2, any.missing = TRUE)
  
  # If outcome is a formula, groups should not be specified separately
  if (missing(groups)) {
    assert_formula(outcome)
    groups <- as.character(outcome[[3]])
    if (length(groups) > 1) cli::cli_abort("If formula notation is used, only one grouping variable should be provided on the RHS")
    groups <- rlang::sym(groups)
    outcome <- rlang::sym(as.character(outcome[[2]]))
  } else {
    # Check if outcome and groups are column names in the data frame
    assert_choice(as.character(rlang::enexpr(outcome)), colnames(data))
    assert_choice(as.character(rlang::enexpr(groups)), colnames(data))
  }
  
  assert_logical(na.rm, null.ok = TRUE)
  # Handle missing values in the grouping variable (na.rm)
  group_values <- dplyr::pull(data, {{ groups }})
  outcome_values <- dplyr::pull(data, {{ outcome }})
  
  if (any(is.na(group_values)) | any(is.na(outcome_values))) {
    if (is.null(na.rm) || na.rm) {
      if(is.null(na.rm)) {
        n_removed <- sum(is.na(group_values) | is.na(outcome_values))
        cli::cli_warn("Missing values found in the grouping or outcome variables. {n_removed} case{if (n_removed > 1) 's' else ''} will be removed.")
      }
      data <- data %>% dplyr::filter(!is.na({{ groups }}))
    } else {
      cli::cli_abort("Missing values found in the grouping or outcome variables. Set `na.rm = TRUE` to remove them.")
    }
  }
  
  # Check if the grouping variable has more than one unique value
  unique_groups <- unique(dplyr::pull(data, {{ groups }}))
  if (length(unique_groups) < 2) {
    cli::cli_abort("The grouping variable must have at least two unique values for pairwise comparisons.")
  }
  
  # Handle mixed data types (numeric and character)
  if (!is.factor(group_values)) {
    if (!is.character(group_values) && is.null(attr(group_values, "labels"))) {
    cli::cli_warn("The grouping variable is not a factor. It will be converted to a factor.")
    } 
    
    if (is.numeric(group_values) && is.null(attr(group_values, "labels"))) {
      haven_available <- "haven" %in% rownames(installed.packages())
      if (haven_available) {
        as_factor <- getNamespace("haven")$as_factor.labelled
        data <- data %>% dplyr::mutate({{ groups }} := as_factor({{ groups }}))
      } else {
        cli::cli_warn("The grouping variable is a labelled numeric, but haven package is not available. It will be converted to factor, but the group labels might be lost.")
        data <- data %>% dplyr::mutate({{ groups }} := as.factor({{ groups }}))
      }
    } else {
    
    data <- data %>% dplyr::mutate({{ groups }} := as.factor({{ groups }}))
    }
  }
  
  # Generate pairs for t-tests
  pairs <- utils::combn(levels(dplyr::pull(data, {{ groups }})), 2, simplify = FALSE)
  
  fmla <- as.formula(paste(dplyr::as_label(rlang::enexpr(outcome)), "~", dplyr::as_label(rlang::enexpr(groups))))
  
  # Perform pairwise t-tests
  out <- purrr::map_df(pairs, function(x) {
    dat <- dplyr::filter(data, {{ groups }} %in% !! x)
    out <- stats::t.test(fmla, dat, var.equal = var_equal, conf.level = conf_level, na.action = "na.omit") %>% 
      broom::tidy()
    desc <- dat %>%
      dplyr::arrange(dplyr::desc({{ groups }} == x[1])) %>%
      dplyr::group_by({{ groups }}) %>%
      dplyr::summarise(M = mean({{ outcome }}, na.rm = TRUE), var = stats::var({{ outcome }}, na.rm = TRUE), .groups = "drop")
    cohens_d <- (desc$M[1] - desc$M[2]) / sqrt((desc$var[1] + desc$var[2]) / 2)
    out <- cbind(tibble::tibble(var_1 = x[1], var_2 = x[2], cohens_d = cohens_d), out) %>%
      dplyr::select("var_1", "var_2", mean_1 = "estimate1", mean_2 = "estimate2", mean_diff = "estimate", 
                    conf_low = "conf.low", conf_high = "conf.high", t_value = "statistic", 
                    df = "parameter", p_value = "p.value", "cohens_d", test = "method")
  })
  
  # Adjust p-values
  out$p_value <- stats::p.adjust(out$p_value, method = p.adjust.method)
  out$p_value_adjust <- p.adjust.method[1]
  
  out$group_var <- dplyr::as_label(rlang::enexpr(groups))

  out$apa <- paste0("t(", round(out$df), ") = ", round_(out$t_value, 2), ", p ", fmt_p(out$p_value), ", d = ", round_(out$cohens_d, 2))

  tibble::tibble(out)
}


#' polr() with standardised continuous variables
#'
#' This runs [MASS::polr()] after standardising all continuous predictors, while leaving
#' factors intact. Note that the Hessian (the observed information matrix)
#' is always returned, so that the `Hess` argument cannot be used.
#'
#' In the model call, the weights variable will always be called `.weights`. This might
#' pose a problem when you update the model later on, for  the moment the only workaround
#' is to rename the weights variable accordingly (or to fix it and contribute a PR on
#' Github).
#'
#' @inheritParams MASS::polr
#' @inheritDotParams MASS::polr -data -subset -Hess
#' @references See (Fox, 2015) for an argument why dummy variables should never
#' be standardised. 
#' @examples 
#' polr_std(poverty ~ religion + age + gender, WVS)
#' @export

polr_std <- function(formula, data = NULL, weights = NULL, ...) {

  if (any(stringr::str_detect(deparse(formula, width.cutoff = 200), "~.*factor\\("))) cli::cli_abort("Functions in the formula are applied after standardising - thus factor() needs to be used before polr_std() is called")
  
  vars <- all.vars(formula)
  
  extras <- list(...)
  
  if ("subset" %in% names(extras)) cli::cli_abort("Cannot subset in this function as that would happen after standardisation - please subset first.")
  if ("Hess" %in% names(extras)) cli::cli_abort("Cannot use Hess argument - Hess is always set to TRUE in this function.")
  
  if (is.null(data)) {
    for (i in seq_along(vars)) {
      #Odd extraction to work with with() call environments
      x <- get(vars[i], pos = parent.frame())
      data <- dplyr::bind_cols(data, tibble::tibble(!!vars[i] := x))
    }
    if (!is.null(weights)) data$.weights <- weights
  }
  
  if (!is.null(data)) {
    weights <- rlang::enquo(weights)
    
    if (!rlang::quo_is_null(weights)) {
      if (rlang::as_label(weights) %in% names(data)) {
        data <- dplyr::rename(data, .weights = !!weights) 
      } else {
        data$.weights <- rlang::eval_tidy(weights)
      }
      data <- data[c(vars, ".weights")]
    } else {
      data <- data[vars]
    }
  }
  
  vars_num <- vars[purrr::map_lgl(data, is.numeric)]
  vars_num <- vars_num[!is.na(vars_num)]
  
  vars_dummies <- vars_num[purrr::map_lgl(vars_num, ~ dplyr::n_distinct(data[[.x]]) < 3)]
  
  if (length(vars_dummies) > 0) cli::cli_warn("The following variables have less than three distinct values but are of type numeric: {paste0(vars_dummies, collapse = ', ')}. Check whether they should not be factors instead. As it stands, they are standardised, which is typically not recommended.")
  
  data %<>% dplyr::mutate(dplyr::across(dplyr::all_of(vars_num), scale_blank))
  
  formula <- Reduce(paste, deparse(formula))
  
  if (!rlang::quo_is_null(weights)) {
    mod <- eval(parse(text = glue::glue("MASS::polr({formula}, data = data, Hess = TRUE, weights = .weights, ...)")))
  } else {
    mod <- eval(parse(text = glue::glue("MASS::polr({formula}, data = data, Hess = TRUE, ...)")))
  }
  
  class(mod) <- c("tsR_std", class(mod))
  mod
}

#' Dummy-code variable
#' 
#' Turns a categorical variable into a tibble of n-1 dummy-coded values. If x is a factor, the first level is 
#' omitted and thus treated as the reference level (to match the behavior of lm() and related functions). If x
#' is not a factor, the first value in x is treated as the reference level. Variable names returned include a common 
#' prefix and a cleaned up version of the factor levels (without special characters and in snake_case).
#' 
#' @param x The categorical variable to be dummy-coded
#' @param prefix String to be pre-fixed to the new variable names (typically the name of the variable that is dummy-coded). 
#' If NULL, variables are just named with the levels. If left as NA, the function will try to extract the name of the variable passed.
#' @param drop_first Should first level be dropped? Defaults to TRUE
#' @param verbose Should message with reference level be displayed?
#' @examples 
#' dummy_code(iris$Species)
#' @export     

dummy_code <- function(x, prefix = NA, drop_first = TRUE, verbose = interactive()) {
  if (is.na(prefix)) {
    prefix <- deparse(substitute(x)) %>% stringr::str_remove("^.*\\$")
    if (prefix == ".") {
      cli::cli_inform("prefix cannot be automatically extracted when x is piped in (and . is not a valid prefix). No prefix will be added.")
      prefix <- NULL
    }
  }
  x <- forcats::as_factor(x)
  dummy_names <- .clean_names(levels(x))

  if (!is.null(prefix)) dummy_names <- paste0(.clean_names(prefix), "_", dummy_names)
  out <- purrr::map2_dfc(seq_along(levels(x)), levels(x), function(id, level) {
    tibble::tibble(!!dummy_names[id] := x == level)
  })

  if (drop_first) {
    if (verbose) {
      if (!is.null(prefix)) {
        cli::cli_inform("{prefix} dummy-coded with the following reference level: {levels(x)[1]}")
      } else {
        cli::cli_inform("Dummy-coded with the following reference level: {levels(x)[1]}")
      }
    }
    out <- out[-1]
  }
  out
}


#Simplified from janitor - to avoid yet another dependency
#https://github.com/sfirke/janitor/blob/e7540d6835b0ab7643ebdccf1b4d4cd6395b669d/R/make_clean_names.R

.clean_names <- function(x) {
  transliterated_names <- stringi::stri_trans_general(
    x,
    id = intersect(c("Any-Latin", "Greek-Latin", "Any-NFKD", "Any-NFC", "Latin-ASCII"), stringi::stri_trans_list()) %>%
      paste(collapse = ";")
  )
  # Remove starting spaces and punctuation
  good_start <-
    stringr::str_replace(
      string = transliterated_names,
      # Description of this regexp:
      # \A: beginning of the string (rather than beginning of the line as ^ would indicate)
      # \h: any horizontal whitespace character (spaces, tabs, and anything else that is a Unicode whitespace)
      # \s: non-unicode whitespace matching (it may overlap with \h)
      # \p{}: indicates a unicode class of characters, so these will also match punctuation, symbols, separators, and "other" characters
      # * means all of the above zero or more times (not + so that the capturing part of the regexp works)
      # (.*)$: captures everything else in the string for the replacement
      pattern = "\\A[\\h\\s\\p{Punctuation}\\p{Symbol}\\p{Separator}\\p{Other}]*(.*)$",
      replacement = "\\1"
    )
  # Convert all interior spaces and punctuation to single underscores
  cleaned_within <-
    stringr::str_replace_all(
      string = good_start,
      pattern = "[\\h\\s\\p{Punctuation}\\p{Symbol}\\p{Separator}\\p{Other}]+",
      replacement = "_"
    )

  # Simple snakecase conversion - credit to https://stackoverflow.com/a/22528880/10581449

  out <- gsub("([a-z])([A-Z])([a-z])", "\\1_\\L\\2\\3", cleaned_within, perl = TRUE) %>%
    gsub("([\\_])([A-Z])([a-z])", "\\1\\L\\2\\3", ., perl = TRUE) %>%
    gsub("([a-z])([A-Z])([A-Z])", "\\1_\\2\\3", ., perl = TRUE) %>%
    sub("^(.[a-z])", "\\L\\1", ., perl = TRUE)

  out
}
