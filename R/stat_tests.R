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

  dv <- rlang::as_string(dv)
  iv <- rlang::as_string(iv)

  iv_values <- data$variables[[iv]]

  if (is.null(pair)) {
    unique_levels <- stats::na.omit(unique(iv_values))
    if (length(unique_levels) == 2) {
      pair <- as.character(unique_levels)
    } else {
      cli::cli_abort("pair must not be NULL unless iv has exactly two distinct values.")
    }
  }

  if (!all(pair %in% levels(factor(iv_values)))) {
    cli::cli_abort("All values in {.arg pair} must be present in {.arg iv}.")
  }

  data$variables$filt <- factor(iv_values, levels = pair)
  subset_data <- subset(data, !is.na(filt))

  if (nrow(subset_data$variables) == 0) {
    cli::cli_abort("No observations remain after filtering the survey object for the requested pair.")
  }

  t.test_result <- NULL
  if (ttest) {
    t_formula <- stats::as.formula(paste0(dv, " ~ filt"))
    t.test_result <- survey::svyttest(t_formula, subset_data)
  }

  mean_formula <- stats::as.formula(paste0("~", dv))

  means <- survey::svyby(mean_formula, ~filt, subset_data, survey::svymean, na.rm = TRUE)
  vars <- survey::svyby(mean_formula, ~filt, subset_data, survey::svyvar, na.rm = TRUE)

  means <- dplyr::rename(means, mean = !!rlang::sym(dv))
  vars <- dplyr::rename(vars, variance = !!rlang::sym(dv))

  if (nrow(means) != 2 || nrow(vars) != 2) {
    cli::cli_abort("Unable to compute Cohen's d because fewer than two groups remain after filtering.")
  }

  cohens_d <- (means$mean[1] - means$mean[2]) /
    sqrt((vars$variance[1] + vars$variance[2]) / 2)

  if (print) {
    cli::cli_inform("T-Test for {paste(pair, collapse = ' & ')}:")
    if (ttest) {
      print(t.test_result)
    }
    cli::cli_inform(
      "Cohen's d for pair {paste(pair, collapse = ' & ')} is: {round_(cohens_d, 3)}"
    )
  }

  d <- tibble::tibble(pair = paste0(pair, collapse = " & "), d = cohens_d)

  if (!ttest) {
    return(if (print) invisible(d) else d)
  }

  t <- broom::tidy(t.test_result) %>%
    dplyr::transmute(
      pair = paste0(pair, collapse = " & "),
      t = .data$estimate,
      df = .data$parameter,
      p.value = .data$p.value,
      mean_diff = .data$estimate,
      mean_diff_ci.low = .data$conf.low,
      mean_diff_ci.high = .data$conf.high
    )

  if (sign(t$t) != sign(d$d)) {
    t <- t %>%
      dplyr::mutate(
        dplyr::across(
          c("t", "mean_diff", "mean_diff_ci.low", "mean_diff_ci.high"),
          ~ .x * -1
        )
      )
  }

  result <- dplyr::left_join(d, t, by = "pair")
  if (print) invisible(result) else result
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
  
  dv <- rlang::as_string(dv)
  iv <- rlang::as_string(iv)
  
  iv_levels <- levels(factor(data$variables[[iv]]))
  
  if (is.null(cats)) {
    cats <- iv_levels
  } else {
    assert_character(cats, any.missing = FALSE, min.len = 2)
    if (!all(cats %in% iv_levels)) {
      cli::cli_abort("All supplied {.arg cats} must exist in {.arg iv}.")
    }
  }

  if (length(cats) < 2) {
    cli::cli_abort("At least two categories are required for pairwise comparisons.")
  }

  pairs <- utils::combn(cats, 2, simplify = FALSE)
  results <- purrr::map_dfr(pairs, function(pair_values) {
    svy_cohen_d_pair(data, dv = dv, iv = iv, pair = pair_values, print = FALSE, ...)
  })

  if ("p.value" %in% names(results)) {
    results$p.value <- p.adjust(results$p.value, method = p.adjust)
  }
  
  results
}

#' lm() with standardised continuous variables
#'
#' This runs lm() after standardising all continuous variables, while leaving
#' factors and character variables intact.
#'
#' @section Standardization Method:
#' When using weights, this function calculates standardized coefficients by
#' default using weighted means and standard deviations. This ensures that the
#' resulting coefficients can be interpreted as the estimated change in standard
#' deviations of Y for a one standard deviation change in X. If you require
#' coefficients based on unweighted sample standard deviations, you can set
#' `weighted_standardize = FALSE`. This is fairly common practice, but the 
#' results should not be interpreted as population estimates.
#'
#' @section Transformations and Interactions:
#' Note that standardization is applied to the raw numeric variables from the
#' `data` frame. Any in-formula transformations (e.g., `log(x)`, `I(x^2)`) or
#' interactions (`x1 * x2`) are applied by `lm()` *after* the base variables
#' (`x`, `x1`, `x2`) have been standardized. This is important for the
#' interpretation of the resulting coefficients.
#'
#' @section Internal Details:
#' In the model call, the weights variable will always be internally renamed to
#' `.weights`. This may be relevant if you update the model object later.
#'
#' @inheritParams stats::lm
#' @param ... Additional arguments passed to `lm()`. Note that `subset` cannot
#'   be used here and should be applied to `data` beforehand.
#' @param weighted_standardize How to standardize numeric variables when weights
#'   are present. Can be one of `"auto"` (the default), `TRUE`, or `FALSE`.
#'   If `"auto"`, weighted standardization is performed if a `weights`
#'   argument is provided. See details.
#' @references See Fox, J. (2015). *Applied Regression Analysis and
#'   Generalized Linear Models*. Sage.
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
#' @export

lm_std <- function(formula, data = NULL, weights = NULL, weighted_standardize = "auto", ...) {
  
  weights_quo <- rlang::enquo(weights)
  vars <- all.vars(formula)
  extras <- list(...)
  
  #--- Initial Checks ---#
  if (is.character(rlang::quo_get_expr(weights_quo))) {
    cli::cli_abort("The {.arg weights} argument cannot be a character string. Please provide it as a bare variable name or an expression.")
  }
  
  if (any(stringr::str_detect(as.character(formula), "factor\\("))) {
    cli::cli_abort("Functions in the formula are applied after standardising - thus factor() needs to be used before lm_std() is called.")
  }
  if ("subset" %in% names(extras)) {
    cli::cli_abort("Cannot subset in this function as that would happen after standardisation - please subset first.")
  }
  
  weights_provided <- !rlang::quo_is_null(weights_quo)
  
  #--- Resolve Weighted Standardization Logic ---#
  if (weighted_standardize == "auto") {
    do_weighted_std <- weights_provided
  } else if (!is.logical(weighted_standardize)) {
    cli::cli_abort('{.arg weighted_standardize} must be "auto", TRUE, or FALSE.')
  } else {
    do_weighted_std <- weighted_standardize
  }
  
  if (do_weighted_std && !weights_provided) {
    cli::cli_abort("{.code weighted_standardize = TRUE} requires a {.arg weights} argument.")
  }
  
  #--- Data Handling ---#
  if (is.null(data)) {
    env <- parent.frame()
    var_list <- purrr::map(vars, function(var) {
      if (!exists(var, envir = env, inherits = TRUE)) {
        cli::cli_abort("Variable '{var}' not found in the parent environment.")
      }
      value <- get(var, envir = env, inherits = TRUE)
      if (is.data.frame(value) && var %in% names(value)) {
        value <- value[[var]]
      }
      if (is.matrix(value) && ncol(value) == 1) {
        value <- as.vector(value[, 1])
      }
      value
    })
    names(var_list) <- vars

    var_lengths <- vapply(var_list, length, integer(1))
    if (length(unique(var_lengths)) > 1) {
      cli::cli_abort("Variables pulled from the environment have different lengths. Please provide a data frame in {.arg data} instead.")
    }

    data <- tibble::as_tibble(var_list, .name_repair = "minimal")
  }
  
  #--- Weights Handling ---#
  if (weights_provided) {
    # If weights is a symbol matching a column name, rename it
    if (rlang::is_symbol(rlang::quo_get_expr(weights_quo)) && rlang::as_label(weights_quo) %in% names(data)) {
      data <- dplyr::rename(data, .weights = !!weights_quo)
      # Otherwise, evaluate it as a vector
    } else {
      data$.weights <- rlang::eval_tidy(weights_quo, data = data)
    }
  }
  
  #--- Variable Identification and Checks ---#
  vars_num <- vars[purrr::map_lgl(data[vars], is.numeric)]
  vars_num <- vars_num[!is.na(vars_num)]
  
  # Check for zero-variance numeric variables
  vars_zero_var <- vars_num[purrr::map_lgl(data[vars_num], ~ stats::sd(.x, na.rm = TRUE) == 0)]
  if (length(vars_zero_var) > 0) {
    cli::cli_warn("The following numeric variables have zero variance and will be dropped by {.fn lm}: {.val {vars_zero_var}}.")
  }
  
  # Check for potential dummy variables coded as numeric
  vars_dummies <- vars_num[purrr::map_lgl(data[vars_num], ~ dplyr::n_distinct(.x, na.rm = TRUE) <= 3)]
  # Exclude zero-variance variables from this specific warning
  vars_dummies <- setdiff(vars_dummies, vars_zero_var)
  if (length(vars_dummies) > 0) {
    dummy_list <- paste(sort(unique(vars_dummies)), collapse = ", ")
    cli::cli_warn("The following numeric variables have fewer than three distinct values: {dummy_list}. Consider converting them to factors as standardizing them is typically not recommended.")
  }
  
  #--- Standardization ---#

  if (do_weighted_std) {
    cli::cli_inform("Standardizing variables using weighted means and standard deviations.")
    data <- data %>% dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(vars_num),
      .fns  = ~ scale_weighted(.x, w = .data$.weights)
    ))
  } else {
    if (weights_provided) {
      cli::cli_inform("Standardizing variables using UNweighted means and standard deviations.")
      cli::cli_warn("Note that standardising variables based on unweighted summary statistics is common, but the results should not be interpreted as population estimates.", .frequency = "once", .frequency_id = "lm_std_unweighted")
    }
    data <- data %>% dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(vars_num),
      .fns  = scale_blank
    ))
  }
  
  #--- Model Fitting (Robust programmatic approach) ---#
  cl <- rlang::call2("lm", formula = formula, data = quote(data), .ns = "stats")
  
  if (weights_provided) {
    cl$weights <- as.symbol(".weights")
  }
  
  cl <- rlang::call_modify(cl, !!!extras)
  
  mod <- rlang::eval_tidy(cl)
  
  if (is.null(mod$na.action) && "na.action" %in% names(extras)) {
    mod$na.action <- extras$na.action
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
#'
#' @return A tibble with the group level, a combined letters string, and
#' individual columns for each letter assigned. The tibble is sorted by level.
#' @source Algorithm based on finding maximal cliques of non-significant comparisons.
#'
#' @examples
#' # Works with pairwise.htest objects
#' library(tibble)
#' library(dplyr)
#'
#' pwtt <- pairwise.t.test(airquality$Ozone, airquality$Month)
#' get_pairwise_letters(pwtt)
#'
get_pairwise_letters <- function(tests, alpha_level = .05) {

  # Step 1: Convert input into a standardized P-value matrix
  if ("pairwise.htest" %in% class(tests)) {
    p_matrix <- tests$p.value

    # Handle single-level case (0x0 matrix from pairwise.t.test)
    if (nrow(p_matrix) == 0 || ncol(p_matrix) == 0) {
      # Extract the single level from the data.name string
      # Format is typically "variable and groupvar" - need to get unique levels
      cli::cli_warn("Only one level found in pairwise comparisons. Assigning letter 'a' to single group.")
      # Return a single-row tibble - caller needs to provide the actual level name
      # since it's not in the p_matrix
      return(tibble::tibble(level = character(0), letters = character(0)))
    }

    all_levels <- union(rownames(p_matrix), colnames(p_matrix))
    full_p_matrix <- matrix(1, nrow = length(all_levels), ncol = length(all_levels),
                            dimnames = list(all_levels, all_levels))
    full_p_matrix[rownames(p_matrix), colnames(p_matrix)] <- p_matrix
    full_p_matrix[colnames(p_matrix), rownames(p_matrix)] <- t(p_matrix)
    p_matrix <- full_p_matrix
    
  } else {
    dat_levels <- unique(c(tests$x, tests$y))
    p_matrix <- matrix(1, nrow = length(dat_levels), ncol = length(dat_levels),
                       dimnames = list(dat_levels, dat_levels))
    for(i in 1:nrow(tests)) {
      p_matrix[tests$x[i], tests$y[i]] <- tests$p_value[i]
      p_matrix[tests$y[i], tests$x[i]] <- tests$p_value[i]
    }
  }
  
  # Step 2: Create a boolean matrix of non-significant comparisons
  non_sig_matrix <- p_matrix >= alpha_level
  # Treat NaN/NA p-values as non-significant (groups can't be shown to differ)
  # NaN occurs when: SD=0, Inf values, or other numerical issues
  non_sig_matrix[is.na(non_sig_matrix)] <- TRUE
  diag(non_sig_matrix) <- TRUE
  dat_levels <- colnames(non_sig_matrix)
  n_levels <- length(dat_levels)
  
  # Step 3: Find all cliques (groups of mutually non-significant levels)
  all_cliques <- list()
  for (level in dat_levels) {
    all_cliques <- c(all_cliques, list(level)) # Add cliques of size 1
  }
  if (n_levels >= 2) {
    for (i in 2:n_levels) {
      potential_cliques <- utils::combn(dat_levels, i, simplify = FALSE)
      for (clique in potential_cliques) {
        pairs <- utils::combn(clique, 2, simplify = FALSE)
        is_clique <- all(sapply(pairs, function(p) non_sig_matrix[p[1], p[2]]))
        if(is_clique) {
          all_cliques <- c(all_cliques, list(clique))
        }
      }
    }
  }
  
  # Step 4: Filter for maximal cliques
  final_cliques <- list()
  if (length(all_cliques) > 0) {
    all_cliques <- all_cliques[order(sapply(all_cliques, length), decreasing = TRUE)]
    for (clique1 in all_cliques) {
      is_maximal <- TRUE
      for (clique2 in all_cliques) {
        if (identical(clique1, clique2)) next
        if (all(clique1 %in% clique2)) {
          is_maximal <- FALSE
          break
        }
      }
      if (is_maximal) {
        final_cliques <- c(final_cliques, list(clique1))
      }
    }
  }
  final_cliques <- unique(lapply(final_cliques, sort))
  
  # Step 5: Format output tibble
  letter_data <- tibble::tibble(level = dat_levels)
  
  if (length(final_cliques) > 0) {
    final_cliques <- final_cliques[order(sapply(final_cliques, `[`, 1))]
    names(final_cliques) <- letters[1:length(final_cliques)]
    
    # Create individual letter columns (a, b, c...) with the letter or NA
    letter_cols <- list()
    for (letter in names(final_cliques)) {
      col_data <- ifelse(letter_data$level %in% final_cliques[[letter]], letter, NA_character_)
      letter_cols[[letter]] <- col_data
    }
    letter_cols_df <- tibble::as_tibble(letter_cols)
    
    # Create the combined "letters" string column
    letters_combined <- apply(letter_cols_df, 1, function(x) paste(stats::na.omit(x), collapse=""))
    
    # Bind all columns together
    letter_data$letters <- letters_combined
    letter_data <- dplyr::bind_cols(letter_data, letter_cols_df)
    
    # Reorder columns to spec: level, letters, a, b, c...
    letter_data <- letter_data[c("level", "letters", names(final_cliques))]
  } else {
    letter_data$letters <- "" # Case with no significant differences
  }
  
  # Final Step 6: Sort the entire tibble by the level name
  letter_data <- letter_data |> dplyr::arrange(level)
  
  return(letter_data)
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
  
  assert_data_frame(data, min.rows = 2, any.missing = TRUE)
  assert_logical(na.rm, null.ok = TRUE)

  outcome_expr <- rlang::enexpr(outcome)
  groups_missing <- missing(groups)

  if (rlang::is_string(outcome_expr) && !groups_missing) {
    cli::cli_warn("Outcome and groups should be provided as bare variable names, not strings.")
    outcome_expr <- rlang::sym(outcome_expr)
  }

  if (!groups_missing) {
    groups_expr <- rlang::enexpr(groups)
    if (rlang::is_string(groups_expr)) {
      cli::cli_warn("Outcome and groups should be provided as bare variable names, not strings.")
      groups_expr <- rlang::sym(groups_expr)
    }
  }

  if (groups_missing) {
    outcome_formula <- stats::as.formula(outcome_expr, env = rlang::caller_env())
    assert_formula(outcome_formula)
    mf <- stats::model.frame(outcome_formula, data, na.action = NULL)
    if (ncol(mf) != 2) {
      cli::cli_abort("Formula input must contain exactly one outcome and one grouping variable.")
    }
    original_group_label <- names(mf)[2]
    names(mf) <- c(".tsr_outcome", ".tsr_group")
    data <- tibble::as_tibble(mf)
    outcome_sym <- rlang::sym(".tsr_outcome")
    groups_sym <- rlang::sym(".tsr_group")
  } else {
    outcome_sym <- rlang::ensym(outcome)
    groups_sym <- rlang::ensym(groups)
    assert_choice(rlang::as_string(outcome_sym), colnames(data))
    assert_choice(rlang::as_string(groups_sym), colnames(data))
    original_group_label <- rlang::as_string(groups_sym)
  }

  group_values <- dplyr::pull(data, !!groups_sym)
  outcome_values <- dplyr::pull(data, !!outcome_sym)

  if (any(is.na(group_values)) || any(is.na(outcome_values))) {
    if (is.null(na.rm) || isTRUE(na.rm)) {
      if (is.null(na.rm)) {
        n_removed <- sum(is.na(group_values) | is.na(outcome_values))
        cli::cli_warn("Missing values found in the grouping or outcome variables. {n_removed} case{if (n_removed > 1) 's' else ''} will be removed.")
      }
      data <- dplyr::filter(data, !is.na(!!groups_sym) & !is.na(!!outcome_sym))
      group_values <- dplyr::pull(data, !!groups_sym)
    } else {
      cli::cli_abort("Missing values found in the grouping or outcome variables. Set `na.rm = TRUE` to remove them.")
    }
  }

  unique_groups <- unique(group_values)
  if (length(unique_groups) < 2) {
    cli::cli_abort("The grouping variable must have at least two unique values for pairwise comparisons.")
  }

  if (!is.factor(group_values)) {
    if (!is.character(group_values) && is.null(attr(group_values, "labels"))) {
      cli::cli_warn("The grouping variable is not a factor. It will be converted to a factor.")
    }

    if (is.numeric(group_values) && is.null(attr(group_values, "labels"))) {
      haven_available <- "haven" %in% rownames(utils::installed.packages())
      if (haven_available) {
        as_factor <- getNamespace("haven")$as_factor.labelled
        data <- data %>% dplyr::mutate(!!groups_sym := as_factor(!!groups_sym))
      } else {
        cli::cli_warn("The grouping variable is a labelled numeric, but haven package is not available. It will be converted to a factor, but the group labels might be lost.")
        data <- data %>% dplyr::mutate(!!groups_sym := as.factor(!!groups_sym))
      }
    } else {
      data <- data %>% dplyr::mutate(!!groups_sym := as.factor(!!groups_sym))
    }
    group_values <- dplyr::pull(data, !!groups_sym)
  }

  pairs <- utils::combn(levels(dplyr::pull(data, !!groups_sym)), 2, simplify = FALSE)

  fmla <- stats::as.formula(
    paste(rlang::as_string(outcome_sym), "~", rlang::as_string(groups_sym))
  )

  out <- purrr::map_df(pairs, function(x) {
    dat <- dplyr::filter(data, !!groups_sym %in% x)
    t_res <- stats::t.test(fmla, dat, var.equal = var_equal, conf.level = conf_level, na.action = "na.omit") %>%
      broom::tidy()
    desc <- dat %>%
      dplyr::arrange(dplyr::desc(!!groups_sym == x[1])) %>%
      dplyr::group_by(!!groups_sym) %>%
      dplyr::summarise(
        M = mean(!!outcome_sym, na.rm = TRUE),
        var = stats::var(!!outcome_sym, na.rm = TRUE),
        .groups = "drop"
      )
    cohens_d <- (desc$M[1] - desc$M[2]) / sqrt((desc$var[1] + desc$var[2]) / 2)
    tibble::tibble(var_1 = x[1], var_2 = x[2], cohens_d = cohens_d) %>%
      dplyr::bind_cols(t_res) %>%
      dplyr::select(
        "var_1", "var_2",
        mean_1 = "estimate1",
        mean_2 = "estimate2",
        mean_diff = "estimate",
        conf_low = "conf.low",
        conf_high = "conf.high",
        t_value = "statistic",
        df = "parameter",
        p_value = "p.value",
        "cohens_d",
        test = "method"
      )
  })

  out$p_value <- stats::p.adjust(out$p_value, method = p.adjust.method)
  out$p_value_adjust <- p.adjust.method[1]
  out$group_var <- original_group_label
  out$t_value <- unname(out$t_value)
  out$df <- unname(out$df)
  out$apa <- paste0(
    "t(", round(out$df), ") = ", round_(out$t_value, 2),
    ", p ", fmt_p(out$p_value), ", d = ", round_(out$cohens_d, 2)
  )

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
  if (is.null(prefix)) {
    prefix <- NULL
  } else if (length(prefix) == 1 && is.na(prefix)) {
    prefix <- deparse(substitute(x)) %>% stringr::str_remove("^.*\\$")
    if (prefix == ".") {
      cli::cli_inform("prefix cannot be automatically extracted when x is piped in (and . is not a valid prefix). No prefix will be added.")
      prefix <- NULL
    }
  } else {
    prefix <- as.character(prefix)
  }
  x <- forcats::as_factor(x)
  dummy_names <- .clean_names(levels(x)) %>% stringr::str_to_lower()

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
