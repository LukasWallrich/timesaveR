#' Create a correlation table with summary statistics in APA style
#'
#' This function, creates (and optionally saves) a correlation table with
#' summary statistics. It accepts correlation matrices from various functions
#' in this package as its first argument
#'
#' @param cor_matrix A correlation matrix, for example returned from
#' [cor_matrix()], [svy_cor_matrix()], or [cor_matrix_mi()]
#' @param ci Method to create CI - default is to use any given in the cor_matrix,
#' and otherwise to compute them using z-transformations. The simple SE method
#' should not be used, but is provided for compatibility.
#' @param n Number of observations to calculate confidence intervals - only
#' needed if cor_matrix does not contain degrees of freedom (df) or numbers of observations (n) and confidence
#' intervals are to be calculated using z-transformations
#' @param add_distributions Add graphs showing variable distributions?
#' @inheritDotParams plot_distributions -var_names
#' @param data Original data, only needed if `add_distribution = TRUE`
#' @param notes List of additional notes to show under the table.
#' @param filename the file name to create on disk. Include '.html' extension to
#' best preserve formatting (see gt::gtsave for details)
#' @inheritParams sigstars
#' @param add_title Should title be added to table? Set to TRUE for default
#' title or to character for custom title
#' @param extras Tibble of additional columns to be added after the descriptives column -
#'  needs to be sorted in the same order as the `desc` element in the cor_matrix unless 
#'  there is a `row_names` column. If there is, this will be used to match it to the `desc` rows.
#' @param apa_style Logical, should APA-style formatting be applied
#' @source Based on the apaTables `apa.cor.table()` function, but adapted to
#' accept weighted correlation matrices and work with the `gt` package instead. Code
#' for calculation of confidence intervals adapted from
#' https://medium.com/@shandou/how-to-compute-confidence-interval-for-pearsons-r-a-brief-guide-951445b9cb2d`
#' @return A table that can be printed in the RStudio console to be shown in the
#' viewer. Unless it is to be post-processed with further `gt` functions, it should
#' usually be saved by passing a filename argument.
#' @export

report_cor_table <- function(cor_matrix, ci = c("given", "z_transform", "simple_SE"),
                             n = NULL, add_distributions = FALSE, data = NULL,
                             filename = NULL, notes = list(NULL), stars = NULL,
                             add_title = FALSE, extras = NULL, apa_style = TRUE, ...) {
  .check_req_packages("gt")


  if (add_title) {
    add_title <- "Means, standard deviations, and correlations with confidence intervals"
  }

  assert_data_frame(extras, null.ok = TRUE)

  if (add_distributions && is.null(data)) {
    stop("If add_distributions = TRUE, data needs to be provided.", call. = FALSE)
  }
  
  if (add_distributions && "survey.design" %in% class(data)) {
    stop("Presently, distributions cannot be shown for weighted survey data. ",
         "Set add_distributions to FALSE", call. = FALSE)
  }

  plot_args <- list(...)
  if ("plot_theme" %in% names(plot_args)) {
    plot_args$plot_theme <- ggplot2::theme(axis.text.x = ggplot2::element_text(size = 40)) + plot_args$plot_theme
  } else {
    plot_args$plot_theme <- ggplot2::theme(axis.text.x = ggplot2::element_text(size = 40))
  }

  #Sort extras or warn
  
  if (!is.null(extras)) {
    if (!"row_names" %in% names(extras)) {
      warning("The ordering of the 'extras' argument cannot be checked unless a `row_names` column is included. Ensure that it matches 'desc' in the correlation matrix or include such a column.") 
      
    } else {
      extras <- extras %>% 
        dplyr::left_join(cor_matrix$desc %>% dplyr::select(.data$var), ., by = c("var" = "row_names")) %>%
        dplyr::select(-.data$var)
    }
    
    }
  

  if (add_distributions) {
    if (!is.null(cor_matrix$var_renames)) {
      plots <- do.call(plot_distributions, c(list(data = data, var_names = cor_matrix$var_renames), plot_args))
    } else {
      plots <- data %>% dplyr::select(dplyr::all_of(rownames(cor_matrix$cors))) %>% 
        {do.call(plot_distributions, c(list(data = .), plot_args))}
    }

    if (is.null(extras)) {
      extras <- tibble::tibble(Distributions = seq_len(nrow(cor_matrix$cors)))
    } else {
      extras <- cbind(tibble::tibble(Distributions = seq_len(nrow(cor_matrix$cors))), extras)
    }
  }
  df_col <- dim(cor_matrix[[1]])[2]
  number_variables <- df_col
  number_columns <- df_col - 1
  output_cor <- matrix(" ", number_variables, number_columns)
  output_ci <- matrix(" ", number_variables, number_columns)
  output_descriptives <- matrix(
    " ", number_variables,
    1
  )
  output_variable_names <- paste(as.character(seq_len(number_variables)),
    ". ", rownames(cor_matrix[[1]]),
    sep = ""
  )
  
  if (!is.null(cor_matrix[["ci.low"]]) && ci[1] == "given") {
    get_cor.ci.low <- function(cor_matrix, cor.r, cor.se, i, j, df) {
      if (!is.null(cor_matrix[["ci.low"]])) {
        return(cor_matrix[["ci.low"]][i, j])
      }
    }

    get_cor.ci.high <- function(cor_matrix, cor.r, cor.se, i, j, df) {
      if (!is.null(cor_matrix[["ci.high"]])) {
        return(cor_matrix[["ci.high"]][i, j])
      }
    }
  } else if ("z_transform" %in% ci && (!is.null(cor_matrix[["df"]]) || !is.null(cor_matrix[["n"]])) && is.null(n)) {
    get_cor.ci.low <- function(cor_matrix, cor.r, cor.se, i, j, n) {
      z_prime <- .5 * log((1 + cor.r) / (1 - cor.r))
      CI_low <- z_prime - 1.96 * 1 / sqrt(n - 3)
      tanh(CI_low)
    }

    get_cor.ci.high <- function(cor_matrix, cor.r, cor.se, i, j, df) {
      z_prime <- .5 * log((1 + cor.r) / (1 - cor.r))
      n <- df + 1
      CI_low <- z_prime + 1.96 * 1 / sqrt(n - 3)
      tanh(CI_low)
    }

    if (is.null(cor_matrix[["df"]])) {
      cor_matrix$df <- cor_matrix$cors
      if (!is.null(cor_matrix$n)) {
        cor_matrix$df[] <- cor_matrix$n - 1
      } else {
      cor_matrix$df[] <- n - 1
      }
    }
  } else {
    message("Confidence intervals are calculated based on correlation
            coefficient +/- 2 SE. This is generally not recommended!")
    if ("z_transform" %in% ci && (is.null(cor_matrix[["df"]]) && is.null(n))) {
      message("This is because n is not provided and cor_matrix does not contain df. Change either to enable z-transformed confidence intervals.")
    }

    get_cor.ci.low <- function(cor_matrix, cor.r, cor.se, i, j, df) {
      cor.r - 2 * cor.se
    }

    get_cor.ci.high <- function(cor_matrix, cor.r, cor.se, i, j, df) {
      cor.r + 2 * cor.se
    }
  }



  for (i in seq_len(number_variables)) {
    output_descriptives[i, 1] <- paste0(
      sprintf("%.2f", cor_matrix$desc[i, 2]),
      " (", sprintf("%.2f", cor_matrix$desc[i, 3]), ")"
    )
    for (j in seq_len(number_variables)) {
      if ((j < i)) {
        cor.r <- cor_matrix$cors[i, j]
        cor.p <- cor_matrix$p.values[i, j]
        cor.se <- cor_matrix$std.err[i, j]
        cor.df <- cor_matrix$df[i, j]
        cor.ci.low <- get_cor.ci.low(cor_matrix, cor.r, cor.se, i, j, cor.df)
        cor.ci.high <- get_cor.ci.high(cor_matrix, cor.r, cor.se, i, j, cor.df)
        output_cor[i, j] <- paste(fmt_cor(cor.r), sigstars(cor.p, stars))
        output_ci[i, j] <- paste0(
          '<span style="font-size:80%">',
          fmt_ci(cor.ci.low, cor.ci.high),
          "</span>"
        )
      }
    }
  }

  cor_cells <- paste(output_cor, output_ci, sep = "<br />")
  dim(cor_cells) <- dim(output_cor)

  if (is.null(extras)) {
    cells <- cbind(
      matrix(output_variable_names, ncol = 1),
      output_descriptives, cor_cells
    )
  } else {
    cells <- cbind(
      matrix(output_variable_names, ncol = 1),
      output_descriptives, extras, cor_cells,
      stringsAsFactors = FALSE
    )
  }


  colnames(cells) <- c("Variable", "desc", colnames(extras), seq_len(length(output_variable_names) - 1))

  cells_df <- tibble::as_tibble(cells)

  tab <- cells_df %>%
    gt::gt() %>%
    gt::fmt_markdown(columns = gt::everything())

  if (apa_style) tab <- tab %>% gt_apa_style()

  if (add_distributions) tab <- gt_add_plots(tab, plots, 3)

  notes %<>% c("*M* and *SD* are used to represent mean and standard deviation, respectively. Values in square brackets indicate the 95% confidence interval for each correlation.")

  notes %<>% c(.make_stars_note())

  notes <- Filter(Negate(is.null), notes)
  for (i in seq_along(notes)) {
    tab <- tab %>% gt::tab_source_note(gt::md(notes[[i]]))
  }

  if (is.character(add_title)) {
    tab <- tab %>% gt::tab_header(
      title = add_title
    )
  }

  tab <- tab %>% gt::cols_label(desc = gt::md("*M (SD)*"))
  # TK - add md formatting to extras column titles

  if (!is.null(filename)) {
    gt::gtsave(tab, filename)
  } else {
    return(tab)
  }
}



#' Calculate correlation matrix with significance tests and descriptives
#'
#' Calculates the correlation matrix between the numeric variables in a given dataframe and
#' includes descriptives (mean and standard deviation) - ready for creating a nice table with [report_cor_table()]
#'
#' By setting missing to "fiml", this function uses the lavaan package to estimate the correlations (and descriptives)
#' by using a full-information maximum likelihood algorithm. This estimates the covariances separately
#' for each pattern of missingness and then combines them based on the frequency of each pattern. This
#' will take longer than pairwise deletion, but should yield more precise estimates in the presence of
#' missing data. To date, FIML correlation matrices can be obtained with the [psych::corFiml()]
#' function, but that function does not report significance tests or confidence intervals, or with the
#' [lavaan::lavCor()] function - this function also uses `lavaan` for the estimation,
#' but facilitates bootstrapping and returns the results in a more accessible format.
#'
#' @param data Dataframe. Only numeric variables are included into correlation matrix.
#' @param var_names A named character vector with new variable names or a tibble as provided by [get_rename_tribbles()]
#' for variables. If NULL, then the variables are not renamed. If names are provided, only the variables included here are retained.
#' This is most helpful when the results are passed to some print function, such as [report_cor_table()]
#' @param missing How should missing data be dealt with? Options are "pairwise" deletion, "listwise" deletion or "fiml" for full
#' information maximum likelihood estimation of the correlation table. Note that if you use "fiml", this will also be applied to
#' the estimation of means and standard deviations.
#' @param conf_level Confidence level to use for confidence intervals, defaults to .95
#' @inheritParams psych::corr.test
#' @param bootstrap When using FIML estimation (with missing = "fiml"), significance tests and confidence
#' intervals can be bootstrapped. If you want to do that, pass the number of desired bootstrap resamples
#' (e.g., 5000) to this parameter, but beware that this can take a while.
#' @return A list including the correlation matrix, p-values, standard errors, t-values, pairwise number of observations, confidence intervals, descriptives and (if var_names was provided) a tibble with old and new variable names
#' @source Adapted from
#'  http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
#' @references For evidence on the utility of the FIML estimator, see Enders, C. K. (2001)
#' The performance of the full information maximum likelihood estimator in multiple regression models with missing data
#' @export


cor_matrix <- function(data,
                       var_names = NULL,
                       missing = c("pairwise", "listwise", "fiml"),
                       conf_level = .95,
                       method = c("pearson", "spearman", "kendall"),
                       adjust = "none",
                       bootstrap = NULL) {
  
  data %<>% dplyr::select_if(is.numeric)
  all_missing <- data %>% dplyr::summarise(dplyr::across(dplyr::everything(), ~all(is.na(.x)))) %>% unlist()
  if (any(all_missing)) {
    all_missing <- names(data)[all_missing]
    message(glue::glue_collapse(all_missing, sep = ", ", last = " and "), " only have missing values. Therefore, they are dropped from the correlation table.")
    data <- data %>% dplyr::select(-dplyr::all_of(all_missing))
  }
  if (ncol(data) < 2) stop("Data needs to contain at least two numeric columns.")
  missing <- dplyr::case_when(
    missing[1] == "pairwise" ~ "pairwise",
    missing[1] == "listwise" ~ "complete",
    missing[1] == "fiml" ~ "fiml",
    TRUE ~ NA_character_
  )

  if (is.na(missing)) assert_choice(missing, c("pairwise", "listwise", "fiml"))
  if (!is.null(bootstrap) && missing != "fiml") stop('bootstrapping can only be used when missing = "fiml"')

  if (is.data.frame(var_names)) {
    assert_names(names(var_names), must.include = c("old", "new"))
    var_names <- var_names$new %>% magrittr::set_names(var_names$old)
  }

  if (!is.null(var_names)) {
    data <- data %>% dplyr::select(dplyr::any_of(names(var_names)))
    miss_vars <- setdiff(names(var_names), names(data))
    if (length(miss_vars) > 0) warning("The following variables are included in var_names but cannot be included into the correlation matrix - either, they are missing from data or not of type numeric: ", paste(miss_vars, collapse = ", "), call. = FALSE)
    var_names <- var_names[intersect(names(var_names), names(data))]
  }

  if (!missing == "fiml") {

    # Compute correlation matrix
    correlation_matrix <- psych::corr.test(data, method = method[1], adjust = adjust, alpha = 1 - conf_level, use = missing)
    cors <- correlation_matrix$r # Matrix of correlation coefficients
    p.values <- correlation_matrix$p # Matrix of p-value
    std.err <- correlation_matrix$se # Matrix of standard errors
    t.values <- correlation_matrix$t # Matrix of t-values
    n.matrix <- correlation_matrix$n # Matrix of pairwise counts

    # Copy (possibly) adjusted p-values into lower half that will be used by report_cor_table()
    p.values[lower.tri(p.values)] <- t(p.values)[lower.tri(p.values)]

    # Ensure that n is a named matrix (corr.test returns single number for complete data)
    if (is.null(dim(n.matrix))) {
      n.out <- n.matrix
      n.matrix <- cors
      n.matrix[TRUE] <- n.out
    }

    ci_low <- p.values
    ci_low[TRUE] <- NA
    ci_low[lower.tri(ci_low)] <- correlation_matrix$ci$lower

    ci_high <- p.values
    ci_high[TRUE] <- NA
    ci_high[lower.tri(ci_high)] <- correlation_matrix$ci$upper


    desc_stat <- data %>%
      psych::describe() %>%
      data.frame() %>%
      tibble::rownames_to_column("var") %>%
      dplyr::select(.data$var, M = .data$mean, SD = .data$sd)
  } else {
    .check_req_packages("lavaan", "FIML method for dealing with missing data uses the lavaan package. ")
    
    mod <- lavaan::lavCor(data, missing = "fiml", estimator = "ML", output = "fit", se = "standard")
    
    vars_used <- names(data)

    Ms <- lavaan::parameterestimates(mod) %>%
      dplyr::filter(.data$op == "~1") %>%
      dplyr::select(var = .data$lhs, M = .data$est)

    desc_stat <- lavaan::parameterestimates(mod) %>%
      dplyr::filter(.data$op == "~~" & .data$lhs == .data$rhs) %>%
      dplyr::transmute(var = .data$lhs, SD = sqrt(.data$est)) %>%
      dplyr::left_join(Ms, ., by = "var")

    if (!is.null(bootstrap)) {
      message("Starting to bootstrap ", bootstrap, " resamples. This might take a while.")

      # Estimate correlations with CIs
      extract_correlations <- function(mod) {
        res <- lavaan::standardizedsolution(mod) %>% 
          dplyr::filter(.data$op == "~~", .data$lhs != .data$rhs)
        res$est %>% magrittr::set_names(paste0(res$lhs, "~~", res$rhs))
      }
      
      
      res <- lavaan::bootstrapLavaan(mod, R = bootstrap, FUN = extract_correlations) %>%
        t() %>%
        data.frame() %>%
        tibble::rownames_to_column("rowid") %>%
        tidyr::separate(.data$rowid, c("rhs", "lhs"), sep = "~~") %>%
        tidyr::gather(-.data$lhs, -.data$rhs, key = "rep", value = "coef") %>%
        dplyr::group_by(.data$lhs, .data$rhs) %>%
        dplyr::summarise(
          M = mean(.data$coef),
          pvalue = ifelse(.data$M > 0, mean(.data$coef < 0) + mean(.data$coef > 2 * .data$coef), mean(.data$coef > 0) + mean(.data$coef < 2 * .data$coef)),
          ci.lower = stats::quantile(.data$coef, (1 - conf_level) / 2), ci.upper = stats::quantile(.data$coef, 1 - (1 - conf_level) / 2), se = sd(.data$coef),
          .groups = "drop"
        ) %>%
        dplyr::rename(est = .data$M)
    } else {
      res <- lavaan::standardizedsolution(mod) %>% dplyr::filter(.data$rhs != .data$lhs) %>% dplyr::rename(est = .data$est.std)
    }
    m <- matrix(nrow = length(vars_used), ncol = length(vars_used)) %>%
      magrittr::set_rownames(vars_used) %>%
      magrittr::set_colnames(vars_used)
    fill_matrix <- function(x) {
      for (i in seq_len(ncol(m) - 1)) {
        for (j in seq(i + 1, nrow(m))) {
          m[j, i] <- res[[x]][(res$rhs == colnames(m)[i] & res$lhs == rownames(m)[j]) |
                               (res$lhs == colnames(m)[i] & res$rhs == rownames(m)[j])]        
          }
      }
      m
    }
    cors <- fill_matrix("est") %>% Matrix::forceSymmetric(uplo = "L")
    std.err <- fill_matrix("se") %>% Matrix::forceSymmetric(uplo = "L")
    p.values <- fill_matrix("pvalue") %>% Matrix::forceSymmetric(uplo = "L")
    t.values <- m 
    t.values[TRUE] <- NA
    n.matrix <- m
    n.matrix[TRUE] <- nrow(data)
    ci_low <- fill_matrix("ci.lower") %>% Matrix::forceSymmetric(uplo = "L")
    ci_high <- fill_matrix("ci.upper") %>% Matrix::forceSymmetric(uplo = "L")
  }

  cor_matrix <- list(cors = cors, std.err = std.err, p.values = p.values, t.values = t.values, n = n.matrix, ci.low = ci_low, ci.high = ci_high, desc = desc_stat)

  if (!is.null(var_names)) {
    cor_matrix[1:7] <- purrr::map(cor_matrix[1:7], function(x) {
      rownames(x) <- var_names[rownames(x)]
      colnames(x) <- var_names[colnames(x)]
      x
    })
    used_vars <- intersect(var_names, rownames(cor_matrix[[1]]))
    cor_matrix[1:7] <- purrr::map(cor_matrix[1:7], function(x) x[used_vars, used_vars])
    cor_matrix$desc$var <- var_names[cor_matrix$desc$var]
    cor_matrix$desc <- cor_matrix$desc[match(cor_matrix$desc$var, used_vars), ]
  }

  cor_matrix$var_renames <- NULL


  if (exists("used_vars")) {
    cor_matrix$var_renames <- tibble::tibble(old = names(var_names[match(used_vars, var_names)]), new = var_names[match(used_vars, var_names)])
  }
  cor_matrix %<>% add_class("cor_matrix")
  cor_matrix
}

#' Create a correlation matrix from survey data with summary statistics
#'
#' This function wraps jtools::svycor() so that it works in a srvyr-pipeline,
#' runs bootstrapped significance-tests and calculates weighted summary
#' statistics. Only numeric variables are included in the result.
#'
#' @param svy_data A survey object created with the survey or srvyr package. Only
#' numeric variables will be included in the result.
#' @param var_names A named character vector with new variable names or a tibble as provided by [get_rename_tribbles()]
#' for variables. If NULL, then the variables are not renamed. If names are provided, only the variables included here are retained.
#' This is most helpful when the results are passed to some print function, such as [report_cor_table()]
#' @param return_n Should the sample size be returned? Note that this is *not* survey-weighted, and should thus only be used when the weights add up to the number of observations.
#' @return A correlation matrix list in the format provided by
#' `jtools::svycor()` with the addition of a `desc`-element with means
#' and standard deviations of the variables.
#' @export
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("survey") & requireNamespace("srvyr")) {
#'   library(survey)
#'   library(srvyr)
#'   data(api)
#'   # Create survey design object
#'   dstrat <- apistrat %>% as_survey_design(1, strata = stype, fpc = fpc, weight = pw)
#'
#'var_names <- c(meals = "Share subsidized meals", ell = "English language learners",
#'               growth = "Performance Change")
#'
#'   # Print correlation matrix
#'   svy_cor_matrix(dstrat, var_names)
#' }
#' }
#'
svy_cor_matrix <- function(svy_data, var_names = NULL, return_n = FALSE) {
  .check_req_packages(c("jtools", "survey", "srvyr", "weights"))

  assert_class(svy_data, "survey.design")

  if (!inherits(svy_data, "tbl_svy")) svy_data %<>% srvyr::as_survey(svy_data)

  svy_data %<>%
    srvyr::select_if(is.numeric)

  if (is.data.frame(var_names)) {
    assert_names(names(var_names), must.include = c("old", "new"))
    var_names <- var_names$new %>% magrittr::set_names(var_names$old)
  }

  if (!is.null(var_names)) {
    svy_data %<>%
      srvyr::select(dplyr::one_of(names(var_names)))
  }
  
  names(svy_data$variables) <- stringr::str_replace_all(names(svy_data$variables), stringr::fixed("_1"), "_.1")
  
  cor_matrix <- jtools::svycor(~., svy_data, na.rm = TRUE, sig.stats = TRUE)
  if (return_n) {
    cor_matrix_no_survey <- psych::corr.test(svy_data$variables)
    cor_matrix$n <- cor_matrix_no_survey$n
  }
  
  cor_matrix$desc <- svy_data %>%
    srvyr::select_if(is.numeric) %>%
    srvyr::summarise_all(.funs = list(`1M` = srvyr::survey_mean, `1SD` = srvyr::survey_var), na.rm = TRUE) %>%
    dplyr::select(!dplyr::matches("_se")) %>%
    tidyr::gather(key = "key", value = "value") %>%
    tidyr::separate(.data$key, into = c("var", "statistic"), sep = "_1") %>%
    tidyr::spread(.data$statistic, .data$value) %>%
    dplyr::mutate(SD = sqrt(.data$SD)) %>%
    dplyr::arrange(match(.data$var, rownames(cor_matrix[[1]])))

  if (nrow(cor_matrix$desc) == 0) {
    stop("No numeric columns found - check your input and that you have
         installed the most recent dplyr version.", call. = FALSE)
  }

  if (!is.null(var_names)) {
    cor_matrix[c(1, 4:6)] <- purrr::map(cor_matrix[c(1, 4:6)], function(x) {
      rownames(x) <- rownames(x) %>% stringr::str_replace_all(var_names)
      colnames(x) <- colnames(x) %>% stringr::str_replace_all(var_names)
      x
    })
    used_vars <- intersect(var_names, rownames(cor_matrix[[1]]))
    cor_matrix[c(1, 4:6)] <- purrr::map(cor_matrix[c(1, 4:6)], function(x) x[used_vars, used_vars])
    cor_matrix$desc$var %<>% stringr::str_replace_all(var_names)
    cor_matrix$desc <- cor_matrix$desc[match(used_vars, cor_matrix$desc$var), ]
  }

  cor_matrix$var_renames <- NULL

  if (exists("used_vars")) {
    cor_matrix$var_renames <- tibble::tibble(old = names(var_names[match(used_vars, var_names)]), new = var_names[match(used_vars, var_names)])
  }

  cor_matrix %<>% add_class("svy_cor_matrix")
  
  cor_matrix
}

#' Create a correlation matrix from multiply imputed data with weights
#'
#' This function takes an imputationList with a vector of weights and returns
#' a correlation matrix for all numeric variables as well as a list of
#' descriptives that pools the results across all imputations.
#'
#' Variables starting with . are dropped, as these are likely to be .imp and .id
#' from mice. If you want correlations for such variables, rename them.
#'
#' @param mi_list A list of dataframes of multiple imputation results
#' @param weights A variable within mi_list that gives the survey weights
#' @param var_names A named character vector with new variable names or a tibble as provided by [get_rename_tribbles()]
#' for variables. If NULL, then the variables are not renamed. If names are provided, only the variables included here are retained.
#' This is most helpful when the results are passed to some print function, such as [report_cor_table()]
#' To facilitate post-processing, correlations with original variable
#' names are returned in the `tests` element.
#' @return A correlation matrix list similar to the format provided by
#' `jtools::svycor()` with the addition of a `desc`-element with means
#' and standard deviations of the variables.
#' @source Takes some code from the `miceadds::micombine.cor` function,
#' but adapted to use weights and return in the format accepted by
#' `report_cor_table`
#' @export
#' @rdname package-deprecated
#' @seealso cor_matrix_mi

wtd_cor_matrix_mi <- function(mi_list, weights, var_names = NULL) {
  .Deprecated("cor_matrix_mi")
  mice::complete(mi_list, "long")
  cor_matrix_mi(mi_list, weights, var_names)
}

#' Create a (weighted) correlation matrix from multiply imputed data
#'
#' This function takes an imputationList with a vector of weights and returns
#' a correlation matrix for all numeric variables as well as a list of
#' descriptives that pools the results across all imputations.
#'
#' Variables starting with . are dropped, as these are likely to be .imp and .id
#' from mice. If you want correlations for such variables, rename them.
#'
#' @param data A dataframe with multiple imputations distinguished by a `.imp` variable. 
#' Typically the output from `mice::complete(mids, "long").
#' @param weights A variable within `data` that gives the survey weights
#' @param var_names A named character vector with new variable names or a tibble as provided by [get_rename_tribbles()]
#' for variables. If NULL, then the variables are not renamed. If names are provided, only the variables included here are retained.
#' This is most helpful when the results are passed to some print function, such as [report_cor_table()]
#' To facilitate post-processing, correlations with *original* variable
#' names are returned in the `tests` element.
#' @return A correlation matrix list similar to the format provided by
#' `jtools::svycor()` with the addition of a `desc`-element with means
#' and standard deviations of the variables.
#' @source Takes some code from the `miceadds::micombine.cor` function,
#' but adapted to use weights and return in the format accepted by
#' [`report_cor_table`]
#' @export
#' @examples 
#' 
#' library(dplyr)
#' library(mice)
#' 
#' # Create Dataset with missing data
#' ess_health <- ess_health %>% sample_n(500) %>% 
#'     select(etfruit, eatveg , dosprt, health, wt = pspwght)
#' add_missing <- function(x) {x[!rbinom(length(x), 1, .9)] <- NA; x}
#' ess_health <- ess_health %>% mutate(across(c(everything(), -wt), add_missing))
#' 
#' # Impute data
#' ess_health_mi <- mice(ess_health, printFlag = FALSE) 
#' ess_health_mi <- complete(ess_health_mi, "long")
#' 
#' cor_matrix <- cor_matrix_mi(ess_health_mi, weights = wt)

cor_matrix_mi <- function(data, weights = NULL, var_names = NULL) {
  .check_req_packages(c("survey", "srvyr", "mitools", "mice"))

  if (!".imp"  %in% names(data)) stop("data should contain multiple imputations, indicated by an `.imp` variable (see mice::complete() with action = 'long'")

  data <- data %>% dplyr::filter(.data$.imp != 0) #Remove original data if included
  
  all_missing <- data %>% dplyr::summarise(dplyr::across(dplyr::everything(), ~all(is.na(.x)))) %>% unlist()
  
  if (any(all_missing)) {
    all_missing <- names(data)[all_missing]
    message(glue::glue_collapse(all_missing, sep = ", ", last = " and "), " only have missing values. Therefore, they are dropped from the correlation table.")
    data <- data %>% dplyr::select(-dplyr::all_of(all_missing))
  }
    
  if (is.data.frame(var_names)) {
    assert_names(names(var_names), must.include = c("old", "new"))
    var_names <- var_names$new %>% magrittr::set_names(var_names$old)
  }

  if (!missing(weights)) {
    weights <- rlang::enquo(weights)
  } else {
    data$`__wt__` <- 1
    weights <- rlang::quo(!!rlang::sym("__wt__"))
  }

  data <- data %>% dplyr::select(tidyselect::vars_select_helpers$where(is.numeric))

  mi_list <- data %>% split(.$.imp)

  mi_list <- purrr::map(mi_list, dplyr::select, !!weights, dplyr::everything(), -dplyr::matches("^\\."))

  variables <- names(mi_list[[1]])
  variables <- variables[-1]
  ct <- length(variables)

  data <- NULL
  for (i in seq_len(ct - 1)) {
    for (j in (i + 1):ct) {
      if (i != j) {
        ii <- variables[i]
        jj <- variables[j]
        mi_selected <- purrr::map(mi_list, magrittr::extract, c(jj, ii, dplyr::as_label(weights)))
        mi_selected <- purrr::map(mi_selected, dplyr::rename, x = 1, y = 2, wt = !!weights)
        cor.ii.jj <- purrr::map(mi_selected, do.call, what = .wtd_cor_test_lm)
        data <- rbind(data, data.frame(x = ii, y = jj, mice::pool(cor.ii.jj) %>% summary() %>% magrittr::extract(c("estimate", "p.value", "std.error", "statistic", "df")) %>% magrittr::extract(2, )))
      }
    }
  }


  to_matrix <- function(data, names, value) {
    m <- matrix(0, length(names), length(names))
    m[as.matrix(data %>% magrittr::extract(c("row", "column")))] <- data[[value]]
    rownames(m) <- names
    colnames(m) <- names
    diag(m) <- 1
    empty <- lower.tri(m)
    m[empty] <- t(m)[empty]
    m
  }

  data %<>% dplyr::mutate(row = match(.data$x, variables), column = match(.data$y, variables))
  cors <- to_matrix(data, variables, "estimate")
  std.err <- to_matrix(data, variables, "std.error")
  p.values <- to_matrix(data, variables, "p.value")
  t.values <- to_matrix(data, variables, "statistic")
  dfs <- to_matrix(data, variables, "df")
  n <- dfs
  n[TRUE] <- nrow(mi_list[[1]])

  imp_svy <- survey::svydesign(~1, weights = as.formula(paste0("~`", dplyr::as_label(weights), "`")), data = mitools::imputationList(mi_list))


  desc <- NULL
  for (i in seq_len(ct)) {
    M <- mitools::MIcombine(with(imp_svy, survey::svymean(as.formula(paste0("~", variables[i])), design = .design)))[[1]]
    SD <- sqrt(mitools::MIcombine(with(imp_svy, survey::svyvar(as.formula(paste0("~", variables[i])), design = .design)))[[1]])
    desc <- rbind(desc, data.frame(var = variables[i], M = M, SD = SD))
  }

  cor_matrix <- list(cors = cors, std.err = std.err, p.values = p.values, t.values = t.values, df = dfs, n = n, desc = desc, tests = data)

  if (!is.null(var_names)) {
    cor_matrix[1:5] <- purrr::map(cor_matrix[1:5], function(x) {
      rownames(x) <- rownames(x) %>% stringr::str_replace_all(var_names)
      colnames(x) <- colnames(x) %>% stringr::str_replace_all(var_names)
      x
    })
    used_vars <- intersect(var_names, rownames(cor_matrix[[1]]))
    cor_matrix[1:5] <- purrr::map(cor_matrix[1:5], function(x) x[used_vars, used_vars])
    rownames(cor_matrix$desc) <- rownames(cor_matrix$desc) %>% stringr::str_replace_all(var_names)
    cor_matrix$desc$var %<>% stringr::str_replace_all(var_names)
    cor_matrix$desc <- cor_matrix$desc[match(used_vars, cor_matrix$desc$var), ]
  }

  cor_matrix$var_renames <- NULL

  if (exists("used_vars")) {
    cor_matrix$var_renames <- tibble::tibble(old = names(var_names[match(used_vars, var_names)]), new = var_names[match(used_vars, var_names)])
  }

  cor_matrix
}

.wtd_cor_test_lm <- function(x, y, wt, ...) {
  lm(scale(y) ~ scale(x), weights = wt)
}

#' Create distribution charts to show in descriptive table
#'
#' Particularly in exploratory data analysis, it can be instructive to see histograms
#' or density charts.
#'
#' @param data A dataframe - if var_names is NULL, all numeric variables in x will be used, otherwise those included in var_names will be selected
#' @param var_names A named character vector with new variable names or a tibble as provided by [get_rename_tribbles()]
#' If provided, only variables included here will be plotted. Apart from that, this will only determine the names of the list items, so it is most relevant if the output is to be combined with a correlation matrix, e.g., from `cor_matrix()`
#' @param plot_type Type of plot that should be produced - `histogram` or `density` plot. If `auto`,
#' histograms are produced for variables that take fewer than 10 unique values, density plots for others. If a number is provided,
#' that number is used as the maximum number of unique values for which a histogram is used.
#' @param hist_align_y Should histograms use the same y-axis, so that bin heights are comparable? Defaults to FALSE
#' @param plot_theme Additional theme_ commands to be added to each plot
#' @export
#' @return A list of plots
#' @examples
#' \dontrun{
#' plot_distributions(mtcars, var_names = c(wt = "Weight", mpg = "Efficiency",
#'                    am = "Transmission", gear = "Gears"))
#' }
#'
plot_distributions <- function(data, var_names = NULL, plot_type = c("auto", "histogram", "density"), hist_align_y = FALSE, plot_theme = NULL) {
  data %<>% dplyr::select_if(is.numeric)
  if (is.data.frame(var_names)) {
    assert_names(names(var_names), must.include = c("old", "new"))
    var_names <- var_names$new %>% magrittr::set_names(var_names$old)
  }

  if (!is.null(var_names)) data <- data[names(var_names)]

  if (!(plot_type[1] %in% c("auto", "histogram", "density") || test_integerish(plot_type, max.len = 1))) stop('plot_type must be one of "auto", "histogram", "density" or a single number', call. = FALSE)

  assert(
    plot_type[1] %in% c("auto", "histogram", "density"),
    test_integerish(plot_type, max.len = 1)
  )

  plot_hist <- (
    if (is.numeric(plot_type)) {
      purrr::map_lgl(data, ~ (unique(.x) %>% rm_na() %>% length()) <= plot_type)
    } else {
      switch(plot_type[1],
        auto = purrr::map_lgl(data, ~ (unique(.x) %>% rm_na() %>% length()) < 10),
        histogram = rep(TRUE, ncol(data)),
        density = rep(FALSE, ncol(data))
      )
    })


  if (is.null(var_names)) var_names <- names(data)
  names(var_names) <- names(data)
  plots <- purrr::map2(names(var_names), plot_hist, function(var_name, plot_hist) {
    out <- ggplot2::ggplot(data, ggplot2::aes(!!rlang::sym(var_name)))
    if (plot_hist) {
      out <- out + ggplot2::geom_histogram(na.rm = TRUE, bins = (data[[var_name]] %>% unique() %>% rm_na() %>% length()), col = "white", size = 3)
      breaks <- (data[[var_name]] %>% unique() %>% rm_na())
      if (length(breaks) <= 5) {
        return(out + ggplot2::scale_x_continuous(breaks = breaks))
      } else {
        return(out + ggplot2::scale_x_continuous(breaks = scales::breaks_extended(5)))
      }
    } else {
      out + ggplot2::geom_density(na.rm = TRUE, fill = "grey", outline.type = "full")
    }
  })

  if (hist_align_y) {
    ymax <- purrr::map_dbl(plots, ~ ggplot2::layer_scales(.x) %>%
      extract2("y") %>%
      extract2("range") %>%
      extract2("range") %>%
      extract(2))

    if (any(plot_hist)) {
      hist_max <- max(ymax[plot_hist])
      plots[plot_hist] <- purrr::map(plots[plot_hist], ~ .x + ggplot2::ylim(0, hist_max))
    }
  }
  plots <- purrr::map(plots, ~ .x + ggplot2::theme_classic() + ggplot2::theme(axis.title = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), axis.line = ggplot2::element_blank()))
  if (!is.null(plot_theme)) plots <- purrr::map(plots, ~ .x + plot_theme)
  names(plots) <- var_names
  plots
}

#' Add plots into gt table column
#'
#' This function takes a list of ggplot2 plots and adds them into a gt table column.
#'
#' @param gt_table A gt table to add the plots into
#' @param plots A list of ggplot2 plots, typically with the same length as the number of rows in gt_table
#' @param col_index The index of the column in gt_table that is to be overwritten with the plots
#'
#' @export
#' @examples
#' \dontrun{
#' var_names <- c(wt = "Weight", am = "Transmission", mpg = "Consumption (mpg)", gear = "Gears")
#' cor_table <- cor_matrix(mtcars, var_names) %>%
#'   report_cor_table(extras = tibble::tibble(Distributions = c(seq_along(var_names))))
#' large_text <- ggplot2::theme(axis.text.x = ggplot2::element_text(size = 40))
#' distr_plots <- plot_distributions(mtcars, var_names, plot_theme = large_text)
#' gt_add_plots(cor_table, distr_plots, 3)
#' }
#'
gt_add_plots <- function(gt_table, plots, col_index) {
  purrr::walk(seq_along(plots), function(x) {
    gt_table <<- gt::text_transform(gt_table, gt::cells_body(col_index, x), fn = function(y) {
      plots[[x]] %>%
        gt::ggplot_image(height = gt::px(50))
    })
  })
  gt_table
}

#' Tidy a correlation matrix
#' 
#' This function turns the correlation matrix returned by [cor_matrix()] and 
#' its relatives into a tidy dataframe. Note that by default, results for both the `cor(A, B)` 
#' and `cor(B, A)` are returned, while entries for `A, A`, i.e. the values on the diagonal, 
#' are never included.
#' 
#' @param x A `cor_marix` object returned from
#' [cor_matrix()]
#' @param both_directions Should both  `cor(A, B)` 
#' and `cor(B, A)` be returned. Defaults to `TRUE`.
#' @param ... Additional arguments. Not used. Needed to match generic signature
#' only. 
#' @return A [tibble::tibble()] with columns:
#' \item{column1}{Name of the first variable}
#' \item{column2}{Name of the second variable}
#' \item{estimate}{The estimated value of the correlation}
#' \item{statistic}{The t-statistic used for significance testing}
#' \item{p.value}{The two-sided p-value of the correlation}
#' \item{n}{Number of observations used to compute the correlation}
#' \item{ci.low}{Lower bound of confidence interval. Width is determined in call to [cor_matrix()]}
#' \item{ci.high}{Upper bound of confidence interval. Width is determined in call to [cor_matrix()]}
#' @method tidy cor_matrix
#' @export

tidy.cor_matrix <- function(x, both_directions = TRUE, ...) {
  extras <- list(...)
  if ("conf_level" %in% names(extras)) {
    stop("conf_level cannot be changed in this tidy function. Please recreate the cor_matrix with the desired confidence level")
  }

  out <- purrr::map2(x[1:7], names(x[1:7]), function(m, name) {
    ind <- which(lower.tri(m, diag = FALSE), arr.ind = TRUE)
    nn <- dimnames(m)
    res <- tibble::tibble(
      column1 = nn[[1]][ind[, 1]],
      column2 = nn[[2]][ind[, 2]],
      val = m[ind]
    )
    names(res)[3] <- name
    res
  }) %>% 
    purrr::reduce(dplyr::left_join, by = c("column1", "column2")) %>%
    dplyr::rename(estimate = .data$cors, conf.high = .data$ci.high, conf.low = .data$ci.low, statistic = .data$t.values, std.error = .data$std.err, p.value = .data$p.values)

  if (both_directions) {
    out <- out %>%
      dplyr::rename(column2 = .data$column1, column1 = .data$column2) %>%
      dplyr::bind_rows(out)
  }
  out
}

#' Tidy a survey-weighted correlation matrix
#' 
#' This function turns the correlation matrix returned by [svy_cor_matrix()]. Note that by default, results for both the `cor(A, B)` 
#' and `cor(B, A)` are returned, while entries for `A, A`, i.e. the values on the diagonal, 
#' are never included.
#' 
#' @param x A `svy_cor_marix` object returned from
#' [svy_cor_matrix()]
#' @param both_directions Should both  `cor(A, B)` 
#' and `cor(B, A)` be returned. Defaults to `TRUE`.
#' @param ... Additional arguments. Not used. Needed to match generic signature
#' only. 
#' @return A [tibble::tibble()] with columns:
#' \item{column1}{Name of the first variable}
#' \item{column2}{Name of the second variable}
#' \item{estimate}{The estimated value of the correlation}
#' \item{statistic}{The t-statistic used for significance testing}
#' \item{p.value}{The two-sided p-value of the correlation}
#' @method tidy svy_cor_matrix
#' @export

tidy.svy_cor_matrix <- function(x, both_directions = TRUE, ...) {
  extras <- list(...)
  if ("conf_level" %in% names(extras)) {
    stop("conf_level cannot be changed in this tidy function. Please recreate the cor_matrix with the desired confidence level")
  }
  
  message("Presently, confidence intervals cannot be calculated for survey-weighted correlations.")
  
  out <- purrr::map2(x[c(1, 4:6)], names(x[c(1, 4:6)]), function(m, name) {
    ind <- which(lower.tri(m, diag = FALSE), arr.ind = TRUE)
    nn <- dimnames(m)
    res <- tibble::tibble(
      column1 = nn[[1]][ind[, 1]],
      column2 = nn[[2]][ind[, 2]],
      val = m[ind]
    )
    names(res)[3] <- name
    res
  }) %>% 
    purrr::reduce(dplyr::left_join, by = c("column1", "column2")) %>%
    dplyr::rename(estimate = .data$cors, statistic = .data$t.values, std.error = .data$std.err, p.value = .data$p.values)
  
  if (both_directions) {
    out <- out %>%
      dplyr::rename(column2 = .data$column1, column1 = .data$column2) %>%
      dplyr::bind_rows(out)
  }
  out
}

