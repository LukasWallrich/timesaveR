#' Create a correlation table with summary statistics in APA style
#'
#' This function creates (and optionally saves) a correlation table with
#' summary statistics. It accepts correlation matrices from various functions
#' in this package as its first argument.
#'
#' @param cor_matrix A correlation matrix, for example returned from
#' [cor_matrix()], [svy_cor_matrix()], or [cor_matrix_mi()].
#' @param n Number of observations to calculate confidence intervals - only
#' needed if cor_matrix does not contain degrees of freedom (`df`) or numbers of observations (`n`) and confidence
#' intervals are to be calculated using z-transformations.
#' @param add_distributions Logical. Add graphs showing variable distributions?
#' Works with both regular data frames and survey design objects.
#' @param data Original data, only needed if `add_distributions = TRUE`. Can be a
#' regular data frame or a survey design object. For survey data, distributions
#' will be properly weighted using `svyhist()` and `svysmooth()`.
#' @param filename The file name to create on disk. Include '.html' extension to
#' best preserve formatting (see `gt::gtsave` for details).
#' @inheritDotParams plot_distributions -var_names
#' @param notes List of additional notes to show under the table.
#' @inheritParams sigstars
#' @param add_title Should title be added to table? Set to TRUE for default
#' title or provide a character string for custom title.
#' @param extras Tibble of additional columns to be added after the descriptives column -
#' needs to be sorted in the same order as the `desc` element in the `cor_matrix` unless
#' there is a `row_names` column. If there is, this will be used to match it to the `desc` rows.
#' @param apa_style Logical, should APA-style formatting be applied.
#' @param ci_type `r lifecycle::badge("defunct")` Recalculating CIs in this
#'   function has been removed. Please generate CIs in `cor_matrix()`.
#' @param ci_width `r lifecycle::badge("defunct")` Use `conf_level` in `cor_matrix()` instead.
#' @param ci `r lifecycle::badge("defunct")` Superseded by functionality in `cor_matrix()`.
#' @param n `r lifecycle::badge("defunct")` No longer used.
#' @return A `gt` table that can be further formatted with `gt`-functions.
#' @source Based on the apaTables `apa.cor.table()` function, but adapted to
#' accept weighted correlation matrices and work with the `gt` package instead. Code
#' for calculation of confidence intervals adapted from
#' https://medium.com/@shandou/how-to-compute-confidence-interval-for-pearsons-r-a-brief-guide-951445b9cb2d`
#' @examples
#' # Basic correlation table
#' cor_matrix(iris, var_names = c(Sepal.Length = "Sepal Length",
#'                                 Sepal.Width = "Sepal Width")) %>%
#'   report_cor_table()
#'
#' # With distributions
#' cor_matrix(iris) %>%
#'   report_cor_table(add_distributions = TRUE, data = iris,
#'                    add_title = "Iris correlations and distributions")
#'
#' # Using ESS health data
#' cor_matrix(ess_health, var_names = c(health = "Health",
#'                                       dosprt = "Physical activity",
#'                                       etfruit = "Fruit consumption")) %>%
#'   report_cor_table()
#'
#' # With survey data and distributions
#' \dontrun{
#' library(survey)
#' library(srvyr)
#' data(api)
#' dstrat <- apistrat %>% as_survey_design(1, strata = stype, fpc = fpc, weight = pw)
#' svy_cor_matrix(dstrat, var_names = c(enroll = "Enrollment",
#'                                       api00 = "API 2000",
#'                                       api99 = "API 1999")) %>%
#'   report_cor_table(add_distributions = TRUE, data = dstrat)
#' }
#'
#' @export

report_cor_table <- function(cor_matrix, ci_type = deprecated(),
                             ci_width = deprecated(), ci = deprecated(), n = deprecated(), add_distributions = FALSE,
                             data = NULL, filename = NULL, notes = list(NULL),
                             stars = NULL, add_title = FALSE, extras = NULL,
                             apa_style = TRUE, ...) {
  # Check all defunct arguments
  details <- "Recalculating CIs has been removed. Please generate CIs in `cor_matrix()` and set parameters there."

  # Call the reusable helper for each defunct argument
  check_defunct(ci, details)
  check_defunct(ci_type, details)
  check_defunct(ci_width, details)
  check_defunct(n, details)

  .check_req_packages("gt")

  if (!is.list(cor_matrix)) {
    cli::cli_abort("The {.arg cor_matrix} must be a list, typically returned
                    by {.fun cor_matrix} or similar functions.")
  }

  required_elements <- c("cors", "p.values", "std.err", "desc")
  missing_elements <- setdiff(required_elements, names(cor_matrix))
  if (length(missing_elements) > 0) {
    cli::cli_abort("The {.arg cor_matrix} is missing required element{?s}: {.field {missing_elements}}.
                    Ensure that {.arg cor_matrix} is a valid correlation matrix object, typically returned by {.fun cor_matrix} or similar functions.")
  }

  assert_data_frame(extras, null.ok = TRUE)
  assert_logical(add_distributions, null.ok = FALSE)

  if (add_distributions && is.null(data)) {
    cli::cli_abort("If {.arg add_distributions} = TRUE, the {.arg data} argument must be provided.")
  }

  if (!is.null(stars) && (!is.numeric(stars) || is.null(names(stars)))) {
    cli::cli_abort("{.arg stars} must be a named numeric vector.", call. = FALSE)
  }

  if (isTRUE(add_title)) {
    add_title <- "Means, standard deviations, and correlations with confidence intervals"
  } else if (!isFALSE(add_title) && !is.character(add_title)) {
    cli::cli_abort("'add_title' must be TRUE, FALSE, or a character string.")
  }

  if (!is.null(extras)) {
    if ("row_names" %in% names(extras)) {
      # Use 'row_names' to align 'extras' with variables
      extras <- cor_matrix$desc %>%
        dplyr::select("var") %>%
        dplyr::left_join(extras, by = c("var" = "row_names")) %>%
        dplyr::select(-"var")
    } else {
      if (nrow(extras) != nrow(cor_matrix$desc)) {
        cli::cli_abort("The number of rows in {.arg extras} does not match the number of variables in {.arg cor_matrix}.
                         Provide a {.field row_names} column in {.arg extras} to align the data.", call. = FALSE)
      } else {
        cli::cli_warn("The {.arg extras} data frame does not have a {.field row_names} column. Variables will be aligned by row order,
                         which may not be correct. Ensure that it matches {.field desc} in the {.arg cor_matrix} or include such a column.
                         Provide a {.field row_names} column to align by variable names.", call. = FALSE)
      }
    }
  }

  # Prepare plot arguments from '...'
  plot_args <- list(...)
  if ("plot_theme" %in% names(plot_args)) {
    plot_args$plot_theme <- ggplot2::theme(axis.text.x = ggplot2::element_text(size = 40)) + plot_args$plot_theme
  } else {
    plot_args$plot_theme <- ggplot2::theme(axis.text.x = ggplot2::element_text(size = 40))
  }

  if (add_distributions) {
    if (!is.null(cor_matrix$var_renames)) {
      plots <- do.call(plot_distributions, c(list(data = data, var_names = cor_matrix$var_renames), plot_args))
    } else {
      vars_to_plot <- rownames(cor_matrix$cors)
      plots <- do.call(plot_distributions, c(list(data = data %>% dplyr::select(dplyr::all_of(vars_to_plot))), plot_args))
    }

    if (is.null(extras)) {
      extras <- tibble::tibble(Distributions = seq_len(nrow(cor_matrix$cors)))
    } else {
      extras <- cbind(tibble::tibble(Distributions = seq_len(nrow(cor_matrix$cors))), extras)
    }
  }

  # Determine if confidence intervals can be shown
  show_ci <- !is.null(cor_matrix$ci.low) && !is.null(cor_matrix$ci.high)
  if (!show_ci) {
    cli::cli_warn("CI data ('ci.low' and 'ci.high') not found in 'cor_matrix'. Confidence intervals will not be shown.")
  }

  # Initialize variables
  df_col <- ncol(cor_matrix$cors)
  number_variables <- df_col
  number_columns <- df_col - 1
  output_cor <- matrix(" ", number_variables, number_columns)
  output_ci <- matrix(" ", number_variables, number_columns)
  output_descriptives <- matrix(" ", number_variables, 1)
  output_variable_names <- paste0(seq_len(number_variables), ". ", rownames(cor_matrix$cors))

  # Build the table data
  for (i in seq_len(number_variables)) {
    output_descriptives[i, 1] <- paste0(
      sprintf("%.2f", cor_matrix$desc[i, 2]),
      " (", sprintf("%.2f", cor_matrix$desc[i, 3]), ")"
    )
    for (j in seq_len(number_variables)) {
      if (j < i) {
        cor.r <- cor_matrix$cors[i, j]
        cor.p <- cor_matrix$p.values[i, j]
        output_cor[i, j] <- paste(fmt_cor(cor.r), sigstars(cor.p, stars))

        if (show_ci) {
          cor.ci.low <- cor_matrix$ci.low[i, j]
          cor.ci.high <- cor_matrix$ci.high[i, j]
          output_ci[i, j] <- paste0(
            '<span style="font-size:80%">',
            fmt_ci(cor.ci.low, cor.ci.high),
            "</span>"
          )
        }
      }
    }
  }

  # Combine correlation values and confidence intervals if available
  cor_cells <- if (show_ci) {
    paste(output_cor, output_ci, sep = "<br />")
  } else {
    output_cor
  }
  dim(cor_cells) <- dim(output_cor)

  # Construct the table cells
  cells <- if (is.null(extras)) {
    cbind(
      Variable = output_variable_names,
      desc = output_descriptives,
      cor_cells
    )
  } else {
    cbind(
      Variable = output_variable_names,
      desc = output_descriptives,
      extras,
      cor_cells,
      stringsAsFactors = FALSE
    )
  }

  colnames(cells) <- c("Variable", "desc", if (!is.null(extras)) names(extras) else NULL, seq_len(number_columns))

  cells_df <- tibble::as_tibble(cells)

  tab <- cells_df %>%
    gt::gt() %>%
    gt::fmt_markdown(columns = gt::everything())

  if (apa_style) tab <- tab %>% gt_apa_style()

  if (add_distributions) {
    if (length(plots) != nrow(cells_df)) {
      cli::cli_abort("The number of plots does not match the number of variables in the table.")
    }
    distribution_column <- which(names(cells_df) == "Distributions")
    tab <- gt_add_plots(tab, plots, distribution_column)
  }

  # Add notes
  final_notes <- notes
  final_notes <- append(final_notes, "*M* and *SD* are used to represent mean and standard deviation, respectively.")
  if (show_ci) {
    final_notes <- append(final_notes, "Values in square brackets indicate the confidence interval for each correlation.")
  }
  final_notes <- append(final_notes, .make_stars_note(stars))
  final_notes <- Filter(Negate(is.null), final_notes)

  for (note in final_notes) {
    tab <- tab %>% gt::tab_source_note(gt::md(note))
  }

  if (is.character(add_title)) {
    tab <- tab %>% gt::tab_header(
      title = add_title
    )
  }

  tab <- tab %>% gt::cols_label(desc = gt::md("*M (SD)*"))

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
#' When `bootstrap` is used, confidence intervals are calculated using the
#' Bias-Corrected and Accelerated (BCa) bootstrap method. In the rare case
#' that BCa calculations fail for a given correlation, the function will
#' automatically fall back to the simpler percentile method for that value and
#' issue a warning.
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
#' @param seed Pass an integer to set the seed for bootstrapping and thus make this reproducible
#' @return A list including the correlation matrix, p-values, standard errors, t-values, pairwise number of observations, confidence intervals, descriptives and (if var_names was provided) a tibble with old and new variable names
#' @source Adapted from
#'  http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
#' @references For evidence on the utility of the FIML estimator, see Enders, C. K. (2001)
#' The performance of the full information maximum likelihood estimator in multiple regression models with missing data
#' @examples
#' # Basic correlation matrix
#' cor_matrix(iris)
#'
#' # With renamed variables
#' cor_matrix(ess_health, var_names = c(health = "Self-rated health",
#'                                       dosprt = "Days of sport per week",
#'                                       etfruit = "Fruit consumption"))
#'
#' # Using Spearman correlations
#' cor_matrix(ess_health, method = "spearman")
#'
#' @export

cor_matrix <- function(data,
                       var_names = NULL,
                       missing = c("pairwise", "listwise", "fiml"),
                       conf_level = .95,
                       method = c("pearson", "spearman", "kendall"),
                       adjust = "none",
                       bootstrap = NULL,
                       seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)

  # Preserve attributes before selecting numeric columns
  data_attrs <- attributes(data)
  data_attrs <- data_attrs[!names(data_attrs) %in% c("names", "row.names", "class")]

  data %<>% dplyr::select_if(is.numeric)

  all_missing <- data %>% dplyr::summarise(dplyr::across(dplyr::everything(), ~all(is.na(.x)))) %>% unlist()
  if (any(all_missing)) {
    all_missing <- names(data)[all_missing]
    cli::cli_inform("{glue::glue_collapse(all_missing, sep = ', ', last = ' and ')} only have missing values. Therefore, they are dropped from the correlation table.")
    data <- data %>% dplyr::select(-dplyr::all_of(all_missing))
  }

 # Restore non-standard attributes (e.g., n_partialed from pcor_matrix)
  for (attr_name in names(data_attrs)) {
    attr(data, attr_name) <- data_attrs[[attr_name]]
  }

  if (ncol(data) < 2) cli::cli_abort("Data needs to contain at least two numeric columns.")
  missing <- dplyr::case_when(
    missing[1] == "pairwise" ~ "pairwise",
    missing[1] == "listwise" ~ "complete",
    missing[1] == "fiml" ~ "fiml",
    TRUE ~ NA_character_
  )
  
  if (is.na(missing)) assert_choice(missing, c("pairwise", "listwise", "fiml"))
  if (!is.null(bootstrap) && missing != "fiml") cli::cli_abort('bootstrapping can only be used when {.arg missing} = {.val fiml}')
  
  if (is.data.frame(var_names)) {
    assert_names(names(var_names), must.include = c("old", "new"))
    var_names <- var_names$new %>% magrittr::set_names(var_names$old)
  }
  
  if (!is.null(var_names)) {
    # Preserve attributes before selecting variables
    preserved_attrs <- attributes(data)
    preserved_attrs <- preserved_attrs[!names(preserved_attrs) %in% c("names", "row.names", "class")]

    data <- data %>% dplyr::select(dplyr::any_of(names(var_names)))

    # Restore non-standard attributes
    for (attr_name in names(preserved_attrs)) {
      attr(data, attr_name) <- preserved_attrs[[attr_name]]
    }

    miss_vars <- setdiff(names(var_names), names(data))
    if (length(miss_vars) > 0) cli::cli_warn("The following variables are included in {.arg var_names} but cannot be included into the correlation matrix - either, they are missing from data or not of type numeric: {paste(miss_vars, collapse = ', ')}")
    var_names <- var_names[intersect(names(var_names), names(data))]
  }
  
  extract_correlations <- function(mod) {
    lavaan::standardizedsolution(mod) |>
      dplyr::filter(.data$op == "~~", .data$lhs != .data$rhs) |>
      dplyr::transmute(name = paste(.data$lhs, .data$rhs, sep = "~~"), .data$est.std) |>
      tibble::deframe()
  }
  
  if (missing != "fiml") {
    
    correlation_matrix <- psych::corr.test(
      data, method = method[1], adjust = adjust,
      alpha = 1 - conf_level, use = missing
    )
    
    cors      <- correlation_matrix$r
    p.values  <- correlation_matrix$p
    std.err   <- correlation_matrix$se
    t.values  <- correlation_matrix$t
    n.matrix  <- correlation_matrix$n
    
    p.values[lower.tri(p.values)] <- t(p.values)[lower.tri(p.values)]
    
    if (is.null(dim(n.matrix))) {
      n.out <- n.matrix
      n.matrix <- cors
      n.matrix[TRUE] <- n.out
    }
    
    ci_low  <- ci_high <- p.values
    ci_low[TRUE]  <- NA
    ci_high[TRUE] <- NA
    ci_low [lower.tri(ci_low )] <- correlation_matrix$ci$lower
    ci_high[lower.tri(ci_high)] <- correlation_matrix$ci$upper
    
    desc_stat <- data |>
      psych::describe() |>
      tibble::as_tibble(rownames = "var") |>
      dplyr::select("var", M = "mean", SD = "sd")
    
  } else {
    
    .check_req_packages("lavaan", "FIML method for dealing with missing data uses the lavaan package.")
    
    mod <- lavaan::lavCor(data, missing = "fiml", estimator = "ML", output = "fit", se = "standard")
    
    vars_used <- names(data)
    
    Ms <- lavaan::parameterestimates(mod) |>
      dplyr::filter(.data$op == "~1") |>
      dplyr::select(var = "lhs", M = "est")

    desc_stat <- lavaan::parameterestimates(mod) |>
      dplyr::filter(.data$op == "~~", .data$lhs == .data$rhs) |>
      dplyr::transmute(var = .data$lhs, SD = sqrt(.data$est)) |>
      dplyr::left_join(Ms, by = "var")
    
    if (!is.null(bootstrap)) {
      
      orig <- extract_correlations(mod)
      
      .check_req_packages(c("boot", "lavaan"), "FIML with bootstrapping requires the boot and lavaan packages.")
      cli::cli_inform("Starting to bootstrap {bootstrap} resamples for BCa CIs. This might take a while.")
      

      
      stat_fun <- function(d, indices) {
        d_boot <- d[indices, , drop = FALSE]
        fit <- lavaan::lavCor(
          d_boot,
          missing   = "fiml",
          estimator = "ML",
          output    = "fit",
          se        = "none"    # SEs not needed inside the bootstrap
        )
        extract_correlations(fit)
      }
      
      # Determine if we can safely use multicore
      ncpus_available <- parallel::detectCores()
      use_multicore <- .Platform$OS.type != "windows" && !is.null(ncpus_available) && ncpus_available > 1

      # For safety, try multicore first but fall back to single-core if it fails
      boot_obj <- tryCatch({
        if (use_multicore) {
          boot::boot(
            data      = data,
            statistic = stat_fun,
            R         = bootstrap,
            sim       = "ordinary",
            parallel  = "multicore",
            ncpus     = max(1, ncpus_available - 1)
          )
        } else {
          boot::boot(
            data      = data,
            statistic = stat_fun,
            R         = bootstrap,
            sim       = "ordinary"
          )
        }
      }, error = function(e) {
        # If multicore fails, fall back to single-core
        if (use_multicore) {
          cli::cli_warn("Multicore bootstrapping failed, using single-core instead.")
        }
        boot::boot(
          data      = data,
          statistic = stat_fun,
          R         = bootstrap,
          sim       = "ordinary"
        )
      })
      
      if (is.null(colnames(boot_obj$t))) {
        colnames(boot_obj$t) <- names(stat_fun(data, seq_len(nrow(data))))
      }
      
      res <- purrr::imap_dfr(
        as.data.frame(boot_obj$t),
        \(reps, nm) {
          idx <- match(nm, colnames(boot_obj$t))            # numeric index for boot.ci
          ci  <- boot::boot.ci(boot_obj, conf = conf_level,
                               type = "bca", index = idx)

          tibble::tibble(
            term     = nm,
            est      = orig[[nm]],
            se       = sd(reps,   na.rm = TRUE),
            pvalue   = 2 * min(mean(reps <= 0, na.rm = TRUE),
                               mean(reps >= 0, na.rm = TRUE)),
            ci.lower = ci$bca[4],
            ci.upper = ci$bca[5]
          )
        }
      ) |>
        tidyr::separate(.data$term, c("lhs", "rhs"), sep = "~~")
      
    } else {
      
      res <- lavaan::standardizedsolution(mod) |>
        dplyr::filter(.data$rhs != .data$lhs) |>
        dplyr::rename(est = .data$est.std)
    }
    
    m <- matrix(nrow = length(vars_used), ncol = length(vars_used),
                dimnames = list(vars_used, vars_used))
    
    fill_matrix <- function(col) {
      for (i in seq_len(ncol(m) - 1)) {
        for (j in seq(i + 1, nrow(m))) {
          m[j, i] <- res[[col]][(res$rhs == colnames(m)[i] & res$lhs == rownames(m)[j]) |
                                  (res$lhs == colnames(m)[i] & res$rhs == rownames(m)[j])]
        }
      }
      m
    }
    
    cors     <- Matrix::forceSymmetric(fill_matrix("est"), uplo = "L")
    std.err  <- Matrix::forceSymmetric(fill_matrix("se"),  uplo = "L")
    p.values <- Matrix::forceSymmetric(fill_matrix("pvalue"), uplo = "L")
    t.values <- matrix(NA_real_, nrow = nrow(m), ncol = ncol(m),
                       dimnames = dimnames(m))
    n.matrix <- matrix(nrow(data), nrow = nrow(m), ncol = ncol(m),
                       dimnames = dimnames(m))
    ci_low  <- Matrix::forceSymmetric(fill_matrix("ci.lower"), uplo = "L")
    ci_high <- Matrix::forceSymmetric(fill_matrix("ci.upper"), uplo = "L")
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
  
  if (inherits(data, "resid_df")) {
    # Get number of partialed variables and calculate correct df
    k <- attr(data, "n_partialed")
    if (is.null(k)) {
      cli::cli_abort("Data is a {.cls resid_df} object, but does not have an {.field n_partialed} attribute. 
                     Ensure that the data was created with {.fun pcor_matrix} or similar functions.")
    }
    
    df_pcor <- cor_matrix$n - 2 - k
    
    # Store df matrix for confidence intervals
    cor_matrix$df <- df_pcor
    
    # Adjust t-values and p-values based on correct df
    cor_matrix$t.values <- sqrt(df_pcor) * cor_matrix$cors / sqrt(1 - cor_matrix$cors^2)
    raw_p_values <- stats::pt(abs(cor_matrix$t.values), df_pcor, lower.tail = FALSE) * 2
    
    if (adjust != "none") {
      # p.adjust works on a vector, so extract the lower triangle of the p-value matrix
      p_vector <- raw_p_values[lower.tri(raw_p_values)]
      adjusted_p_vector <- stats::p.adjust(p_vector, method = adjust)
      
      # Place the adjusted p-values back into the matrix
      adjusted_p_matrix <- raw_p_values
      adjusted_p_matrix[lower.tri(adjusted_p_matrix)] <- adjusted_p_vector
      adjusted_p_matrix[upper.tri(adjusted_p_matrix)] <- t(adjusted_p_matrix)[upper.tri(adjusted_p_matrix)]
      
      cor_matrix$p.values <- adjusted_p_matrix
    } else {
      cor_matrix$p.values <- raw_p_values
    }
  }
  
  if (exists("used_vars")) {
    cor_matrix$var_renames <- tibble::tibble(old = names(var_names[match(used_vars, var_names)]), new = var_names[match(used_vars, var_names)])
  }
  cor_matrix %<>% add_class("cor_matrix")
  cor_matrix
}

#' Create a correlation matrix from survey data with summary statistics and confidence intervals
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
#' @param ci_level Confidence level for confidence intervals (default = 0.95)
#' @return A correlation matrix list in the format provided by
#' `jtools::svycor()` with the addition of a `desc`-element with means
#' and standard deviations of the variables, plus `ci.low` and `ci.high` matrices.
#' @export
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("survey") & requireNamespace("srvyr")) {
#'  library(survey)
#'  library(srvyr)
#'  data(api)
#'  # Create survey design object
#'  dstrat <- apistrat %>% as_survey_design(1, strata = stype, fpc = fpc, weight = pw)
#'
#'var_names <- c(meals = "Share subsidized meals", ell = "English language learners",
#'               growth = "Performance Change")
#'
#'  # Print correlation matrix with confidence intervals
#'  result <- svy_cor_matrix(dstrat, var_names)
#'  print(result$ci.low)  # Lower confidence bounds
#'  print(result$ci.high) # Upper confidence bounds
#' }
#' }
#'
svy_cor_matrix <- function(svy_data, var_names = NULL, ci_level = 0.95) {
  .check_req_packages(c("jtools", "survey", "srvyr", "weights"))

  if (any(duplicated(var_names))) cli::cli_abort("var_names must map to unique new names")

  assert_numeric(ci_level, lower = 0, upper = 1, null.ok = FALSE)
  
  assert(
    check_class(svy_data, "survey.design"),
    check_class(svy_data, "svyrep.design")
  )
  
  svy_data %<>%
    srvyr::select_if(is.numeric)
  
  if (is.data.frame(var_names)) {
    assert_names(names(var_names), must.include = c("old", "new"))
    var_names <- var_names$new %>% magrittr::set_names(var_names$old)
  }
  
  if (!is.null(var_names)) {
    svy_data %<>%
      srvyr::select(dplyr::all_of(names(var_names)))
  }
  
  names(svy_data$variables) <- stringr::str_replace_all(names(svy_data$variables), stringr::fixed("_1"), "_.1")

  cor_matrix <- jtools::svycor(~., svy_data, na.rm = TRUE, sig.stats = TRUE)
  
  ## --- weighted descriptives -------------------------------------------------
  cor_matrix$desc <- svy_data %>%
    srvyr::select_if(is.numeric) %>%
    srvyr::summarise_all(list(`1M` = srvyr::survey_mean, `1SD` = srvyr::survey_var), na.rm = TRUE) %>%
    dplyr::select(!dplyr::matches("_se")) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "key", values_to = "value") %>%
    tidyr::separate(.data$key, into = c("var", "statistic"), sep = "_1") %>%
    tidyr::pivot_wider(names_from = "statistic", values_from = "value") %>%
    dplyr::mutate(SD = sqrt(.data$SD)) %>%
    dplyr::arrange(match(.data$var, rownames(cor_matrix$cors)))
  
  if (nrow(cor_matrix$desc) == 0) {
    cli::cli_abort("No numeric columns found - check your input and that you have installed the most recent dplyr version.")
  }
  
  ## --- undo temporary renaming of variables containing "_1" -----------------
  unmangle <- function(x) stringr::str_replace_all(x, stringr::fixed("_.1"), "_1")
  mangled_mats <- intersect(c("cors", "p.values", "t.values", "std.err"), names(cor_matrix))
  cor_matrix[mangled_mats] <- purrr::map(cor_matrix[mangled_mats], function(m) {
    rownames(m) <- unmangle(rownames(m))
    colnames(m) <- unmangle(colnames(m))
    m
  })
  cor_matrix$desc$var <- unmangle(cor_matrix$desc$var)
  
  ## --- confidence intervals for correlations --------------------------------
  if (!is.null(cor_matrix$std.err)) {
    alpha <- 1 - ci_level
    zcrit <- qnorm(1 - alpha/2)
    r <- cor_matrix$cors
    se_r <- cor_matrix$std.err
    se_z <- se_r / (1 - r^2)
    r_sel <- r
    r_sel[abs(r_sel) > .999] <- NA
    z_val <- atanh(r_sel)
    ci_low_z <- z_val - zcrit * se_z
    ci_high_z <- z_val + zcrit * se_z
    ci_low <- tanh(ci_low_z)
    ci_high <- tanh(ci_high_z)
    diag(ci_low) <- diag(ci_high) <- diag(r)
    cor_matrix$ci.low <- ci_low
    cor_matrix$ci.high <- ci_high
    cor_matrix$ci_level <- ci_level
  } else {
    cli::cli_warn("Standard errors not available from jtools::svycor(). Confidence intervals not calculated.")
  }
  
  ## --- variable renaming & sub-setting --------------------------------------
  if (!is.null(var_names)) {
    rename_mats <- intersect(c("cors", "p.values", "t.values", "std.err", "ci.low", "ci.high"), names(cor_matrix))
    cor_matrix[rename_mats] <- purrr::map(cor_matrix[rename_mats], function(x) {
      rownames(x) <- dplyr::recode(rownames(x), !!!var_names, .default = rownames(x))
      colnames(x) <- dplyr::recode(colnames(x), !!!var_names, .default = colnames(x))
      x
    })
    used_vars <- intersect(var_names, rownames(cor_matrix$cors))
    cor_matrix[rename_mats] <- purrr::map(cor_matrix[rename_mats], ~ .x[used_vars, used_vars])
    cor_matrix$desc$var <- dplyr::recode(cor_matrix$desc$var, !!!var_names, .default = cor_matrix$desc$var)
    cor_matrix$desc <- cor_matrix$desc[match(used_vars, cor_matrix$desc$var), ]
  }
  
  cor_matrix$var_renames <- if (!is.null(var_names) && exists("used_vars")) {
    tibble::tibble(old = names(var_names[match(used_vars, var_names)]), new = var_names[match(used_vars, var_names)])
  } else NULL
  
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
#' @param ci_level Confidence level for confidence intervals (default = 0.95)
#' @return A correlation matrix list similar to the format provided by
#' `jtools::svycor()` with the addition of a `desc`-element with means
#' and standard deviations of the variables. Returns `ci.low` and `ci.high` 
#' matrices with confidence intervals for correlations.
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
#'       select(etfruit, eatveg , dosprt, health, wt = pspwght)
#' add_missing <- function(x) {x[!rbinom(length(x), 1, .9)] <- NA; x}
#' ess_health <- ess_health %>% mutate(across(c(everything(), -wt), add_missing))
#' 
#' # Impute data
#' ess_health_mi <- mice(ess_health, printFlag = FALSE) 
#' ess_health_mi <- complete(ess_health_mi, "long")
#' 
#' cor_matrix <- cor_matrix_mi(ess_health_mi, weights = wt)
#' # Access confidence intervals
#' print(cor_matrix$ci.low)
#' print(cor_matrix$ci.high)

cor_matrix_mi <- function(data, weights = NULL, var_names = NULL, ci_level = 0.95) {
  .check_req_packages(c("survey", "srvyr", "mitools", "mice"))
  
  if (any(duplicated(var_names))) cli::cli_abort("var_names must map to unique new names")
  
  # Validate ci_level parameter
  if (ci_level <= 0 || ci_level >= 1) {
    cli::cli_abort("ci_level must be between 0 and 1 (exclusive)")
  }
  
  if (!".imp"  %in% names(data)) cli::cli_abort("The {.arg data} argument should contain multiple imputations, indicated by a {.field .imp} variable (see {.fun mice::complete} with {.arg action} = {.val long}).")
  
  data <- data %>% dplyr::filter(.data$.imp != 0) #Remove original data if included
  
  all_missing <- data %>% dplyr::summarise(dplyr::across(dplyr::everything(), ~all(is.na(.x)))) %>% unlist()
  
  if (any(all_missing)) {
    all_missing <- names(data)[all_missing]
    cli::cli_inform("{glue::glue_collapse(all_missing, sep = ', ', last = ' and ')} only have missing values. Therefore, they are dropped from the correlation table.")
    data <- data %>% dplyr::select(-dplyr::all_of(all_missing))
  }
  
  l <- data %>% 
    dplyr::summarise(dplyr::across(dplyr::everything(), ~length(unique(.x)))) %>%
    unlist()
  
  l[".imp"] <- 2 # Avoid removal of id column
  
  if (any(l < 2)) {
    cli::cli_warn("Some variables only have a single value, and thus no variance. They will be dropped from the correlation table: {glue::glue_collapse(names(data)[l < 2], sep = ', ', last = ' & ')}")
    data <- data %>% dplyr::select(-dplyr::all_of(names(data)[l < 2]))
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
  
  if (ct < 2) cli::cli_abort("The data needs to contain at least two numeric columns that have more than 1 distinct value.")
  
  # Pre-allocate results list for better performance
  n_pairs <- ct * (ct - 1) / 2
  results_list <- vector("list", n_pairs)
  idx <- 1
  
  for (i in seq_len(ct - 1)) {
    for (j in (i + 1):ct) {
      if (i != j) {
        ii <- variables[i]
        jj <- variables[j]
        mi_selected <- purrr::map(mi_list, magrittr::extract, c(jj, ii, dplyr::as_label(weights)))
        mi_selected <- purrr::map(mi_selected, dplyr::rename, x = 1, y = 2, wt = !!weights)
        cor.ii.jj <- purrr::map(mi_selected, do.call, what = .wtd_cor_test_lm)
        results_list[[idx]] <- data.frame(x = ii, y = jj, mice::pool(cor.ii.jj) %>% summary() %>% magrittr::extract(c("estimate", "p.value", "std.error", "statistic", "df")) %>% magrittr::extract(2, ))
        idx <- idx + 1
      }
    }
  }
  
  data <- dplyr::bind_rows(results_list)
  
  to_matrix <- function(data, names, value, set_diag = 1) {
    m <- matrix(0, length(names), length(names))
    m[as.matrix(data %>% dplyr::select("row", "column"))] <- data[[value]]
    rownames(m) <- names
    colnames(m) <- names
    if (!is.na(set_diag)) {
      diag(m) <- set_diag
    }
    empty <- lower.tri(m)
    m[empty] <- t(m)[empty]
    m
  }
  
  data %<>% dplyr::mutate(row = match(.data$x, variables), column = match(.data$y, variables))
  cors <- to_matrix(data, variables, "estimate", set_diag = 1)
  std.err <- to_matrix(data, variables, "std.error", set_diag = NA)
  p.values <- to_matrix(data, variables, "p.value", set_diag = NA)
  t.values <- to_matrix(data, variables, "statistic", set_diag = NA)
  dfs <- to_matrix(data, variables, "df", set_diag = NA)
  n <- dfs
  n[TRUE] <- nrow(mi_list[[1]])
  diag(n) <- nrow(mi_list[[1]])
  
  ## --- confidence intervals for correlations --------------------------------
  alpha <- 1 - ci_level
  zcrit <- matrix(NA_real_, nrow(dfs), ncol(dfs)) 
  offdiag <- dfs > 0                               # TRUE everywhere except diagonal
  zcrit[offdiag] <- qt(1 - alpha/2, df = dfs[offdiag])
  diag(zcrit) <- NA  
  r <- cors
  se_r <- std.err
  se_z <- se_r / (1 - r^2)
  se_z[abs(r) > .999] <- NA
  z_val <- atanh(r)
  ci_low_z <- z_val - zcrit * se_z
  ci_high_z <- z_val + zcrit * se_z
  ci.low <- tanh(ci_low_z)
  ci.high <- tanh(ci_high_z)
  diag(ci.low) <- diag(ci.high) <- diag(r)
  
  imp_svy <- survey::svydesign(~1, weights = as.formula(paste0("~`", dplyr::as_label(weights), "`")), data = mitools::imputationList(mi_list))
  
  desc <- NULL
  for (i in seq_len(ct)) {
    M <- mitools::MIcombine(with(imp_svy, survey::svymean(as.formula(paste0("~", variables[i])), design = .design)))[[1]]
    SD <- sqrt(mitools::MIcombine(with(imp_svy, survey::svyvar(as.formula(paste0("~", variables[i])), design = .design)))[[1]])
    desc <- rbind(desc, data.frame(var = variables[i], M = M, SD = SD))
  }
  
  cor_matrix <- list(cors = cors, std.err = std.err, p.values = p.values, t.values = t.values, df = dfs, n = n, 
                     ci.low = ci.low, ci.high = ci.high, ci_level = ci_level, desc = desc, tests = data)
  
  # Drop placeholder weight if auto-generated
  if ("__wt__" %in% names(cor_matrix$tests)) 
    cor_matrix$tests$`__wt__` <- NULL
  
  if (!is.null(var_names)) {
    rename_mats <- intersect(c("cors", "std.err", "p.values", "t.values", "df", "n", "ci.low", "ci.high"), names(cor_matrix))

    # Find which variables to keep (based on old names, before renaming)
    vars_to_keep <- intersect(names(var_names), rownames(cor_matrix$cors))

    # Apply renaming to matrices
    cor_matrix[rename_mats] <- purrr::map(cor_matrix[rename_mats], function(x) {
      rownames(x) <- dplyr::recode(rownames(x), !!!var_names, .default = rownames(x))
      colnames(x) <- dplyr::recode(colnames(x), !!!var_names, .default = colnames(x))
      x
    })

    # Subset to requested variables (now using new names)
    new_var_names <- var_names[vars_to_keep]
    cor_matrix[rename_mats] <- purrr::map(cor_matrix[rename_mats], ~ .x[new_var_names, new_var_names])

    # Handle descriptives
    cor_matrix$desc$var <- dplyr::recode(cor_matrix$desc$var, !!!var_names, .default = cor_matrix$desc$var)
    cor_matrix$desc <- cor_matrix$desc[cor_matrix$desc$var %in% new_var_names, ]

    used_vars <- new_var_names
  }
  
  cor_matrix$var_renames <- if (!is.null(var_names) && exists("used_vars")) {
    vars_to_keep <- intersect(names(var_names), variables)
    tibble::tibble(old = vars_to_keep, new = var_names[vars_to_keep])
  } else NULL
  
  cor_matrix
}

#' Calculate the correlation, based on weighted scaling and
#' listwise deletion *before* scaling
#' @noRd
#' @keywords internal

.wtd_cor_test_lm <- function(x, y, wt, ...) {
  
  is_missing <- is.na(x) | is.na(y) | is.na(wt)
  
  x_clean <- x[!is_missing]
  y_clean <- y[!is_missing]
  w_clean <- wt[!is_missing]
  
  x_scaled <- scale_weighted(x_clean, w_clean)
  y_scaled <- scale_weighted(y_clean, w_clean)
  
  lm(y_scaled ~ x_scaled, weights = w_clean)
}

#' Extract data from survey histogram for ggplot2 conversion
#'
#' Internal helper function that captures output from svyhist() and extracts
#' the histogram data for conversion to ggplot2
#'
#' @param formula Formula for the variable to plot
#' @param design Survey design object
#' @return A list with breaks, density, and counts
#' @keywords internal
#' @noRd
.extract_svyhist_data <- function(formula, design) {
  # Capture the histogram output without actually plotting
  # svyhist returns the histogram object, but it creates plots as a side effect
  # We need to suppress the plotting by opening a temporary null device

  # Store current device
  cur_dev <- grDevices::dev.cur()

  # Open a null device to suppress plotting
  tmp_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file = tmp_file)
  on.exit({
    grDevices::dev.off()
    unlink(tmp_file)
    if (cur_dev > 1) grDevices::dev.set(cur_dev)
  }, add = TRUE)

  # Call svyhist - it will plot to our null device
  hist_obj <- survey::svyhist(formula, design)

  list(
    breaks = hist_obj$breaks,
    density = hist_obj$density,
    counts = hist_obj$counts,
    mids = hist_obj$mids
  )
}

#' Extract data from survey density estimation for ggplot2 conversion
#'
#' Internal helper function that captures output from svysmooth() and extracts
#' the density data for conversion to ggplot2
#'
#' @param formula Formula for the variable to plot
#' @param design Survey design object
#' @return A data frame with x and y coordinates
#' @keywords internal
#' @noRd
.extract_svysmooth_data <- function(formula, design) {
  # Get the smooth/density estimate
  smooth_obj <- survey::svysmooth(formula, design, method = "locpoly")

  # Extract x and y coordinates
  tibble::tibble(
    x = smooth_obj[[1]]$x,
    y = smooth_obj[[1]]$y
  )
}

#' Create distribution plots for survey data
#'
#' Internal function that creates distribution plots for survey design objects,
#' properly accounting for survey weights using svyhist() and svysmooth()
#'
#' @inheritParams plot_distributions
#' @return A list of ggplot2 plots
#' @keywords internal
#' @noRd
.plot_distributions_svy <- function(data, var_names = NULL, plot_type = c("auto", "histogram", "density"), hist_align_y = FALSE, plot_theme = NULL) {

  # Extract variable data from survey design
  svy_data <- data$variables %>% dplyr::select_if(is.numeric)

  if (ncol(svy_data) == 0) cli::cli_abort("No numeric columns found - check your input.")

  if (is.data.frame(var_names)) {
    assert_names(names(var_names), must.include = c("old", "new"))
    var_names <- var_names$new %>% magrittr::set_names(var_names$old)
  }

  # Extract the underlying data for determining plot types
  if (!is.null(var_names)) {
    # For determining plot types, work with the variable data
    svy_data <- svy_data[names(var_names)]
  }

  if (!(plot_type[1] %in% c("auto", "histogram", "density") || test_integerish(plot_type, max.len = 1))) {
    cli::cli_abort('{.arg plot_type} must be one of {.val auto}, {.val histogram}, {.val density} or a single number')
  }

  assert(
    plot_type[1] %in% c("auto", "histogram", "density"),
    test_integerish(plot_type, max.len = 1)
  )

  # Determine which variables should use histograms vs density plots
  plot_hist <- (
    if (is.numeric(plot_type)) {
      purrr::map_lgl(svy_data, ~ (unique(.x) %>% rm_na() %>% length()) <= plot_type)
    } else {
      switch(plot_type[1],
             auto = purrr::map_lgl(svy_data, ~ (unique(.x) %>% rm_na() %>% length()) < 10),
             histogram = rep(TRUE, ncol(svy_data)),
             density = rep(FALSE, ncol(svy_data))
      )
    })

  if (is.null(var_names)) var_names <- names(svy_data)
  names(var_names) <- names(svy_data)

  # Create plots for each variable
  plots <- purrr::map2(names(var_names), plot_hist, function(var_name, use_hist) {
    formula_str <- paste0("~", var_name)
    formula_obj <- as.formula(formula_str)

    if (use_hist) {
      # Use survey-weighted histogram
      hist_data <- .extract_svyhist_data(formula_obj, data)

      # Create data frame for ggplot
      plot_data <- tibble::tibble(
        x = hist_data$mids,
        y = hist_data$density,
        xmin = hist_data$breaks[-length(hist_data$breaks)],
        xmax = hist_data$breaks[-1]
      )

      # Create ggplot histogram
      out <- ggplot2::ggplot(plot_data) +
        ggplot2::geom_rect(
          ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = 0, ymax = .data$y),
          fill = "grey", color = "white", linewidth = 3
        )

      # Set x-axis breaks
      breaks <- unique(svy_data[[var_name]]) %>% rm_na()
      if (length(breaks) <= 5) {
        out <- out + ggplot2::scale_x_continuous(breaks = breaks)
      } else {
        out <- out + ggplot2::scale_x_continuous(breaks = scales::breaks_extended(5))
      }

      return(out)
    } else {
      # Use survey-weighted density estimation
      tryCatch({
        density_data <- .extract_svysmooth_data(formula_obj, data)

        ggplot2::ggplot(density_data, ggplot2::aes(x = .data$x, y = .data$y)) +
          ggplot2::geom_area(fill = "grey", color = "black", linewidth = 0.5)
      }, error = function(e) {
        # Fallback to histogram if density estimation fails
        # Check if KernSmooth is missing and offer to install it
        if (grepl("KernSmooth", e$message, ignore.case = TRUE) &&
            !requireNamespace("KernSmooth", quietly = TRUE) &&
            interactive()) {
          if (utils::askYesNo("The KernSmooth package is required for density estimation. Would you like to install it?")) {
            utils::install.packages("KernSmooth")
            if (requireNamespace("KernSmooth", quietly = TRUE)) {
              cli::cli_inform("KernSmooth installed successfully. Please re-run your command.")
            }
          }
        }
        cli::cli_warn("Density estimation failed for {.field {var_name}}, using histogram instead: {e$message}")
        hist_data <- .extract_svyhist_data(formula_obj, data)

        plot_data <- tibble::tibble(
          x = hist_data$mids,
          y = hist_data$density,
          xmin = hist_data$breaks[-length(hist_data$breaks)],
          xmax = hist_data$breaks[-1]
        )

        ggplot2::ggplot(plot_data) +
          ggplot2::geom_rect(
            ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = 0, ymax = .data$y),
            fill = "grey", color = "white", linewidth = 3
          )
      })
    }
  })

  # Align y-axes for histograms if requested
  if (hist_align_y) {
    ymax <- purrr::map_dbl(plots, ~ ggplot2::layer_scales(.x)[["y"]][["range"]][["range"]][2])

    if (any(plot_hist)) {
      hist_max <- max(ymax[plot_hist])
      plots[plot_hist] <- purrr::map(plots[plot_hist], ~ .x + ggplot2::ylim(0, hist_max))
    }
  }

  # Apply consistent theme
  plots <- purrr::map(plots, ~ .x + ggplot2::theme_classic() +
                        ggplot2::theme(axis.title = ggplot2::element_blank(),
                                      axis.text.y = ggplot2::element_blank(),
                                      axis.ticks = ggplot2::element_blank(),
                                      axis.line = ggplot2::element_blank()))

  if (!is.null(plot_theme)) plots <- purrr::map(plots, ~ .x + plot_theme)

  names(plots) <- var_names
  plots
}

#' Create distribution charts to show in descriptive table
#'
#' Particularly in exploratory data analysis, it can be instructive to see histograms
#' or density charts. This function works with both regular data frames and survey
#' design objects, properly accounting for survey weights when present.
#'
#' @param data A dataframe or survey design object. If var_names is NULL, all numeric
#' variables in data will be used, otherwise those included in var_names will be selected.
#' For survey design objects, survey weights will be properly incorporated using
#' `svyhist()` and `svysmooth()`.
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
#' # Regular data
#' plot_distributions(mtcars, var_names = c(wt = "Weight", mpg = "Efficiency",
#'                                          am = "Transmission", gear = "Gears"))
#'
#' # Survey data
#' library(survey)
#' library(srvyr)
#' data(api)
#' dstrat <- apistrat %>% as_survey_design(1, strata = stype, fpc = fpc, weight = pw)
#' plot_distributions(dstrat, var_names = c(enroll = "Enrollment", api00 = "API Score"))
#' }
#'
plot_distributions <- function(data, var_names = NULL, plot_type = c("auto", "histogram", "density"), hist_align_y = FALSE, plot_theme = NULL) {

  # Check if this is survey data
  is_survey <- inherits(data, "survey.design") || inherits(data, "svyrep.design") || inherits(data, "tbl_svy")

  if (is_survey) {
    .check_req_packages(c("survey", "srvyr"))
    return(.plot_distributions_svy(data, var_names, plot_type, hist_align_y, plot_theme))
  }

  # Original implementation for regular data
  data %<>% dplyr::select_if(is.numeric)
  if (ncol(data) == 0) cli::cli_abort("No numeric columns found - check your input.")
  if (is.data.frame(var_names)) {
    assert_names(names(var_names), must.include = c("old", "new"))
    var_names <- var_names$new %>% magrittr::set_names(var_names$old)
  }
  
  if (!is.null(var_names)) data <- data[names(var_names)]
  
  if (!(plot_type[1] %in% c("auto", "histogram", "density") || test_integerish(plot_type, max.len = 1))) cli::cli_abort('{.arg plot_type} must be one of {.val auto}, {.val histogram}, {.val density} or a single number')
  
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
      out <- out + ggplot2::geom_histogram(na.rm = TRUE, bins = (data[[var_name]] %>% unique() %>% rm_na() %>% length()), 
                                           col = "white", linewidth = 3)
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
    ymax <- purrr::map_dbl(plots, ~ ggplot2::layer_scales(.x)[["y"]][["range"]][["range"]][2])

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
#' @return A `gt` table object (same class as `gt_table`), with the specified
#'  column's cells replaced by the corresponding rendered plots.
#' @export
#' @examples
#' \dontrun{
#' var_names <- c(wt = "Weight", am = "Transmission", mpg = "Consumption (mpg)", gear = "Gears")
#' cor_table <- cor_matrix(mtcars, var_names) %>%
#'  report_cor_table(extras = tibble::tibble(Distributions = c(seq_along(var_names))))
#' large_text <- ggplot2::theme(axis.text.x = ggplot2::element_text(size = 40))
#' distr_plots <- plot_distributions(mtcars, var_names, plot_theme = large_text)
#' gt_add_plots(cor_table, distr_plots, 3)
#' }
#'
gt_add_plots <- function(gt_table, plots, col_index) {
  if (length(plots) != nrow(gt_table$`_data`)) {
    cli::cli_warn("The number of plots should usually match the number of rows in the table - check alignment.")
  }
  purrr::reduce(
    seq_along(plots),
    .init = gt_table,
    ~ gt::text_transform(
      .x,
      locations = gt::cells_body(columns = col_index, rows = .y),
      fn = function(z) gt::ggplot_image(plots[[.y]], height = gt::px(50))
    )
  )
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
#' @examples
#' # Create and tidy a correlation matrix
#' cm <- cor_matrix(iris)
#' tidy(cm, both_directions = FALSE)
#'
#' # With ESS health data
#' cm_ess <- cor_matrix(ess_health,
#'                      var_names = c(health = "Health", dosprt = "Sport"))
#' tidy(cm_ess)
#' @export

tidy.cor_matrix <- function(x, both_directions = TRUE, ...) {
  extras <- list(...)
  if ("conf_level" %in% names(extras)) {
    cli::cli_abort("{.arg conf_level} cannot be changed in this tidy function. Please recreate the cor_matrix with the desired confidence level.")
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
    dplyr::rename(estimate = "cors", conf.high = "ci.high", conf.low = "ci.low", 
                  statistic = "t.values", std.error = "std.err", p.value = "p.values")
  
  if (both_directions) {
    out <- out %>%
      dplyr::rename(column2 = "column1", column1 = "column2") %>%
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
#' \item{std.error}{The standard error of the correlation}
#' \item{p.value}{The two-sided p-value of the correlation}
#' \item{conf.low, conf.high}{The confidence interval of the correlation
#' (at the level set when creating the matrix), if available}
#' @method tidy svy_cor_matrix
#' @export

tidy.svy_cor_matrix <- function(x, both_directions = TRUE, ...) {
  extras <- list(...)
  if ("conf_level" %in% names(extras)) {
    cli::cli_abort("{.arg conf_level} cannot be changed in this tidy function. Please recreate the cor_matrix with the desired confidence level.")
  }
  
  elements <- intersect(c("cors", "t.values", "std.err", "p.values", "ci.low", "ci.high"), names(x))

  if (!all(c("ci.low", "ci.high") %in% elements)) {
    cli::cli_inform("Confidence intervals are not available for this correlation matrix, so they are not included.")
  }

  out <- purrr::map2(x[elements], elements, function(m, name) {
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
    dplyr::rename(dplyr::any_of(c(
      estimate = "cors", statistic = "t.values", std.error = "std.err",
      p.value = "p.values", conf.low = "ci.low", conf.high = "ci.high"
    )))
  
  if (both_directions) {
    out <- out %>%
      dplyr::rename(column2 = "column1", column1 = "column2") %>%
      dplyr::bind_rows(out)
  }
  out
}

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
#' @return A correlation matrix list in the same format as returned by
#'  [cor_matrix()] (correlation matrix, p-values, standard errors, t-values,
#'  pairwise number of observations, confidence intervals, descriptives, and,
#'  if `var_names` was provided, a tibble with old and new variable names),
#'  based on the residuals after partialling out `given`.
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
      assert_names(names(args$var_names), must.include = c("old", "new"))
      var_names <- args$var_names$old
    } else {
      var_names <- names(args$var_names)
    }
    data %<>% dplyr::select(dplyr::any_of(c(given, var_names)))
  }
  
  if (!is.null(args$missing)) {
   cli::cli_abort("`missing` argument cannot be set - pcor_matrix only supports listwise deletion.") 
  }
  
  if (anyMissing(data)) {
    full <- nrow(data)
    data <- data %>% tidyr::drop_na()
    cli::cli_warn("Dropped {full - nrow(data)} rows with missing data.")
  }
  
  for (v in setdiff(names(data), given)) {
    formula <- formula(paste(v, "~", paste(given, collapse = " + ")))
    data[[v]] <- stats::residuals(lm(formula, data))
  }
  
  data %<>% dplyr::select(-dplyr::all_of(given))
  
  class(data) <- c("resid_df", class(data))
  attr(data, "n_partialed") <- length(given)
  
  args$data <- data
  
  do.call(cor_matrix, args)
  
}

