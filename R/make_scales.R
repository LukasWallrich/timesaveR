#' Create a scale by calculating item mean and returns descriptives
#'
#' This function creates a scale by calculating the mean of a set of items,
#' and prints and returns descriptives that allow to assess internal consistency
#' and spread. It is primarily based on the `psych::alpha` function, with
#' more parsimonious output and some added functionality. It also allows to specify
#' a threshold for proration so that missing data can be easily and explicitly
#' dealt with.
#' 
#' Proration is the easiest way to deal with missing data at the item-level. Here,
#' scale means are calculated for cases that have less than a given share of missing data. 
#' According to [Wu et al. (2022)](https://link.springer.com/article/10.3758/s13428-021-01671-w).
#' a 40% cut-off is defensible, so this is the default used. For more precise estimation,
#' consider item-level multiple imputation, which can be done with `make_scale_mi()`.
#'
#' @param data A dataframe
#' @param items Character vector with names of scale items (variables in data)
#' @param scale_name Name of the scale
#' @param reverse_method Should scale items be reverse coded? One of "auto" - items are
#'  reversed if that contributes to scale consistency, "none" - no items reversed,
#'  or "spec" - items specific in `reverse_items` are reversed.
#' @param reverse_items Character vector with names of scale items to be reversed
#'  (must be subset of `items`)
#' @param two_items_reliability How should the reliability of two-item scales be
#'  reported? "spearman_brown" is the recommended default, but "cronbachs_alpha"
#'  and Pearson's "r" are also supported.
#' @param r_key (optional) Numeric. Set to the possible maximum value of the scale
#' if the whole scale should be reversed, or to -1 to reverse the scale based on
#' the observed maximum.
#' @param proration_cutoff Scales scores are only calculated for cases with at most this share of missing data - see details.
#' @param print_hist Logical. Should histograms for items and resulting scale be printed?
#' @param print_desc Logical. Should descriptives for scales be printed?
#' @param return_list Logical. Should only scale values be returned, or descriptives as well?
#' @param harmonize_ranges Should items that have different ranges be rescaled? Default is *not* to do it but issue a 
#' message to flag this potential issue - set to FALSE to suppress that message. If TRUE, items are rescaled to match the first
#' item given. Alternatively pass a vector (c(min, max)) to specify the desired range.
#' @param scale_items `r lifecycle::badge("deprecated")` Use the `items` argument instead
#' @return Depends on `return_list` argument. Either just the scale values,
#'  or a list of scale values and descriptives. If descriptives are returned, check the `text` element for a convenient summary.
#' @export
#' @examples 
#' scores <- make_scale(ess_health, items = c("etfruit", "eatveg"), 
#'                      scale_name = "Healthy eating")

make_scale <- function(data, items, scale_name, reverse_method = c(
  "auto",
  "none", "spec"
), reverse_items = NULL, two_items_reliability = c(
  "spearman_brown", "cron_alpha",
  "r"
), r_key = NULL, 
proration_cutoff = .4,
print_hist = TRUE, print_desc = TRUE, return_list = FALSE,
harmonize_ranges = NULL,
scale_items = deprecated()) {
  
  if (lifecycle::is_present(scale_items)) {
    lifecycle::deprecate_warn("0.0.3", "make_scale(scale_items)", "make_scale(items)")
    items <- scale_items
  }
  
  if (!all(items %in% names(data))) cli::cli_abort("Not all items can be found in the dataset. The following are missing: {setdiff(items, names(data))}")
  
  if (data %>% dplyr::select(dplyr::any_of(items)) %>% 
      {all(vapply(., FUN = checkmate::allMissing, FUN.VALUE = logical(1)))}) {
    cli::cli_abort("All variables for scale {scale_name} only contain missing values.")
  }
  
  if (length(items) < 2) cli::cli_abort("Scales need to have at least two items specified in the {.arg items} argument.")
  
  assert_choice(reverse_method[1], c("auto", "none", "spec"))
  
  if (!is.null(reverse_items) && !reverse_method[1] == "spec") cli::cli_abort('{.arg reverse_items} should only be specified together with {.arg reverse_method} = "spec".')
  
  if (is.null(r_key)) r_key <- 0
  scale_vals <- data %>%
    dplyr::select(dplyr::one_of(items)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
  
  if (is.null(harmonize_ranges[1]) || 
      is.numeric(harmonize_ranges[1]) || 
      isTRUE(harmonize_ranges[1])) {
    
    ranges <- scale_vals %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), range_)) %>%
      unlist()
    if (length(unique(ranges)) > 1) {
      if (is.null(harmonize_ranges)) {
        cli::cli_inform("Not all items have the same range. This should be ok if respondents did not use the full range, but is likely a problem when the ranges offered were different. The observed ranges are {glue::glue_collapse(unique(ranges), sep = ', ', last = ' and ')}. If the ranges should be harmonized, rerun the function with harmonize_ranges = TRUE.")
      } else {
        if (!is.logical(harmonize_ranges[1])) {
          if (length(harmonize_ranges) != 2) cli::cli_abort("If harmonize_ranges is not NULL or logical, it needs to consist of exactly two items specifying the min and max value.")
          min_val <- harmonize_ranges[1]
          max_val <- harmonize_ranges[2]
        } else {
          x <- scale_vals %>%
            dplyr::select(1) %>%
            dplyr::summarise(dplyr::across(dplyr::everything(), list(~min(.x, na.rm = TRUE), ~max(.x, na.rm = TRUE))))
          min_val <- x[[1, 1]]
          max_val <- x[[1, 2]]
        }
        cli::cli_inform("Not all items have the same range. They will be rescaled to the range from {min_val} to {max_val}. Note that this is not valid if any of the items did not have responses that covered the entire possible range. If that is the case, you should rescale before calling the function and set harmonize_ranges = FALSE. The observed ranges are:\n* {glue::glue_collapse(paste(names(ranges), ranges, sep = ': '), sep = ',\n* ', last = ' and\n* ')}.")
        
        rescale_range <- function(x) {
          ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))) *
            (max_val - min_val) + min_val
        }
        
        scale_vals %>% 
          dplyr::summarise(dplyr::across(dplyr::everything(), rescale_range))
      }
    }
  }
  
  l <- scale_vals %>% 
    dplyr::summarise(dplyr::across(dplyr::everything(), ~length(unique(.x)))) %>%
    unlist()
  
  if (any(l < 2)) {
    cli::cli_warn("Some scale variables have zero variance. This is frequently a mistake and can lead to errors in this function: {glue::glue_collapse(names(scale_vals)[l < 2], sep = ', ', last = ' & ')}")
  }
  
  proration_cutoff <- proration_cutoff * ncol(scale_vals)
  scale_vals[rowSums(is.na(scale_vals)) > proration_cutoff, ] <- NA
  
  if ((reverse_method != "spec")[1]) {
    check.keys <- reverse_method[1] != "none"
    msg <- utils::capture.output(alpha_obj <- suppressWarnings(scale_vals %>% psych::alpha(na.rm = TRUE, check.keys = check.keys)))
    #NB: This handling depends on the exact `psych` output text - so could lead to bugs with psych updates 
    if (length(msg) > 0) {
      stringr::str_replace(msg, "To do this, run the function again with the 'check.keys=TRUE' option", "If that makes sense, rerun the function and either specify these items to be reversed or allow automatic reverse coding") %>%
        stringr::str_replace("with the total scale", paste("with the total", scale_name, "scale")) %>% 
        cat()
    }
  } else {
    alpha_obj <- suppressWarnings(scale_vals %>% psych::alpha(na.rm = TRUE, keys = reverse_items))
  }
  
  if (r_key == -1) {
    alpha_obj$scores <- psych::reverse.code(-1, alpha_obj$scores)
  } else if (r_key > 0) {
    alpha_obj$scores <- psych::reverse.code(-1, alpha_obj$scores, maxi = r_key)
  }
  #Return type changed in psych - this ensures compatibility with both versions
  if (is.list(alpha_obj$keys)) {
    x <- alpha_obj$keys[[1]]
    alpha_obj$keys <- c(1, -1)[stringr::str_detect(x, "-") + 1]
    names(alpha_obj$keys) <- stringr::str_remove(x, "^-")
  }
  
  reversed <- names(alpha_obj$keys[alpha_obj$keys == -1])
  if (length(items) == 2) {
    reliab_method <- two_items_reliability[1]
    if (two_items_reliability[1] == "spearman_brown") {
      reliab <- spearman_brown(data, items = items, SB_only = TRUE)
    } else if (two_items_reliability[1] == "cronbachs_alpha") {
      reliab <- alpha_obj$total$std.alpha
    } else if (two_items_reliability[1] == "r") {
      reliab <- cor.test(data[, items[1]], data[, items[2]], na.rm = TRUE)$estimate
    }
  } else {
    reliab_method <- "cronbachs_alpha"
    reliab <- alpha_obj$total$std.alpha
  }
  
  description_text <- glue::glue('
  
                        Descriptives for {scale_name} scale:
                        Mean: {round_(mean(alpha_obj$scores, na.rm = TRUE), 3)}  SD: {round_(sd(alpha_obj$scores, na.rm = TRUE), 3)}
                        {paste0(ifelse(length(items) == 2, paste0(two_items_reliability, ": "),
                                       "Cronbach\\\'s alpha: "), round_(reliab, 2))}')
  
  if (length(reversed) > 0) {
    description_text <- glue::glue("{description_text}
                                The following items were reverse-coded: {paste(reversed, collapse = ', ')}
                                Min and max used for reverse coding: {min(scale_vals, na.rm = TRUE)} & {max(scale_vals, na.rm = TRUE)}")
  }
  
  if (print_desc) {
    print(description_text)
  }
  if (print_hist) {
    (cbind(scale_vals, Scale = alpha_obj$scores) %>%
       tidyr::gather(
         key = "category", value = "resp",
         factor_key = TRUE
       ) %>%
       ggplot2::ggplot(ggplot2::aes(x = .data$resp)) +
       ggplot2::geom_histogram(binwidth = 0.5, na.rm = TRUE) +
       ggplot2::facet_wrap(~ .data$category) +
       ggplot2::xlab("Response") + ggplot2::ylab("Count") +
       ggplot2::ggtitle(paste0("Histograms for ", scale_name, " scale and items"))) %>%
      print(.) # %>% takes precedence over +!
  }
  
  scores <- alpha_obj$scores
  scores[is.nan(scores)] <- NA
  
  if (return_list) {
    descriptives <- list(n_items = length(items), reliability = reliab, reliability_method = reliab_method,
                         mean = mean(alpha_obj$scores, na.rm = TRUE), 
                         SD = sd(alpha_obj$scores, na.rm = TRUE), 
                         reversed = paste0(reversed, collapse = " "), 
                         rev_min = ifelse(length(reversed) > 0, min(scale_vals, na.rm = TRUE), NA), 
                         rev_max = ifelse(length(reversed) > 0, max(scale_vals, na.rm = TRUE), NA),
                         text = description_text)
    
    
    return(list(scores = scores, descriptives = descriptives))
  }
  
  scores
}

#' Report a range as a formatted string
#' 
#' This function takes a numeric vector, calculates its range
#' and returns this as a formatted string of the form
#' `1.51 - 5.42`. Missing values are ignored.
#' 
#' @param x A numeric vector.
#' @param digits The number of digits to round to.
#' @param simplify If TRUE, returns a single value if the vector is constant.
#' @noRd
#' @examples
#' range_(c(1:5))
#' range_(c(1:5), digits = 0)
#' range_(c(1, 1, 1), simplify = TRUE)
#' range_(c(1:5, NA))

range_ <- function(x, digits = 2, simplify = FALSE) {
  x <- c(x) #drop matrix dimensions
  if(simplify) {
    if (length(stats::na.omit(unique(x))) == 1) return(round_(stats::na.omit(unique(x)), digits))
  }
  glue::glue("{round_(min(x, na.rm = TRUE), digits)} - {round_(max(x, na.rm = TRUE), digits)}")
}

#' Create multiple scales by calculating item means and return descriptives
#'
#' This function creates multiple scales, returns descriptives and supports
#' reverse-coding of items.
#'
#' @param data A dataframe
#' @param items A named list of characters vectors. Names are the scale names,
#'   each vector contains the items for that scale (variables in data)
#' @param reversed How should items be reverse-coded. Can be FALSE (no reverse-coding),
#' TRUE (automatic reverse-coding of items negatively correlated with remainder), or a 
#' named list of characters vectors. Names are the scale names, each vector contains 
#' the items to be reverse-coded for that scale. If TRUE, the result must be checked carefully!
#' @inheritParams make_scale
#' @inheritDotParams make_scale print_hist
#' @return A list of two dataframes: scale values (`scores`) and
#' descriptive statistics for each scale (`descriptives`)
#' @export

make_scales <- function(data, items, reversed = FALSE, two_items_reliability = c(
                          "spearman_brown",
                          "cronbachs_alpha", "r"
                        ), print_desc = FALSE,  ...) {
  if (!all(unlist(items) %in% names(data))) cli::cli_abort("Not all items can be found in the dataset. The following are missing: {paste(setdiff(unlist(items), names(data)), collapse = ', ')}")

  assert_choice(two_items_reliability[1], c("spearman_brown", "cronbachs_alpha", "r"))

  if (!is.logical(reversed)) {
  
    if (!is.null(reversed)) {
      scales_rev <- intersect(names(items), names(reversed))
      if (length(scales_rev) > 0) {
        cli::cli_inform("The following scales will be calculated with specified reverse coding: {paste0(scales_rev, collapse = ', ')}")
        scales_rev_values <- purrr::pmap(list(
          items = items[scales_rev], scale_name = scales_rev,
          reverse_items = reversed[scales_rev]
        ), make_scale,
        data = data, return_list = TRUE,
        reverse = "spec", two_items_reliability, print_desc = print_desc, ...
        ) %>% purrr::transpose()
      } else {
        cli::cli_abort("Reverse list and variable lists cannot be matched - check that they have same names")
      }
    }
    scales_n_rev <- setdiff(names(items), names(reversed))
    
    if (length(scales_n_rev) > 0) {
      cli::cli_inform("The following scales will be calculated without reverse coding: {paste0(scales_n_rev, collapse = ', ')}")
      
    scales_n_rev_values <- purrr::map2(items[scales_n_rev], scales_n_rev, make_scale,
                                         data = data,
                                         return_list = TRUE, reverse = "none", two_items_reliability = two_items_reliability,
                                       print_desc = print_desc, ...
      ) %>% purrr::transpose()
    }
  } else if (reversed) {
    scales_rev_values <- purrr::map2(items, names(items), make_scale,
                                       data = data,
                                       return_list = TRUE, reverse = "auto", two_items_reliability = two_items_reliability,
                                     print_desc = print_desc, ...
    ) %>% purrr::transpose()    
  } else {
    scales_n_rev_values <- purrr::map2(items, names(items), make_scale,
                                       data = data,
                                       return_list = TRUE, reverse = "none", two_items_reliability = two_items_reliability,
                                       print_desc = print_desc, ...
    ) %>% purrr::transpose()
  }
  

  scores <- if (exists("scales_n_rev_values") && exists("scales_rev_values")) {
    cbind(data.frame(scales_n_rev_values$scores), data.frame(scales_rev_values$scores))
  } else if (exists("scales_rev_values")) {
    data.frame(scales_rev_values$scores)
  } else if (exists("scales_n_rev_values")) {
    data.frame(scales_n_rev_values$scores)
  } else {
    cli::cli_abort("No scales created - check inputs")
  }

  descript <- if (exists("scales_n_rev_values") && exists("scales_rev_values")) {
    c(scales_n_rev_values$descriptives, scales_rev_values$descriptives)
  } else if (exists("scales_rev_values")) {
    scales_rev_values$descriptives
  } else if (exists("scales_n_rev_values")) {
    scales_n_rev_values$descriptives
  } else {
    cli::cli_abort("No scales created - check inputs")
  }
  
  descriptives <- purrr::map2_dfr(descript, names(descript), function(descr, scale) {
    descr$text <- stringr::str_flatten(descr$text)
    descr$Scale <- scale
    data.frame(descr) %>% dplyr::select("Scale", dplyr::everything())
  })
  
  list(scores = tibble::tibble(scores), descriptives = tibble::tibble(descriptives))
}

#' Calculate Spearman-Brown reliability for two-item scale
#'
#' This function calculates the Spearman-Brown reliability for a two-item scale,
#' which is the recommended measure for two-item scales (rather than Cronbach's
#' alpha, which is used for longer scales.)
#'
#' @param data A dataframe
#' @param items Character vector of length 2, with names of the two items
#' @param name Name of the scale, relevant only if data.frame is returned
#' @param SB_only Logical, indicating whether to return only the reliability as
#' a number or a dataframe that also includes the scale name and correlation.
#' @return Either the Spearman-Brown coefficient as a single number, or a
#'   dataframe including the Pearson correlation coefficient and the scale name
#' @export
#' @source https://www.r-bloggers.com/five-ways-to-calculate-internal-consistency/
#'
spearman_brown <- function(data, items, name = "", SB_only = FALSE) {
  cor_value <- cor.test(magrittr::extract2(data, items[1]), magrittr::extract2(data, items[2]), na.rm = TRUE)$estimate
  SB_value <- (abs(cor_value) * 2) / (1 + abs(cor_value))
  if (SB_only) {
    return(as.numeric(SB_value))
  }
  result <- data.frame(correlation = cor_value, spearman_brown = SB_value, row.names = name)
  return(result)
}

#' Create scale by calculating item mean and returns descriptives for srvyr objects
#'
#' This function creates a scale by calculating the mean of a set of items,
#' and prints and returns descriptives that allow to assess internal consistency
#' and spread. It is primarily based on the `psych::alpha` function, with
#' more parsimonious output and some added functionality.
#'
#' @param data A srvyr survey object
#' @param items A characters vector containing the items for that scale
#'   (variables in data)
#' @param scale_name Character. The name of the variable the scale should be saved as
#' @param reverse Should scale items be reverse coded? One of "none" - no items reversed,
#'   or "spec" - items specific in `reverse_items` are reversed. Note: "auto" is not
#'   supported for survey objects.
#' @param reverse_items Character vector with names of scale items to be reversed
#'   (must be subset of scale_items). Used when reverse = "spec".
#' @param two_items_reliability How should the reliability of two-item scales be
#'   reported? "spearman_brown" is the recommended default, but "cronbachs_alpha"
#'   and Pearson's "r" are also supported.
#' @param r_key (optional) Numeric. Set to the possible maximum value of the scale
#' if the whole scale should be reversed, or to -1 to reverse the scale based on
#' the observed maximum
#' @param scale_items `r lifecycle::badge("deprecated")` Use the `items` argument instead
#' @param proration_cutoff Scales scores are only calculated for cases with at most this share of missing data.
#' @param print_hist Logical. Should histograms of the scale and its items be printed.
#' @param print_desc Logical. Should descriptive statistics for the scale be printed.
#' @param return_list Logical. Should only scale values be returned, or survey object with scale added?
#' @param harmonize_ranges Should items that have different ranges be rescaled? Default is *not* to do it but issue a 
#' message to flag this potential issue - set to FALSE to suppress that message. If TRUE, items are rescaled to match the first
#' item given. Alternatively pass a vector (c(min, max)) to specify the desired range.
#' @param scale_title Character. Name of scale for printing. Defaults to scale_name
#' @param reversed (deprecated) Use reverse_items instead. A characters vector containing the items that should be reverse-coded (
#'   subset of scale_items)
#' @return The survey object with the scale added as an additional variable.
#' @examples 
#' 
#' library(survey)
#' data(api)
#' 
#' svy_data <- svydesign(id = ~1, strata = ~stype, weights = ~pw, 
#'                    data = apistrat, fpc = ~fpc)
#' scale_items <- c("ell", "meals", "mobility", "col.grad", "full")
#' scale_reversed <- c("col.grad", "full")                    
#' svy_make_scale(svy_data, scale_items, "SES", reversed = scale_reversed)    

#' @export


svy_make_scale <- function(data, items, scale_name, 
                           reverse = c("none", "spec"), reverse_items = NULL,
                           two_items_reliability = c("spearman_brown", "cronbachs_alpha", "r"),
                           r_key = NULL, proration_cutoff = .4,
                           print_hist = TRUE, print_desc = TRUE, return_list = FALSE,
                           harmonize_ranges = NULL, scale_title = scale_name, 
                           reversed = NULL, scale_items = deprecated()) {

  if (lifecycle::is_present(scale_items)) {
    lifecycle::deprecate_warn("0.0.3", "make_scale(scale_items)", "make_scale(items)")
    items <- scale_items
  }

  .check_req_packages(c("survey", "rlang"))
  
  # Handle parameter alignment and backward compatibility
  if (!is.null(reversed)) {
    cli::cli_warn("The 'reversed' parameter is deprecated. Use 'reverse_items' instead.")
    if (is.null(reverse_items)) {
      reverse_items <- reversed
      reverse <- "spec"
    }
  }
  
  # Validate parameters aligned with make_scale
  if (!all(items %in% names(data$variables))) {
    cli::cli_abort("Not all items can be found in the survey data. The following are missing: {paste(setdiff(items, names(data$variables)), collapse = ', ')}")
  }
  
  if (length(items) < 2) {
    cli::cli_abort("Scales need to have at least two items specified in {.arg items}")
  }
  
  if (!is.null(reverse_items) && reverse[1] != "spec") {
    cli::cli_abort('{.arg reverse_items} should only be specified together with {.arg reverse} = "spec"')
  }
  
  if (reverse[1] == "auto") {
    cli::cli_abort('Automatic reverse coding ("auto") is not supported for survey objects. Use {.arg reverse} = "spec" with {.arg reverse_items}.')
  }

  if (!scale_title == scale_name) {
    scale_title <- paste0(scale_title, " (", scale_name, ")")
  }

  # Convert all scale items into numeric vars
  scale_items_num <- paste0(scale_items, "num")
  for (i in seq_along(scale_items)) {
    var_name <- scale_items_num[i]
    source_var <- scale_items[i]
    
    # Extract the variable data directly
    var_data <- as.numeric(unlist(data$variables[[source_var]]))
    
    # Use rlang for safe variable assignment
    data <- survey::update(data, !!rlang::sym(var_name) := var_data)
  }

  # Reverse reverse-coded items
  if (reverse[1] == "spec" && !is.null(reverse_items)) {
    reversed_num <- paste0(reverse_items, "num")
    scale_items_num <- c(setdiff(scale_items_num, reversed_num), paste0(
      reversed_num,
      "r"
    ))
    for (i in seq_along(reverse_items)) {
      var_name <- paste0(reversed_num[i], "r")
      source_data <- data$variables[[reversed_num[i]]]
      reversed_data <- psych::reverse.code(-1, source_data)
      
      data <- survey::update(data, !!rlang::sym(var_name) := reversed_data)
    }
  }

  # Create scale
  scale_vars_data <- data$variables[scale_items_num]
  scale_matrix <- do.call(cbind, scale_vars_data)
  scale_values <- rowMeans(scale_matrix, na.rm = TRUE)
  
  data <- survey::update(data, !!rlang::sym(scale_name) := scale_values)

  # Reverse full scale
  if (!is.null(r_key)) {
    current_scale <- data$variables[[scale_name]]
    
    if (r_key == -1) {
      reversed_scale <- psych::reverse.code(r_key, current_scale)
    } else if (r_key > 0) {
      reversed_scale <- psych::reverse.code(-1, current_scale, maxi = r_key)
    }
    
    data <- survey::update(data, !!rlang::sym(scale_name) := reversed_scale)
  }
  

  if (print_desc) {
    if (reverse[1] == "spec" && !is.null(reverse_items)) {
      reversed_min <- numeric()
      reversed_max <- numeric()
      reversed_num <- paste0(reverse_items, "num")
      for (i in seq_along(reverse_items)) {
        reversed_min[i] <- min(data[, reversed_num[i]]$variables, na.rm = TRUE)
        reversed_max[i] <- max(data[, reversed_num[i]]$variables, na.rm = TRUE)
      }
    }

    print(glue::glue('
    
                     Descriptives for {scale_title} scale:
                     Mean: {round_(survey::svymean(as.formula(paste0("~", scale_name)), 
                     data, na.rm = TRUE)[1], 3)}  SD: {round_(sqrt(survey::svyvar(as.formula(paste0("~", scale_name)), 
                     data, na.rm = TRUE)[1]), 3)}
                     Cronbach\'s alpha: {fmt_cor(survey::svycralpha(as.formula(.scale_formula(scale_items_num)), data, na.rm = TRUE))}'))
    
    if (reverse[1] == "spec" && !is.null(reverse_items) && length(reverse_items) > 0) {
      print(glue::glue('

The following items were reverse coded (with min and max values): \\
                       {paste0("\n", reverse_items, " (", reversed_min, ", ", reversed_max, ")", collapse = "")}'))
    }
  }

  # Print histograms of items and scale
  if (print_hist) {
    hist_vars <- c(scale_name, paste0(items, "num"))
    data2 <- NULL
    for (i in seq_along(hist_vars)) {
      x <- as.data.frame(survey::svytable(
        as.formula(paste0("~round(", hist_vars[i], ")")),
        data
      ))
      x$var <- stringr::str_split(names(x[1]), "\\.")[[1]][2]
      colnames(x)[1] <- "val"
      data2 %<>% rbind(x)
    }
    print(ggplot2::ggplot(data2, ggplot2::aes(.data$val, y = .data$Freq)) +
      ggplot2::geom_bar(stat = "identity", na.rm = TRUE) +
      ggplot2::facet_wrap(~var))
  }
  return(data)
}

# Helper function for calculating Cronbach's alpha
.scale_formula <- function(items) {
  x <- paste0("~", paste(items, "+", collapse = ""))
  stringr::str_sub(x, 1, stringr::str_length(x) - 2)
}


#' Create a scale based on multiple imputation at item level
#'
#' This function creates a scale based on multiple imputation at item level and 
#' returns pooled descriptive statistics (including Cronbach's alpha). Note that
#' this function only supports Cronbach's alpha, including for two-item scales.
#' 
#' Scale scores are returned for the raw data as well (if it is included in `data`). 
#' Descriptive statistics and reliability estimates are only based on the imputed datasets.
#'
#' @param data A dataframe containing multiple imputations, distinguished by a `.imp` 
#' variable. Typically the output from `mice::complete(mids, "long")`.
#' @inheritParams make_scale
#' @param proration_cutoff Applies only to raw data (.imp == 0) in data. Scales scores are only calculated for cases with at most this share of missing data.
#' @inheritDotParams make_scale -print_hist -two_items_reliability 
#' @param alpha_ci Should a confidence interval for Cronbach's alpha be returned? Note that this requires bootstrapping and thus makes the function much slower. TRUE corresponds to a 95% confidence interval, other widths can be specified as fractions, e.g., .9
#' @param boot For pooling, the variance of Cronbach's alpha is bootstrapped. Set number of bootstrap resamples here.
#' @param seed For pooling, the variance of Cronbach's alpha is bootstrapped. Set a seed to make this reproducible.
#' @param parallel Should bootstrapping be conducted in parallel (using `parallel`-package)? Pass a number to select the number of cores - otherwise, the function will use all but one core.
#' @param scale_items `r lifecycle::badge("deprecated")` Use the `items` argument instead
#' @source The approach to pooling Cronbach's alpha is taken from Dion Groothof [on StackOverflow](https://stackoverflow.com/a/70817748/10581449). 
#' The development of the function was motivated by [Gottschall, West & Enders (2012)](https://doi.org/10.1080/00273171.2012.640589) who showed 
#' that multiple imputation at item level results in much higher statistical power than multiple imputation at scale level.
#' @export
#' @examples 
#' library(dplyr)
#' library(mice)
#' 
#' # Create Dataset with missing data
#' ess_health <- ess_health %>% sample_n(500) %>% select(etfruit, eatveg , dosprt, health)
#' add_missing <- function(x) {x[!rbinom(length(x), 1, .9)] <- NA; x}
#' ess_health <- ess_health %>% mutate(across(everything(), add_missing))
#' 
#' # Impute data
#' ess_health_mi <- mice(ess_health, printFlag = FALSE) 
#' ess_health_mi <- complete(ess_health_mi, "long")
#' 
#' scale <- make_scale_mi(ess_health_mi, c("etfruit", "eatveg"), "healthy")

make_scale_mi <- function(data, items, scale_name, proration_cutoff = 0, seed = NULL, alpha_ci = FALSE, boot = 5000, parallel = TRUE, scale_items = deprecated(), ...) {

    if (lifecycle::is_present(scale_items)) {
      lifecycle::deprecate_warn("0.0.3", "make_scale(scale_items)", "make_scale(items)")
      items <- items
    }
  
  if (!alpha_ci) {
    args <- as.list(match.call(sys.function(1), sys.call(1), expand.dots = FALSE))[-1]
    if ("boot" %in% names(args) || "parallel" %in% names(args)) {
      cli::cli_inform("Note: boot and parallel arguments are ignored if alpha_ci = FALSE. In that case, no bootstrapping is needed.")
    }
    boot <- 1
    parallel <- FALSE
  } else if (alpha_ci == TRUE) {
    alpha_ci <- .95
  }
  
  if(alpha_ci) assert_numeric(alpha_ci, lower = 0, upper = 1)
  
  if (!".imp"  %in% names(data)) cli::cli_abort("data should contain multiple imputations, indicated by an `.imp` variable (see mice::complete() with action = 'long')")

  extras <- rlang::list2(...)
  
  if ("print_hist" %in% names(extras) && extras$print_hist == TRUE) {
    cli::cli_warn("Cannot print histograms for multiply imputed data. Argument is ignored")
    extras$print_hist <- NULL
  }

  if ("two_items_reliability" %in% names(extras) && extras$two_items_reliability  != "cron_alpha") {
    cli::cli_warn("Cannot pool estimates for Spearman-Brown reliability or correlation yet - Cronbach's alpha is returned")
    extras$two_items_reliability <- NULL
  }

  return_list <- TRUE
  if ("return_list" %in% names(extras)) {
    return_list <- extras$return_list
    extras$return_list <- NULL
  }

  print_desc <- TRUE
  
  if ("print_desc" %in% names(extras)) {
    print_desc <- extras$print_desc
    extras$print_desc <- NULL
  }
  full <- rlang::exec("make_scale", data, items = items, scale_name = scale_name, print_desc = FALSE, print_hist = FALSE, proration_cutoff = proration_cutoff, return_list = TRUE, !!!extras)
    if (full$descriptives$reversed != "") {
   rev_code <- purrr::map_chr(unlist(stringr::str_split(full$descriptives$reversed, " ")), stringr::str_trim)
   purrr::walk(rev_code, function(rev_code_now) {
     data[rev_code_now] <<- psych::reverse.code(-1, data[rev_code_now])
   })
  }

  if(is.null(seed)) seed <- floor(stats::runif(1)*1000)
  
  m <- length(setdiff(unique(data$.imp), 0)) #Ignore m = 0 -> raw data
  boot_alpha <- rep(list(NA), m)
  
  if (!rlang::is_installed(c("future", "future.apply"))) {
    cli::cli_inform("make_scale_mi() can only be parallelised when the future and future.apply packages are installed. They are not, so setting parallel = FALSE")
    parallel <- FALSE
  }
  
  if (!parallel) {
  for (i in seq_len(m)) {
    set.seed(seed + i) # fix random number generator
    sub <- data[data$.imp == i, ] %>% dplyr::select(dplyr::all_of(items))
    boot_alpha[[i]] <- .cronbach_boot(sub, boot = boot)
  } } else {
    if (boot < 1000) cli::cli_inform("Note: You requested a rather low number of bootstraps ({boot}) with parallel computing (`parallel` argument). parallel = FALSE would probably be faster.")
    if (nrow(data)/(m+1) < 100) cli::cli_inform("Note: You requested parallel computing (`parallel` argument) for a rather small sample size ({nrow(data)/(m+1)}). parallel = FALSE might be faster.")
    
    det_cores <- parallel::detectCores()
    if (parallel == TRUE) {
      parallel <- det_cores - 1
    } else if (parallel > det_cores) {
      cli::cli_warn("{parallel} cores requested, while only {det_cores} are available. Therefore, `parallel`-argument is reset to {det_cores}.")
      parallel <- det_cores
    }
    

    future::plan("multisession", workers = parallel)
    
    boot_alpha <- future.apply::future_lapply(seq_len(m), 
                                              function(i) {
      sub <- data[data$.imp == i, ] %>% dplyr::select(dplyr::all_of(items))
      .cronbach_boot(sub, boot = boot)
    }, future.seed = seed)
  }
  
  # obtain Q and U (see ?mice::pool.scalar)
  Q <- sapply(boot_alpha, function(x) x$alpha)
  U <- sapply(boot_alpha, function(x) x$var)
  pooled_alpha <- pool_estimates(mice::pool.scalar(Q, U), ci = alpha_ci)

  descr <- tibble::tibble(score = full$scores) %>% dplyr::bind_cols(data) %>% 
    dplyr::filter(.data$.imp != 0)  %>% dplyr::group_by(.data$.imp) %>%
    dplyr::summarise(mean = mean(.data$score, na.rm = TRUE), sd = sd(.data$score, na.rm = TRUE), .groups = "drop") %>%
    dplyr::summarise(mean = mean(.data$mean), sd = mean(.data$sd))
  
  description_text <- glue::glue('
    
                     Descriptives for {scale_name} scale:
                     Mean: {round_(descr$mean, 3)}  SD: {round_(descr$sd, 3)}
                     Cronbach\'s alpha: {round_(pooled_alpha[[1]], 2)}{ifelse(alpha_ci, paste0(", ", fmt_pct(alpha_ci,0), " CI [",round_(pooled_alpha[[2]], 2), ",", round_(pooled_alpha[[3]], 2), "]"), "")}')

    
  if (full$descriptives$reversed != "") {
    rev_details <- data %>% dplyr::select(dplyr::all_of(rev_code)) %>% 
      tidyr::pivot_longer(dplyr::everything()) %>% 
      dplyr::group_by(.data$name) %>% 
      dplyr::summarise(min = min(.data$value, na.rm = TRUE), max = max(.data$value, na.rm = TRUE))
    
    description_text <- glue::glue("{description_text}
                                   The following items were reverse-coded: {glue::glue_collapse(rev_details$name, sep = ', ', last = ' and ')}
                                   ")
    if(length(unique(rev_details$min)) == 1 && length(unique(rev_details$max)) == 1) {
      description_text <- glue::glue("{description_text}
                                     Min and max used for reverse coding: {unique(rev_details$min)} & {unique(rev_details$max)}")
    } else {
      description_text <- glue::glue("{description_text}
                                     Min values used for reverse coding: {glue::glue_collapse(rev_details$min, sep = ', ', last = ' and ')}
                                     Max values used for reverse coding: {glue::glue_collapse(rev_details$max, sep = ', ', last = ' and ')}
                                     ")
    }
  }
    if (print_desc) {
      print(description_text)
    }
    
    if (return_list) {
      descriptives <- list(n_items = length(items), reliability = pooled_alpha[[1]], reliability_method = "cron_alpha",
                           mean = descr$mean, SD = descr$sd, 
                           m_imputations = length(unique(data$`.imp`)),
                           text = description_text)
      if (alpha_ci) {
        descriptives$reliability_ci_lower <- pooled_alpha[[2]]
        descriptives$reliability_ci_upper <- pooled_alpha[[3]]
        descriptives$reliability_ci_level <- alpha_ci
      }
      
      if (exists("rev_details")) descriptives$reversed <- rev_details
      return(list(scores = full$scores, descriptives = descriptives))
    }
    
  full$scores

}

# Helper functions based on https://stackoverflow.com/a/70817748/10581449

.cronbach_boot <- function(list_compl_data, boot = 1e4) {
  n <- nrow(list_compl_data)
  p <- ncol(list_compl_data)
  total_variance <- stats::var(rowSums(list_compl_data))
  item_variance <- sum(apply(list_compl_data, 2, sd)^2)
  alpha <- (p / (p - 1)) * (1 - (item_variance/total_variance))
  out <- list(alpha = alpha)
  boot_alpha <- numeric(boot)
  
  if (boot > 1000) {
    cli::cli_progress_bar("Computing Cronbach's alpha", total = boot)
  }
    for (i in seq_len(boot)) {
      boot_dat <- list_compl_data[sample(seq_len(n), replace = TRUE), ]
      total_variance <- stats::var(rowSums(boot_dat))
      item_variance <- sum(apply(boot_dat, 2, sd)^2)
      boot_alpha[i] <- (p / (p - 1)) * (1 - (item_variance/total_variance))
      if (boot > 1000) {
        cli::cli_progress_update()
      }
    }
  if (boot > 1000) {
    cli::cli_progress_done()
  }
    out$var <- stats::var(boot_alpha)
  
  return(out)
}

# pooled estimates
pool_estimates <- function(x, ci = .95) {
  out <- c(
    alpha = x$qbar,
    lwr = x$qbar - stats::qt(1-(ci/2), x$df) * sqrt(x$t),
    upr = x$qbar + stats::qt(1-(ci/2), x$df) * sqrt(x$t)
  )
  return(out)
}
