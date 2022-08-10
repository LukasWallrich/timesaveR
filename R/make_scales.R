#' Create a scale by calculating item mean and returns descriptives
#'
#' This function creates a scale by calculating the mean of a set of items,
#' and prints and returns descriptives that allow to assess internal consistency
#' and spread. It is primarily based on the \code{psych::alpha} function, with
#' more parsimonious output and some added functionality.
#'
#' @param data A dataframe
#' @param scale_items Character vector with names of scale items (variables in data)
#' @param scale_name Name of the scale
#' @param reverse Should scale items be reverse coded? One of "auto" - items are
#'   reversed if that contributes to scale consistency, "none" - no items reversed,
#'   or "spec" - items specific in \code{reverse_items} are reversed.
#' @param reverse_items Character vector with names of scale items to be reversed
#'   (must be subset of scale_items)
#' @param two_items_reliability How should the reliability of two-item scales be
#'   reported? "spearman_brown" is the recommended default, but "cronbachs_alpha"
#'   and Pearson's "r" are also supported.
#' @param r_key (optional) Numeric. Set to the possible maximum value of the scale
#' if the whole scale should be reversed, or to -1 to reverse the scale based on
#' the observed maximum.
#' @param print_hist Logical. Should histograms for items and resulting scale be printed?
#' @param print_desc Logical. Should descriptives for scales be printed?
#' @param return_list Logical. Should only scale values be returned, or descriptives as well?
#' @return Depends on \code{return_list} argument. Either just the scale values,
#'   or a list of scale values and descriptives. If descriptives are returned, check the `text` element for a convenient summary.
#' @export
#'

make_scale <- function(data, scale_items, scale_name, reverse = c(
                         "auto",
                         "none", "spec"
                       ), reverse_items = NULL, two_items_reliability = c(
                         "spearman_brown", "cron_alpha",
                         "r"
                       ), r_key = NULL, print_hist = TRUE, print_desc = TRUE, return_list = FALSE) {
  if (!all(scale_items %in% names(data))) stop("Not all scale_items can be found in the dataset. The following are missing: ", paste(setdiff(scale_items, names(data)), collapse = ", "), call. = FALSE)

  if (data %>% dplyr::select(dplyr::any_of(scale_items)) %>% {
    all(vapply(., FUN = checkmate::allMissing, FUN.VALUE = logical(1)))
  }) {
    stop("All variables for scale ", scale_name, " only contain missing values.", call. = FALSE)
  }

  assert_choice(reverse[1], c("auto", "none", "spec"))

  if (!is.null(reverse_items) && !reverse[1] == "spec") stop('reverse_items should only be specified together with reverse = "spec"')

  if (is.null(r_key)) r_key <- 0
  scale_vals <- data %>%
    dplyr::select(dplyr::one_of(scale_items)) %>%
    dplyr::mutate_all(as.numeric)
  if ((reverse != "spec")[1]) {
    check.keys <- reverse[1] != "none"
    msg <- capture.output(alpha_obj <- suppressWarnings(scale_vals %>% psych::alpha(na.rm = TRUE, check.keys = check.keys)))
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
  reversed <- names(alpha_obj$keys[alpha_obj$keys == -1])
  if (length(scale_items) == 2) {
    reliab_method <- two_items_reliability[1]
    if (two_items_reliability[1] == "spearman_brown") {
      reliab <- spearman_brown(data, items = scale_items, SB_only = TRUE)
    } else if (two_items_reliability[1] == "cronbachs_alpha") {
      reliab <- alpha_obj$total$std.alpha
    } else if (two_items_reliability[1] == "r") {
      reliab <- cor.test(data[, scale_items[1]], data[, scale_items[2]], na.rm = TRUE)$estimate
    }
  } else {
    reliab_method <- "cronbachs_alpha"
    reliab <- alpha_obj$total$std.alpha
  }

  description_text <- glue::glue('
    
                     Descriptives for {scale_name} scale:
                     Mean: {round_(mean(alpha_obj$scores, na.rm = TRUE), 3)}  SD: {round_(sd(alpha_obj$scores, na.rm = TRUE), 3)}
                     {paste0(ifelse(length(scale_items) == 2, paste0(two_items_reliability, ": "),
                                    "Cronbach\'s alpha: "), round_(reliab, 2))}')
  
  if (length(reversed) > 0) {
    description_text <- c(description_text, paste(c("The following items were reverse coded: ", reversed),
                sep = ", ",
                collapse = ", "
    ), "\n",
    paste(
      "Min and max used for reverse coding:", min(scale_vals, na.rm = TRUE),
      max(scale_vals, na.rm = TRUE)
    ))
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
  if (return_list) {
      descriptives <- list(n_items = length(scale_items), reliability = reliab, reliability_method = reliab_method,
                           mean = mean(alpha_obj$scores, na.rm = TRUE), 
                           SD = sd(alpha_obj$scores, na.rm = TRUE), 
                           reversed = paste0(reversed, collapse = " "), 
                           rev_min = ifelse(length(reversed) > 0, min(scale_vals, na.rm = TRUE), NA), 
                           rev_max = ifelse(length(reversed) > 0, max(scale_vals, na.rm = TRUE), NA),
                           text = description_text)
    
    
    return(list(scores = alpha_obj$scores, descriptives = descriptives))
  }

  alpha_obj$scores
}

#' Create multiple scales by calculating item means and returns descriptives
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
  if (!all(unlist(items) %in% names(data))) stop("Not all items can be found in the dataset. The following are missing: ", paste(setdiff(unlist(items), names(data)), collapse = ", "), call. = FALSE)

  assert_choice(two_items_reliability[1], c("spearman_brown", "cronbachs_alpha", "r"))

  if (!is.logical(reversed)) {
  
    if (!is.null(reversed)) {
      scales_rev <- intersect(names(items), names(reversed))
      if (length(scales_rev) > 0) {
        print(paste0(
          "The following scales will be calculated with specified reverse coding: ",
          paste0(scales_rev, collapse = ", ")
        ))
  
        scales_rev_values <- purrr::pmap(list(
          scale_items = items[scales_rev], scale_name = scales_rev,
          reverse_items = reversed[scales_rev]
        ), make_scale,
        data = data, return_list = TRUE,
        reverse = "spec", two_items_reliability, print_desc = print_desc, ...
        ) %>% purrr::transpose()
      } else {
        stop("Reverse list and variable lists cannot be matched - check that they have same names")
      }
    }
    scales_n_rev <- setdiff(names(items), names(reversed))
    
    if (length(scales_n_rev) > 0) {
      print(paste0(
        "The following scales will be calculated without reverse coding: ",
        paste0(scales_n_rev, collapse = ", ")
      ))
      
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
    stop("No scales created - check inputs")
  }

  descript <- if (exists("scales_n_rev_values") && exists("scales_rev_values")) {
    c(scales_n_rev_values$descriptives, scales_rev_values$descriptives)
  } else if (exists("scales_rev_values")) {
    scales_rev_values$descriptives
  } else if (exists("scales_n_rev_values")) {
    scales_n_rev_values$descriptives
  } else {
    stop("No scales created - check inputs")
  }

  descriptives <- do.call(rbind.data.frame, descript) %>% 
    tibble::rownames_to_column(var = "Scale")

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
#' and spread. It is primarily based on the \code{psych::alpha} function, with
#' more parsimonious output and some added functionality.
#'
#' @param data A srvyr survey object
#' @param scale_items A characters vector containing the items for that scale
#'   (variables in data)
#' @param scale_name Character. The name of the variable the scale should be saved as
#' @param print_desc Logical. Should descriptive statistics for the scale be printed.
#' @param print_hist Logical. Should histograms of the scale and its items be printed.
#' @param scale_title Character. Name of scale for printing. Defaults to scale_name
#' @param reversed (optional) A characters vector containing the items that should be reverse-coded (
#'   subset of scale_items)
#' @param r_key (optional) Numeric. Set to the possible maximum value of the scale
#' if the whole scale should be reversed, or to -1 to reverse the scale based on
#' the observed maximum
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

## TODO
### Merge/align with standard make_scale functions


svy_make_scale <- function(data, scale_items, scale_name, print_hist = TRUE, print_desc = TRUE, 
                           scale_title = scale_name, reversed = NULL, r_key = NULL) {
 
  .check_req_packages("survey")

  if (!scale_title == scale_name) {
    scale_title <- paste0(scale_title, " (", scale_name, ")")
  }

  # Convert all scale items into numeric vars
  scale_items_num <- paste0(scale_items, "num")
  for (i in seq_along(scale_items)) {
    data <- eval(parse(text = paste0("update(data,", scale_items_num[i], " = as.numeric(unlist(data[,scale_items[i]]$variables)))")))
  }

  # Reverse reverse-coded items
  if (!is.null(reversed)) {
    reversed_num <- paste0(reversed, "num")
    scale_items_num <- c(setdiff(scale_items_num, reversed_num), paste0(
      reversed_num,
      "r"
    ))
    for (i in seq_along(reversed)) {
      data <- eval(parse(text = paste0("update(data,", reversed_num[i], "r = psych::reverse.code(-1, data[,reversed_num[i]]$variables))")))
    }
  }

  # Create scale
  data <- eval(parse(text = paste0("update(data,", scale_name, " = rowMeans(data[,scale_items_num]$variables, na.rm=T))")))

  # Reverse full scale
  if (!is.null(r_key)) {
    if (r_key == -1) {
      data <- eval(parse(text = paste0(
        "update(data,", scale_name, " = psych::reverse.code(",
        r_key, ", data$variables$", scale_name, "))"
      )))
    } else if (r_key > 0) {
      data <- eval(parse(text = paste0(
        "update(data,", scale_name, " = psych::reverse.code(",
        -1, ", data$variables$", scale_name, ", maxi = ", r_key, "))"
      )))
    }
  }
  

  if (print_desc) {
    if (!is.null(reversed)) {
      reversed_min <- numeric()
      reversed_max <- numeric()
      for (i in seq_along(reversed)) {
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
    
    if (length(reversed) > 0) {
      print(glue::glue('

The following items were reverse coded (with min and max values): \\
                       {paste0("\n", reversed, " (", reversed_min, ", ", reversed_max, ")", collapse = "")}'))
    }
  }

  # Print histograms of items and scale
  if (print_hist) {
    hist_vars <- c(scale_name, paste0(scale_items, "num"))
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
