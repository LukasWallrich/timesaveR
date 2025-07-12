.coef_offset_3 <- tibble::tribble(
  ~obj, ~h_off, ~v_off,
  # Top 1
  "M1_a", +1, -0.3,
  "M1_b", -1.2, -0.3,
  # Top 2
  "M3_a", 0.5, -0.2,
  "M3_b", -0.5, -0.2,
  # Bottom 1
  "M2_a", 0.4, -0.15,
  "M2_b", -0.7, -0.15,
)


#' Plot mediation model with one or more mediators
#'
#' Returns graphViz code and graph for (multiple) mediation model.
#' Coefficients and significance values for paths need to be provided, 
#' based on the format returned by `run_moderation()`
#'
#' @encoding UTF-8
#' @param X Character. Name of predictor
#' @param Y Character. Name of dependent variable
#' @param Ms Character vector. Names of mediator variables
#' @param data Dataframe with coefficients and significance values. See details.
#' @param coef_offset Tibble with values to position mediators. If not
#' provided, the function will align mediators automatically, which is unlikely to
#' provide a well-aligned chart (except for cases when the offset has been explicitly 
#' implemented for that number of mediators, currently 1 and 3). However, the returned code can still
#' be edited. See timesaveR:::.coef_offset_3 for an example of an offset tibble.
#' @param digits Number of digits for rounding
#' @param filename If provided, graph will be saved as .svg file.
#' @param ind_p_values Should significance stars be shown for indirect effects,
#' based on pvalues passed in data? If FALSE, indirect effects with confidence
#' intervals that do not include zero are bolded
#' @param IV `r lifecycle::badge("deprecated")` Use the `X` argument instead
#' @param DV `r lifecycle::badge("deprecated")` Use the `Y` argument instead
#' @return A list of a the graph and the associated code.
#' @export
#' @examples
#' \dontrun{
#' # Estimate mediation model (usually with bootstraps > 1000!)
#'
#' res <- run_mediation(ess_health, fltdpr, health, dosprt, agea, bootstraps = 50)
#' 
#' # Run plot command
#' plot <- plot_mediation(
#'   X = "Frequency of  <br /> feeling depressed",
#'   Y = "Self-reported <br /> poor health", Ms = "Frequency of <br /> physical activity", 
#'   data = res
#' )
#' 
#' # Show the graph
#' plot$graph
#' 
#' # Show the code 
#' plot$code
#' 
#' # To create the graph again (e.g., after you edit its code)
#' DiagrammeR::grViz(plot$code)
#' }
plot_mediation <- function(X, Y, Ms, data, digits = 2, coef_offset = length(Ms), filename = NULL, ind_p_values = FALSE, IV = deprecated(), DV = deprecated()) {
  
  if (lifecycle::is_present(DV)) {
    lifecycle::deprecate_warn("0.0.3", "make_scale(DV)", "make_scale(Y)")
    Y <- DV
  }
  
  if (lifecycle::is_present(IV)) {
    lifecycle::deprecate_warn("0.0.3", "make_scale(IV)", "make_scale(X)")
    X <- IV
  }
  
  .check_req_packages(c("glue", "DiagrammeR"))

  checkmate::assert_numeric(data$est, any.missing = FALSE)

  stylec <- ifelse(data$pvalue[data$type == "direct"] < .05, "solid", "dashed")

  determine_positions <- function(num_Ms) {
    pos <- tibble::tribble(
      ~obj, ~h, ~v,
      NA_character_, NA_real_, NA_real_
    ) %>% .[-1, ]

    for (i in 1:ceiling(num_Ms / 2)) {
      pos %<>% dplyr::bind_rows(tibble::tibble(obj = paste0("M", 2 * i - 1), v = i, h = 2.5))
      pos %<>% dplyr::bind_rows(tibble::tibble(obj = paste0("M", 2 * i - 1, "_a"), h = 0.6, v = 0.6 + i - 1))
      pos %<>% dplyr::bind_rows(tibble::tibble(obj = paste0("M", 2 * i - 1, "_b"), h = 4.6, v = 0.6 + i - 1))

      pos %<>% dplyr::bind_rows(tibble::tibble(obj = paste0("M", 2 * i), v = -i, h = 2.5))
      pos %<>% dplyr::bind_rows(tibble::tibble(obj = paste0("M", 2 * i, "_a"), h = 0.6, v = -0.6 - i + 1))
      pos %<>% dplyr::bind_rows(tibble::tibble(obj = paste0("M", 2 * i, "_b"), h = 4.6, v = -0.6 - i + 1))
    }
    pos
  }

  pos <- determine_positions(length(Ms))
  pos %<>% dplyr::bind_rows(tibble::tibble(obj = "note", h = 0.5, v = -0.1))

  data$type[!is.na(data$mediator) & !data$type == "indirect"] <- paste0("M", seq_along(Ms), "_", data$type[!is.na(data$mediator) & !data$type == "indirect"])
  data$type[data$type == "indirect"] <- paste0("M", seq_along(Ms))

  if (ind_p_values == TRUE) {
    pos <- data %>%
      dplyr::mutate(ci = fmt_ci(.data$ci.lower, .data$ci.upper, digits), est = paste0(sprintf(paste0("%.", digits, "f"), .data$est), sigstars(.data$pvalue))) %>%
      dplyr::select(obj = "type", "est", "ci") %>%
      dplyr::full_join(pos, by = "obj")
  } else {
    pos <- data %>%
      dplyr::mutate(
        ci = fmt_ci(.data$ci.lower, .data$ci.upper, digits),
        est = ifelse(stringr::str_detect(.data$type, "^M[0-9]$"),
          ifelse(sign(.data$ci.lower) == sign(.data$ci.upper),
            paste0("<b>", sprintf(paste0("%.", digits, "f"), .data$est), "</b> "),
            sprintf(paste0("%.", digits, "f"), .data$est)
          ),
          paste0(sprintf(paste0("%.", digits, "f"), .data$est), sigstars(.data$pvalue))
        )
      ) %>%
      dplyr::select(obj = "type", "est", "ci") %>%
      dplyr::full_join(pos, by = "obj")
  }
  pos$est[pos$obj == "note"] <- paste("<i>Direct effect:</i> ", pos$est[pos$obj == "direct"], pos$ci[pos$obj == "direct"], "<br />", "<i>Total effect: </i>", pos$est[pos$obj == "total"], pos$ci[pos$obj == "total"])

  pos %<>% .[!is.na(pos$est), ]
  pos %<>% .[!is.na(pos$h), ]


  if (!is.null(coef_offset)) {
    if (!(coef_offset %in% c(1, 3) || (tibble::is_tibble(coef_offset) && nrow(coef_offset) == length(Ms) * 2))) {
      cli::cli_warn("Valid coef_offset tibble is not provided and automatic alignment of coefficients is not yet implemented for this number of mediators - you will likely need to either provide a valid coef_offset tibble or edit the returned grViz code manually")
      coef_offset <- NULL
    } else if (coef_offset == 3) {
      coef_offset <- .coef_offset_3
    } else if (coef_offset == 1) {
      coef_offset <- NULL
    }
  }


  if (!is.null(coef_offset)) {
    for (i in seq_len(nrow(coef_offset))) {
      pos$h[pos$obj == coef_offset[i, ]$obj] <- pos$h[pos$obj == coef_offset[i, ]$obj] + coef_offset[i, ]$h_off
      pos$v[pos$obj == coef_offset[i, ]$obj] <- pos$v[pos$obj == coef_offset[i, ]$obj] + coef_offset[i, ]$v_off
    }
  }

  code <- glue::glue("digraph  {{

            graph [layout = 'neato',
            outputorder = 'edgesfirst',
            bgcolor = 'white', rankdir=LR,]

            node [fontname = 'Helvetica',
            fontsize = '10',
            shape = 'circle',
            fixedsize = 'true',
            width = '0.5',
            style = 'filled',
            fillcolor = 'white',
            color = 'black',
            fontcolor = 'black']



            'x' [label = <{X}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '0,0!']
            'y' [label = <{Y}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '5,0!']
                 {purrr::map2(Ms, paste0('M', 1:length(Ms)), function(x,y) paste0('\\'', y, '\\' [label = <', x, '<br />', pos$est[pos$obj == y], ' ', pos$ci[pos$obj == y], '>,
                 color = \\'black\\', shape = \\'rectangle\\', height = \\'0.5\\', width = \\'1.5\\',
                 pos = \\'', pos$h[pos$obj == y], ',',  pos$v[pos$obj == y], '!\\']')) %>% glue::glue_collapse(sep = '\\n')}

                 {pos[1:(nrow(pos)-1),] %>% purrr::pmap_chr(function(...) {
                 current <- tibble::tibble(...)

                 if(nchar(current$obj)>2) {
                 paste0('\\'', current$obj, '\\' [label = <', current$est, '<br />', current$ci, '>,
                 color = \\'black\\', shape = \\'plaintext\\', fillcolor=\\'transparent\\',
                 pos = \\'', current$h, ',',  current$v, '!\\']')
                 } else {''}
                 }) %>% glue::glue_collapse(sep = '\\n')}

            edge [fontname = 'Helvetica',
            fontsize = '10',
            len = '1.5',
            color = 'black',
            arrowsize = '0.5']

            {paste0('x->', paste0('M', 1:length(Ms)), collapse = '\\n')}
            {paste0(paste0('M', 1:length(Ms)), '->y', collapse = '\\n')}

            x->y  [style = {stylec}, headlabel= <{pos$est[pos$obj == 'note']}> labeldistance=13, labelangle=7]

             }}")



  if (!is.null(filename)) {
    graph <- .grViz_and_save(code, filename = filename)
  } else {
    graph <- code %>% DiagrammeR::grViz()
  }
  if (interactive()) print(graph)
  named_list(code, graph)
}


.unescape_html <- function(str) {
  purrr::map_chr(str, function(x) xml2::xml_text(xml2::read_html(paste0("<x>", x, "</x>"))))
}


#' Plot a moderated mediation model
#' 
#' This function helps to plot a moderated mediation model - with a single moderator
#' and a single mediator. Labels for each path need to be specified manually. By default,
#' covariates are only related to the outcome variable - if they are also used to estimate
#' the mediator, the graph code needs to be adjusted manually.
#'
#' @param X The name of the predictor variable
#' @param M The name of the mediator variable
#' @param W The name of the moderator variable
#' @param Y The name of the outcome variable
#' @param CV The name of covariates
#' @param mod_direct_path Logical. Should a direct path from X to Y be included?
#' @param labels A list of labels for the paths. See the example for the required names.
#' @param filename The filename to save the plot to (should end in .svg). When NULL, 
#' the graph is not saved but simply returned.
#' @return A named list with the code and the graph
#' @examples
#' plot <- plot_moderated_mediation(X = "Training", M = "Self-Efficacy", 
#'   W = "Motivation", Y = "Performance", CV = "Age, Gender", 
#'   mod_direct_path = TRUE, labels = list(a = "+", b = "+", c = "+", 
#'                                         a_mod = "+", c_mod = "+"), 
#'                                         filename = NULL)
#' 
#' # Show the graph
#' plot$graph
#' 
#' # Show the code 
#' plot$code
#' 
#' # To create the graph again (e.g., after you edit its code)
#' DiagrammeR::grViz(plot$code)
#' 
#' @export


plot_moderated_mediation <- function(X, M, W, Y, CV = NULL, mod_direct_path = TRUE, labels = list(a = "+", b = "+", c = "+", a_mod = "+", c_mod = "+"), filename = NULL) {
  .check_req_packages(c("glue", "DiagrammeR"))

  all_text <- paste(X, M, W, Y, CV)
  escapes <- stringr::str_extract_all(all_text, "&.*?;")[[1]] %>% unique()
  targets <- .unescape_html(escapes)

  # Set parameters

  a <- labels$a
  b <- labels$b
  c <- labels$c
  a_mod <- labels$a_mod
  c_mod <- labels$c_mod

  code <- glue::glue("digraph {{

        graph [layout = 'neato',
        outputorder = 'edgesfirst',
        bgcolor = 'white', rankdir=LR,]

        node [fontname = 'Helvetica',
        fontsize = '10',
        shape = 'circle',
        fixedsize = 'true',
        width = '0.5',
        style = 'filled',
        fillcolor = 'white',
        color = 'black',
        fontcolor = 'black']

        'X' [label = <{X}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '0,0!']
        'Y' [label = <{Y}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '5,0!']
        'M' [label = <{M}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '2.5,1!']
        'a' [label = <{a}>  color = 'black', fillcolor='transparent', shape = 'plaintext', pos = '1.0,0.6!']
        'amod' [label = <{a_mod}>  color = 'black', fillcolor='transparent', shape = 'plaintext', pos = '1.45,-0.25!']
        'b' [label = <{b}>, color = 'black', fillcolor='transparent', shape = 'plaintext', pos = '3.8,0.65!']
        'c' [label = <{c}>, color = 'black', fillcolor='transparent', shape = 'plaintext', pos = '2.45,-0.15!']
        'W' [label = <{W}>, color = 'black', shape = 'rectangle', height = '0.5', width = '1.5', pos = '1.8,-0.7!']
        'MW' [style = invis, pos = '1.3,0.5!', height = '0', width = '0']
        {if(mod_direct_path) '\\'XW\\' [style = invis, pos = \\'1.8,0!\\', height = \\'0\\', width = \\'0\\']'}
        {if(mod_direct_path) glue::glue('\\'cmod\\' [label = <{c_mod}>, color = \\'black\\', shape = \\'plaintext\\', fillcolor=\\'transparent\\', pos = \\'1.9,-0.25!\\']')}

        {if(!is.null(CV)) glue::glue('\\'CV\\' [label = <{CV}>, color = \\'black\\', shape = \\'rectangle\\', height = \\'{0.4+stringr::str_count(CV, \\'BR\\')*.1}\\', width = \\'1.5\\', pos = \\'5,{-0.7-stringr::str_count(CV, \\'BR\\')*.05}!\\']')}

        edge [fontname = 'Helvetica',
        fontsize = '10',
        len = '1.5',
        color = 'black',
        arrowsize = '0.5']

        X->MW  [arrowhead=none]
        MW -> M
        M->Y
        W->MW
        {ifelse(mod_direct_path, '    X->XW [arrowhead=none] \n    XW -> Y \n    W->XW', '    X->Y')}

        {if(!is.null(CV)) 'CV->Y  [style=dashed]'}

        }}")

  graph <- code %>% DiagrammeR::grViz()

  if (!is.null(filename)) {
    graph <- .grViz_and_save(code, filename = filename)
  } else {
    graph <- code %>% DiagrammeR::grViz()
  }

  named_list(code, graph)
}


# Save graph as an svg file, while dealing wiht special characters
.grViz_and_save <- function(code, filename) {
  graph <- DiagrammeR::grViz(code)
  if (suppressWarnings(!all(lapply(c("DiagrammeRsvg"), requireNamespace, quietly = TRUE)))) {
    cli::cli_warn("To save the diagramme, you need the {.pkg DiagrammeRsvg} package. File not saved.")
    return(graph)
  } else {
    ext <- stringr::str_sub(filename, -3)
    if (!(ext == "svg")) {
      cli::cli_warn("File extension should be svg. Adding {.file .svg} added to filename provided.")
      filename <- paste0(filename, ".svg")
    }

    escapes <- stringr::str_extract_all(code, "&.*?;")[[1]] %>% unique()
    targets <- .unescape_html(escapes)
    escapes <- stringi::stri_escape_unicode(targets) %>%
      stringr::str_replace("\\\\u", "&#x")

    if (length(escapes) > 0) escapes %<>% paste0(";")

    graph %>%
      DiagrammeRsvg::export_svg() %>%
      stringr::str_replace_all('fill="transparent"', 'fill-opacity="0.0"') %>%
      ifelse(length(escapes) > 0, stringr::str_replace_all(., escapes %>% magrittr::set_names(targets)), .) %>%
      writeLines(filename)
  }
  graph
}
