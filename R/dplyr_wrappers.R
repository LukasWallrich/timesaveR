#' lm() for use with pipes (`\%>\%`) - data as first argument
#'
#' Within a dplyr-pipe, running lm() is often complicated be the placing of the
#' data argument. This wrapper places data first and allows to run standardized
#' models.
#'
#' Note that the model call in the lm-object is replaced by the call to this
#' function - that means that \code{update()} cannot be used.
#'
#' @param df Data for modelling
#' @param std Logical. Should variables be standardised? This is only applied to
#' numeric variables, factors are left unchanged so that their coefficients
#' remain interpretable.
#' @param rename_std Logical. Should standardised variables be indicated by _sd
#' suffix
#' @inheritParams stats::lm
#' @inheritDotParams stats::lm -data
#' @source After experiencing an issue with passing weights, I rewrote this
#' based on the code suggested by "Vandenman" here
#' https://stackoverflow.com/questions/38683076/ellipsis-trouble-passing-to-lm
#' @references See (Fox, 2015) for an argument why dummy variables should never
#' be standardised. If you want to run a model with all variables standardised,
#' one option is `QuantPsyc::lm.beta()`
#' 
#' @export

run_lm <- function(df, formula, std = FALSE, rename_std = FALSE, ...) {
  if (std) {
    vars <- all.vars(formula)
    vars_num <- vars[purrr::map_lgl(vars, .is.numeric_col, df = df)]

    if (rename_std) {
      df <- df %>% dplyr::mutate_at(vars_num, list(sd = scale_blank))


      repl <- paste0(vars_num, "_sd")
      names(repl) <- vars_num
      formula <- Reduce(paste, deparse(formula)) %>%
        stringr::str_replace_all(c(repl)) %>%
        as.formula()
    } else {
      df <- df %>% dplyr::mutate_at(vars_num, list(scale_blank))
    }
  }

  # get names of stuff in ...
  arg_names <- sapply(substitute(list(...))[-1L], deparse)
  # look for identical names in df
  m <- match(names(df), arg_names, 0L)

  # store other arguments from ... in a list, if any
  dot_args <- eval(parse(text = arg_names[-m]))
  if (is.null(dot_args)) {
    args <- list()
  } else {
    args <- list(dot_args)
    # name the list
    names(args) <- names(arg_names[-m])
  }

  # store complete values in args, instead of just references to columns
  # the unlist code is rather ugly, the goal is to create a list where every
  # element is a column of interest
  args[names(arg_names)[m]] <- unlist(apply(
    df[, as.logical(m), drop = FALSE],
    2, list
  ), recursive = FALSE)
  # also put other stuff in there
  args$formula <- formula
  args$data <- df
  # do lm
  mod <- do.call(lm, args)
  class(mod) <- c(class(mod), "rN_lm")
  if (std) {
    mod$call_fmt <- c(sys.call(), "Note: DV and continuous IVs were standardised")
    class(mod) <- c(class(mod), "rN_std")
  } else {
    mod$call_fmt <- c(sys.call())
  }
  mod
}

#' Summary of an lm object created with run_lm() wrapper
#'
#' Using \code{\link{run_lm}} creates a very unwieldy call to lm(). This
#' function replaces it by a more legible call in the `summary()`-output
#'
#' @param object A model with class `rN_lm`
#' @param ... Parameters passed down to summary and print
#' @inheritDotParams stats::summary.lm
#' @export

summary.rN_lm <- function(object, ...) {
  out <- stats::summary.lm(object, ...)
  out$call <- object$call_fmt
  out
}

#' Tests whether a column in dataframe, specified by string, is numeric
#'
#' @param col Character indicating column name
#' @param df Dataframe that contains `col`

.is.numeric_col <- function(col, df) {
  is.numeric(magrittr::extract2(df, col))
}
