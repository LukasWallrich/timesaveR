#' Create mice predictorMatrix including unordered factors (extends mice::quickpred)
#'
#' \code{\link[mice]{quickpred}} creates a predictor matrix that suggests which variables should
#' be used for multiple imputation. However, it does not yield meaningful information
#' for (unordered) factors. This function returns the same as mice::quickpred for
#' numeric and logical variables and binary factors, but excludes character
#' vectors and tests the predictive power of each level of factors with more
#' than two levels separately (through dummy coding). If the correlation exceeds `mincor` for at least one
#' level, the factor variable is used as a predictor.
#'
#' @param data Matrix or data frame with incomplete data.
#' @param mincor A scalar, numeric vector (of size \code{ncol(data))} or numeric
#' matrix (square, of size \code{ncol(data)} specifying the minimum
#' threshold(s) against which the absolute correlation in the data is compared.
#' @param minpuc A scalar, vector (of size \code{ncol(data))} or matrix (square,
#' of size \code{ncol(data)} specifying the minimum threshold(s) for the
#' proportion of usable cases.
#' @param include A string or a vector of strings containing one or more
#' variable names from \code{names(data)}. Variables specified are always
#' included as a predictor.
#' @param exclude A string or a vector of strings containing one or more
#' variable names from \code{names(data)}. Variables specified are always
#' excluded as a predictor.
#' @param method A string specifying the type of correlation. Use
#' \code{'pearson'} (default), \code{'kendall'} or \code{'spearman'}. Can be
#' abbreviated.
#' @return A square binary matrix of size \code{ncol(data)}.
#' @source This function is based on the mice::quickpred function as available in mice v 3.11.4, written by Stef van Buuren. The code for dummy coding factor variables is based on psych::dummy.code, written by William Revelle
#' @seealso \code{\link[mice]{quickpred}}
#' @export

quickpred_ext <- function(data, mincor = 0.1, minpuc = 0, include = "", exclude = "",
                          method = "pearson") {
  data <- mice_check.dataform(data)
  nvar <- ncol(data)

  # Identify characters and factors
  chr <- names(data)[sapply(data, is.character)]
  chr_which <- NULL

  if (length(chr) > 0) {
    message(paste0(
      "Data contains character variable(s): ", paste(chr, collapse = " "),
      ". These will not be used as predictors."
    ))
    chr_which <- which(names(data) %in% chr)
  }

  fct <- names(data)[sapply(data, nlevels) > 2]
  fct_which <- NULL

  if (length(fct) > 0) {
    message(paste0(
      "Data contains factor variable(s) with more than 2 levels: ", paste(fct, collapse = " "),
      ". mincor argument will be tested for each level."
    ))
    fct_which <- which(names(data) %in% fct)
  }

  # initialize
  predictorMatrix <- matrix(0, nrow = nvar, ncol = nvar, dimnames = list(
    names(data),
    names(data)
  ))
  x <- data.matrix(data)
  r <- !is.na(x)

  # include predictors with
  # 1) pairwise correlation among data
  ## Without factors
  if (length(fct) == 0) {
    suppressWarnings(v <- abs(stats::cor(x,
      use = "pairwise.complete.obs",
      method = method
    )))
    v[is.na(v)] <- 0
  } else {
    ## Correction for factors
    x_dummies <- x

    fct_start <- numeric()
    fct_end <- numeric()

    for (i in seq_along(fct_which)) {
      fct_start[i] <- ncol(x_dummies) + 1
      d <- dummy_code(x[, fct_which[i]])
      x_dummies <- cbind(x_dummies, d)
      fct_end[i] <- ncol(x_dummies)
    }
    suppressWarnings(v <- abs(stats::cor(x_dummies,
      use = "pairwise.complete.obs",
      method = method
    )))

    for (i in seq_along(fct_which)) {
      v[, fct_which[i]] <- do.call(pmax, c(data.frame(v[, fct_start[i]:fct_end[i]]), na.rm = TRUE))
    }
    v <- v[1:ncol(x), 1:ncol(x)]
    v[is.na(v)] <- 0
  }
  # 2) pairwise correlation of data with response indicator higher than mincor
  suppressWarnings(u <- abs(stats::cor(
    y = x, x = r, use = "pairwise.complete.obs",
    method = method
  )))
  u[is.na(u)] <- 0
  maxc <- pmax(v, u)
  predictorMatrix[maxc > mincor] <- 1

  # exclude predictors with a percentage usable cases below minpuc
  p <- mice::md.pairs(data)
  puc <- p$mr / (p$mr + p$mm)
  predictorMatrix[puc < minpuc] <- 0

  # exclude character vars
  predictorMatrix[, chr_which] <- 0

  # exclude predictors listed in the exclude argument
  yz <- pmatch(exclude, names(data))
  predictorMatrix[, yz] <- 0

  # include predictors listed in the include argument
  yz <- pmatch(include, names(data))
  predictorMatrix[, yz] <- 1

  # some final processing
  diag(predictorMatrix) <- 0
  predictorMatrix[colSums(!r) == 0, ] <- 0

  return(predictorMatrix)
}

#' Dummy code variable
#'
#' Simplified from psych::dummy.code and
#' changed to return data.frame
#'
#' @param x A vector to be transformed into dummy codes.
#' @source Simplified from psych::dummy.code, written by William Revelle


dummy_code <- function(x) {
  t <- table(x)
  lt <- length(t)
  n.obs <- length(x)
  new <- matrix(0, nrow = n.obs, ncol = lt)
  new[is.na(x), ] <- NA
  xlev <- factor(x, levels = names(t))

  for (i in 1:n.obs) {
    new[i, xlev[i]] <- 1
  }

  as.data.frame(new)
}


for (i in seq_along(names(df))) {
  n_rows <- nrow(df)
  perc_missing <- 15 # percentage missing data
  row_missing <- sample(1:n_rows, sample(1:n_rows, round(perc_missing / 100 * n_rows, 0))) # sample randomly x% of rows
  col_missing <- i # define column
  df[row_missing, col_missing] <- NA # assign missing values
}


mice_check.dataform <- function (data)
{
  ## Source: mice v 3.11.4, written by Stef van Buuren.
    if (!(is.matrix(data) || is.data.frame(data)))
    stop("Data should be a matrix or data frame", call. = FALSE)
  if (ncol(data) < 2)
    stop("Data should contain at least two columns",
         call. = FALSE)
  data <- as.data.frame(data)
  mat <- sapply(data, is.matrix)
  df <- sapply(data, is.data.frame)
  if (any(mat))
    stop("Cannot handle columns with class matrix: ",
         colnames(data)[mat])
  if (any(df))
    stop("Cannot handle columns with class data.frame: ",
         colnames(data)[df])
  dup <- duplicated(colnames(data))
  if (any(dup))
    stop("Duplicate names found: ", paste(colnames(data)[dup],
                                          collapse = ", "))
  data
}
