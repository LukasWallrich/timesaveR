% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_scales.R
\name{make_scales}
\alias{make_scales}
\title{Create multiple scales by calculating item means and return descriptives}
\usage{
make_scales(
  data,
  items,
  reversed = FALSE,
  two_items_reliability = c("spearman_brown", "cronbachs_alpha", "r"),
  print_desc = FALSE,
  ...
)
}
\arguments{
\item{data}{A dataframe}

\item{items}{A named list of characters vectors. Names are the scale names,
each vector contains the items for that scale (variables in data)}

\item{reversed}{How should items be reverse-coded. Can be FALSE (no reverse-coding),
TRUE (automatic reverse-coding of items negatively correlated with remainder), or a
named list of characters vectors. Names are the scale names, each vector contains
the items to be reverse-coded for that scale. If TRUE, the result must be checked carefully!}

\item{two_items_reliability}{How should the reliability of two-item scales be
reported? "spearman_brown" is the recommended default, but "cronbachs_alpha"
and Pearson's "r" are also supported.}

\item{print_desc}{Logical. Should descriptives for scales be printed?}

\item{...}{
  Arguments passed on to \code{\link[=make_scale]{make_scale}}
  \describe{
    \item{\code{print_hist}}{Logical. Should histograms for items and resulting scale be printed?}
  }}
}
\value{
A list of two dataframes: scale values (\code{scores}) and
descriptive statistics for each scale (\code{descriptives})
}
\description{
This function creates multiple scales, returns descriptives and supports
reverse-coding of items.
}
