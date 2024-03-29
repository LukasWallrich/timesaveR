% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_tests.R
\name{svy_pairwise_t_test}
\alias{svy_pairwise_t_test}
\title{Pairwise t.tests with effect sizes and survey weights}
\usage{
svy_pairwise_t_test(data, dv, iv, cats, p.adjust = "holm", ...)
}
\arguments{
\item{data}{A survey object}

\item{dv}{Character. Name of the dependent variable for the t.test (numeric)}

\item{iv}{Character. Name of the grouping variable for the t.test (factor)}

\item{cats}{Character vector of factor levels to be included in the
pairwise tests. If set to NULL, all levels are used.}

\item{p.adjust}{Method to adjust p-values for multiple comparisons. One of
"holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr" or "none".}

\item{...}{
  Arguments passed on to \code{\link[=svy_cohen_d_pair]{svy_cohen_d_pair}}
  \describe{
    \item{\code{ttest}}{Logical. Should t.test be run? Otherwise, only
Cohen's d is calculated. Defaults to TRUE.}
    \item{\code{print}}{Logical. Should results be printed.}
  }}
}
\value{
A tibble with t-test results and Cohen's d for each pair
}
\description{
This function calculates a t.test() for any pair of levels in a
\code{srvyr} survey object. It does currently not do any p-value adjustment
for multiple comparisons, and print rather than returns the results.
}
