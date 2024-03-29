% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcor.R
\name{pcor_matrix}
\alias{pcor_matrix}
\title{Calculates a partial correlation matrix controlling for one or more variables}
\usage{
pcor_matrix(data, given, ...)
}
\arguments{
\item{data}{Dataframe. Only numeric variables are included into correlation matrix.}

\item{given}{A character vector with one or multiple variables in data. It/they will be parceled out from all other variables in data,
before the correlation table is calculated.}

\item{...}{
  Arguments passed on to \code{\link[=cor_matrix]{cor_matrix}}
  \describe{
    \item{\code{var_names}}{A named character vector with new variable names or a tibble as provided by \code{\link[=get_rename_tribbles]{get_rename_tribbles()}}
for variables. If NULL, then the variables are not renamed. If names are provided, only the variables included here are retained.
This is most helpful when the results are passed to some print function, such as \code{\link[=report_cor_table]{report_cor_table()}}}
    \item{\code{conf_level}}{Confidence level to use for confidence intervals, defaults to .95}
    \item{\code{bootstrap}}{When using FIML estimation (with missing = "fiml"), significance tests and confidence
intervals can be bootstrapped. If you want to do that, pass the number of desired bootstrap resamples
(e.g., 5000) to this parameter, but beware that this can take a while.}
    \item{\code{seed}}{Pass an integer to set the seed for bootstrapping and thus make this reproducible}
    \item{\code{method}}{method="pearson" is the default value.  The alternatives to be passed to cor are "spearman" and "kendall".  These last two are much slower, particularly for big data sets. }
    \item{\code{adjust}}{What adjustment for multiple tests should be used? ("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"). See \code{\link{p.adjust}} for details about why to use "holm" rather than "bonferroni"). }
  }}
}
\description{
This returns a matrix aligned with the \code{cor_matrix()} function after parceling
out the effect of one or more other variables. This function requires complete data;
incomplete cases are dropped with a warning.
}
\examples{

# One might want to estimate correlations between health and possible predictors in the ESS
# after parceling out / controling for key demographic attributes:
pcor_matrix(ess_health, given = c("agea", "gndr"), 
   var_names = c("health" = "Health", "weight" = "Weight", "dosprt" = "Sport")) \%>\% 
   tidy(both_directions = FALSE)

}
