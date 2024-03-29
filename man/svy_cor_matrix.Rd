% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cor_tables.R
\name{svy_cor_matrix}
\alias{svy_cor_matrix}
\title{Create a correlation matrix from survey data with summary statistics}
\usage{
svy_cor_matrix(svy_data, var_names = NULL, return_n = FALSE)
}
\arguments{
\item{svy_data}{A survey object created with the survey or srvyr package. Only
numeric variables will be included in the result.}

\item{var_names}{A named character vector with new variable names or a tibble as provided by \code{\link[=get_rename_tribbles]{get_rename_tribbles()}}
for variables. If NULL, then the variables are not renamed. If names are provided, only the variables included here are retained.
This is most helpful when the results are passed to some print function, such as \code{\link[=report_cor_table]{report_cor_table()}}}

\item{return_n}{Should the sample size be returned? Note that this is \emph{not} survey-weighted, and should thus only be used when the weights add up to the number of observations.}
}
\value{
A correlation matrix list in the format provided by
\code{jtools::svycor()} with the addition of a \code{desc}-element with means
and standard deviations of the variables.
}
\description{
This function wraps jtools::svycor() so that it works in a srvyr-pipeline,
runs bootstrapped significance-tests and calculates weighted summary
statistics. Only numeric variables are included in the result.
}
\examples{
\dontrun{
if (requireNamespace("survey") & requireNamespace("srvyr")) {
  library(survey)
  library(srvyr)
  data(api)
  # Create survey design object
  dstrat <- apistrat \%>\% as_survey_design(1, strata = stype, fpc = fpc, weight = pw)

var_names <- c(meals = "Share subsidized meals", ell = "English language learners",
              growth = "Performance Change")

  # Print correlation matrix
  svy_cor_matrix(dstrat, var_names)
}
}

}
