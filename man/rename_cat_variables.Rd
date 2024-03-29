% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{rename_cat_variables}
\alias{rename_cat_variables}
\title{Rename variables and/or their levels}
\usage{
rename_cat_variables(data, var_names = NULL, level_names = NULL)
}
\arguments{
\item{data}{A dataframe or list of dataframes (e.g., from multiple imputation) contains the variables. If a list is passed, it must have class "list"}

\item{var_names}{A tibble containing \code{old} and \code{new} names for the variables. If NULL, only levels are renamed.}

\item{level_names}{A tibble containing old \code{var} names and \code{level_old} and \code{level_new} names. If NULL, only variables are renamed.}
}
\value{
The dataframe or list of dataframes passed to data, with variables and/or levels renamed.
Any variables where levels are renamed will be converted to factors.
}
\description{
Renaming categorical variables and their levels, for instance for summary tables, can be fiddly. This
function accepts tibbles containing the old and new names for arguments and levels, and returns a dataframe
(or list of dataframes, if one is passed) with variables and levels renamed.
}
