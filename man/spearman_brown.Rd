% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_scales.R
\name{spearman_brown}
\alias{spearman_brown}
\title{Calculate Spearman-Brown reliability for two-item scale}
\source{
https://www.r-bloggers.com/five-ways-to-calculate-internal-consistency/
}
\usage{
spearman_brown(data, items, name = "", SB_only = FALSE)
}
\arguments{
\item{data}{A dataframe}

\item{items}{Character vector of length 2, with names of the two items}

\item{name}{Name of the scale, relevant only if data.frame is returned}

\item{SB_only}{Logical, indicating whether to return only the reliability as
a number or a dataframe that also includes the scale name and correlation.}
}
\value{
Either the Spearman-Brown coefficient as a single number, or a
dataframe including the Pearson correlation coefficient and the scale name
}
\description{
This function calculates the Spearman-Brown reliability for a two-item scale,
which is the recommended measure for two-item scales (rather than Cronbach's
alpha, which is used for longer scales.)
}
