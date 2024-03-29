% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{scale_blank}
\alias{scale_blank}
\title{Scales a vector and returns it without attributes}
\usage{
scale_blank(x, center = TRUE, scale = TRUE)
}
\arguments{
\item{x}{a numeric matrix(like object).}

\item{center}{either a logical value or numeric-alike vector of length
    equal to the number of columns of \code{x}, where
    \sQuote{numeric-alike} means that \code{\link[base]{as.numeric}(.)} will
    be applied successfully if \code{\link[base]{is.numeric}(.)} is not true.}

\item{scale}{either a logical value or a numeric-alike vector of length
    equal to the number of columns of \code{x}.}
}
\description{
The \code{base::scale()} function adds attributes to the output that can lead to
problems later on. This function scales a vector and strips the attributes.
}
\keyword{internal}
