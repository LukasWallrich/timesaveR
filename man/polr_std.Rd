% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_tests.R
\name{polr_std}
\alias{polr_std}
\title{polr() with standardised continuous variables}
\usage{
polr_std(formula, data = NULL, weights = NULL, ...)
}
\arguments{
\item{formula}{
    a formula expression as for regression models, of the form
    \code{response ~ predictors}. The response should be a factor
    (preferably an ordered factor), which will be interpreted as an
    ordinal response, with levels ordered as in the factor.  
    The model must have an intercept: attempts to remove one will
    lead to a warning and be ignored.  An offset may be used.  See the
    documentation of \code{\link{formula}} for other details.
  }

\item{data}{
    an optional data frame, list or environment in which to interpret
    the variables occurring in \code{formula}.
  }

\item{weights}{
    optional case weights in fitting.  Default to 1.
  }

\item{...}{
  Arguments passed on to \code{\link[MASS:polr]{MASS::polr}}
  \describe{
    \item{\code{start}}{
    initial values for the parameters.  This is in the format
    \code{c(coefficients, zeta)}: see the Values section.
  }
    \item{\code{na.action}}{
    a function to filter missing data.
  }
    \item{\code{contrasts}}{
    a list of contrasts to be used for some or all of
    the factors appearing as variables in the model formula.
  }
    \item{\code{model}}{
    logical for whether the model matrix should be returned.
  }
    \item{\code{method}}{
    logistic or probit or (complementary) log-log or cauchit
    (corresponding to a Cauchy latent variable). 
  }
  }}
}
\description{
This runs \code{\link[MASS:polr]{MASS::polr()}} after standardising all continuous predictors, while leaving
factors intact. Note that the Hessian (the observed information matrix)
is always returned, so that the \code{Hess} argument cannot be used.
}
\details{
In the model call, the weights variable will always be called \code{.weights}. This might
pose a problem when you update the model later on, for  the moment the only workaround
is to rename the weights variable accordingly (or to fix it and contribute a PR on
Github).
}
\examples{
polr_std(poverty ~ religion + age + gender, WVS)
}
\references{
See (Fox, 2015) for an argument why dummy variables should never
be standardised.
}
