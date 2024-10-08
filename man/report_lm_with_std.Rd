% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_tables.R
\name{report_lm_with_std}
\alias{report_lm_with_std}
\title{Create a summary table comparing standardized and non-standardized linear models}
\usage{
report_lm_with_std(
  mod,
  mod_std,
  conf_level = 0.95,
  coef_renames = NULL,
  fmt = "\%.2f",
  statistic_vertical = FALSE,
  filename = NULL,
  model_names = NULL,
  show_nimp = FALSE,
  R2_change = FALSE,
  notes = list(NULL),
  apa_style = TRUE,
  stars = std_stars_pad,
  ...
)
}
\arguments{
\item{mod}{A lm-model/mira object of lm models, with variables not standardized (or a list of such models)}

\item{mod_std}{A lm-model/mira object of lm models, with standardized variables. Can be
created with \code{\link[=lm_std]{lm_std()}} (or a list of such models)}

\item{conf_level}{Confidence level to use for confidence intervals, defaults to .95}

\item{coef_renames}{A named character vector with new names for the coefficients or a tibble as provided by \code{\link[=get_coef_rename_tribble]{get_coef_rename_tribble()}}
for variables. If NULL, then the coefficients are not renamed.}

\item{fmt}{how to format numeric values: integer, user-supplied function, or \code{modelsummary} function.
\itemize{
\item Integer: Number of decimal digits
\item User-supplied functions:
\itemize{
\item Any function which accepts a numeric vector and returns a character vector of the same length.
}
\item \code{modelsummary} functions:
\itemize{
\item \code{fmt = fmt_significant(2)}: Two significant digits (at the term-level)
\item \code{fmt = fmt_decimal(digits = 2, pdigits = 3)}: Decimal digits for estimate and p values
\item \code{fmt = fmt_sprintf("\%.3f")}: See \code{?sprintf}
\item \code{fmt = fmt_term("(Intercept)" = 1, "X" = 2)}: Format terms differently
\item \code{fmt = fmt_statistic("estimate" = 1, "r.squared" = 6)}: Format statistics differently.
\item \code{fmt = fmt_identity()}: unformatted raw values
}
\item string:
\item Note on LaTeX output: To ensure proper typography, all numeric entries are enclosed in the \verb{\\num{}} command, which requires the \code{siunitx} package to be loaded in the LaTeX preamble. This behavior can be altered with global options. See the 'Details' section.
}}

\item{statistic_vertical}{Should standard errors and CIs be shown below coefficients? Defaults to horizontal layout}

\item{filename}{the file name to create a HTML file on disk.}

\item{model_names}{If several pairs of models are to be plotted side by side, indicate the label for each \emph{pair} here}

\item{show_nimp}{Logical - DEFUNCT. If mira objects are passed, this determines whether the number of imputations will be reported as a model statistic}

\item{R2_change}{Logical. Report R2 change and F-test to compare models.}

\item{notes}{List of notes to append to bottom of table. An explanation of significance stars is automatically added. If the std models were run with a helper function in this package, a note regarding the standardization is also automatically added.}

\item{apa_style}{Logical, should APA-style formatting be applied}

\item{stars}{Named vector of significance stars and their thresholds, check \code{timesaveR:::std_stars_pad} for default.}

\item{...}{
  Arguments passed on to \code{\link[modelsummary:modelsummary]{modelsummary::modelsummary}}
  \describe{
    \item{\code{coef_map}}{character vector. Subset, rename, and reorder coefficients.
Coefficients omitted from this vector are omitted from the table. The order
of the vector determines the order of the table. \code{coef_map} can be a named
or an unnamed character vector. If \code{coef_map} is a named vector, its values
define the labels that must appear in the table, and its names identify the
original term names stored in the model object: \code{c("hp:mpg"="HPxM/G")}. See
Examples section below.}
    \item{\code{coef_omit}}{integer vector or regular expression to identify which coefficients to omit (or keep) from the table. Positive integers determine which coefficients to omit. Negative integers determine which coefficients to keep. A regular expression can be used to omit coefficients, and perl-compatible "negative lookaheads" can be used to specify which coefficients to \emph{keep} in the table. Examples:
\itemize{
\item c(2, 3, 5): omits the second, third, and fifth coefficients.
\item c(-2, -3, -5): negative values keep the second, third, and fifth coefficients.
\item \code{"ei"}: omit coefficients matching the "ei" substring.
\item \code{"^Volume$"}: omit the "Volume" coefficient.
\item \code{"ei|rc"}: omit coefficients matching either the "ei" or the "rc" substrings.
\item \code{"^(?!Vol)"}: keep coefficients starting with "Vol" (inverse match using a negative lookahead).
\item \code{"^(?!.*ei)"}: keep coefficients matching the "ei" substring.
\item \code{"^(?!.*ei|.*pt)"}: keep coefficients matching either the "ei" or the "pt" substrings.
\item See the Examples section below for complete code.
}}
    \item{\code{coef_rename}}{logical, named or unnamed character vector, or function
\itemize{
\item Logical: TRUE renames variables based on the "label" attribute of each column. See the Example section below.
\item Unnamed character vector of length equal to the number of coefficients in the final table, after \code{coef_omit} is applied.
\item Named character vector: Values refer to the variable names that will appear in the table. Names refer to the original term names stored in the model object. Ex: c("hp:mpg"="hp X mpg")
\item Function: Accepts a character vector of the model's term names and returns a named vector like the one described above. The \code{modelsummary} package supplies a \code{coef_rename()} function which can do common cleaning tasks: \code{modelsummary(model, coef_rename = coef_rename)}
}}
  }}
}
\value{
A list with \code{gt_tab} (the gt-table object including the parts of the table
that can be created with gt. This can be post-processed and formatted with functions in
the gt-package, but does not include the lower part with model statistics, e.g., R^2.) and
\code{html_code} (the code that creates the full table, and is used to render it in
the Viewer).
}
\description{
This function creates a summary table for lm models (including mice::mira objects
containing lm-models) that shows a standardized and non-standardized version of the model
side-by-side. Several pairs of such models can be compared side-by-side.
}
\examples{

# Standard lm model
mod1 <- lm(mpg ~ hp + wt, mtcars)

# Model with standardized coefficients

mod2 <- lm_std(mpg ~ hp + wt, mtcars)

report_lm_with_std(mod1, mod2)
}
