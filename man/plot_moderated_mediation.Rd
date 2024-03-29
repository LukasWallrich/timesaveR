% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/med_mod_plots.R
\name{plot_moderated_mediation}
\alias{plot_moderated_mediation}
\title{Plot a moderated mediation model}
\usage{
plot_moderated_mediation(
  X,
  M,
  W,
  Y,
  CV = NULL,
  mod_direct_path = TRUE,
  labels = list(a = "+", b = "+", c = "+", a_mod = "+", c_mod = "+"),
  filename = NULL
)
}
\arguments{
\item{X}{The name of the predictor variable}

\item{M}{The name of the mediator variable}

\item{W}{The name of the moderator variable}

\item{Y}{The name of the outcome variable}

\item{CV}{The name of covariates}

\item{mod_direct_path}{Logical. Should a direct path from X to Y be included?}

\item{labels}{A list of labels for the paths. See the example for the required names.}

\item{filename}{The filename to save the plot to (should end in .svg). When NULL,
the graph is not saved but simply returned.}
}
\value{
A named list with the code and the graph
}
\description{
This function helps to plot a moderated mediation model - with a single moderator
and a single mediator. Labels for each path need to be specified manually. By default,
covariates are only related to the outcome variable - if they are also used to estimate
the mediator, the graph code needs to be adjusted manually.
}
\examples{
plot <- plot_moderated_mediation(X = "Training", M = "Self-Efficacy", 
  W = "Motivation", Y = "Performance", CV = "Age, Gender", 
  mod_direct_path = TRUE, labels = list(a = "+", b = "+", c = "+", 
                                        a_mod = "+", c_mod = "+"), 
                                        filename = NULL)

# Show the graph
plot$graph

# Show the code 
plot$code

# To create the graph again (e.g., after you edit its code)
DiagrammeR::grViz(plot$code)

}
