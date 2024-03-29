% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{clip_excel}
\alias{clip_excel}
\title{Copy data to clipboard to paste into Excel}
\source{
https://www.r-bloggers.com/copying-data-from-excel-to-r-and-back/
}
\usage{
clip_excel(data, row_names = FALSE, col_names = TRUE, ...)
}
\arguments{
\item{data}{Dataframe to be copied.}

\item{row_names}{Logical. Should row names be copied?}

\item{col_names}{Logical. Should column names be copied?}

\item{...}{Further arguments passed to \code{write.table}}
}
\description{
This function copies a dataframe into the clipboard, so that it can be
pasted into excel.
}
