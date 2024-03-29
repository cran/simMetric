% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modSE.R
\name{modSE}
\alias{modSE}
\title{Calculates the average model standard error}
\usage{
modSE(se, get = c("modSE", "modSE_mcse"), na.rm = FALSE, ...)
}
\arguments{
\item{se}{A numeric vector containing the standard errors from the model(s).}

\item{get}{A character vector containing the values returned by the function.}

\item{na.rm}{A logical value indicating whether NA values for \code{se} should be removed before modSE calculation.}

\item{...}{Additional arguments to be ignored.}
}
\value{
A named vector containing the estimate and the Monte Carlo standard error for the average model standard error.
}
\description{
Calculates the average model standard error and the Monte Carlo standard error of this estimate.
}
\examples{
modSE(se=runif(n=20, min=1, max=1.5))
}
