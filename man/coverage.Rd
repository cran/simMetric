% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coverage.R
\name{coverage}
\alias{coverage}
\title{Calculate the coverage}
\usage{
coverage(
  true_value,
  ll,
  ul,
  get = c("coverage", "coverage_mcse"),
  na.rm = FALSE,
  ...
)
}
\arguments{
\item{true_value}{The true value which should be covered by the interval.}

\item{ll}{A numeric vector containing the lower limits of the confidence intervals.}

\item{ul}{A numeric vector containing the upper limits of the confidence intervals.}

\item{get}{A character vector containing the values returned by the function.}

\item{na.rm}{A logical value indicating whether NA values for ll and ul should be removed before coverage estimation.}

\item{...}{Additional arguments to be ignored.}
}
\value{
A named vector containing the estimate and the Monte Carlo standard error for the coverage.
}
\description{
Estimate the coverage and the Monte Carlo standard error of this estimate given a vector of confidence intervals and the true value.
}
\examples{
coverage(true_value=0, ll=c(-1, -1, -1, -1), ul=c(1, 1, 1, -0.5))
}
