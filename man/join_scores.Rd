% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/join.R
\name{join_scores}
\alias{join_scores}
\title{Combine filter results}
\usage{
join_scores(..., .impute = FALSE)
}
\arguments{
\item{...}{A set of objects with class \code{filter_method}.}

\item{.impute}{A single logical to indicate whether missing data are converted
to a value that implies that a variable should be retained.}
}
\description{
Joins different results across variables.
}
