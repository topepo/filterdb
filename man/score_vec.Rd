% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score-vec.R
\name{score_vec}
\alias{score_vec}
\title{Create a filter score object}
\usage{
score_vec(x = numeric(), direction = "maximize", impute = Inf)
}
\arguments{
\item{x}{A numeric vector..}

\item{direction}{What is the best result? Possible values are "maximize",
"minimize", or "zero".}

\item{impute}{A single numeric value for when values are missing. This should
be a value that indicates that a variable \emph{should not} be removed.}
}
\description{
\code{score_vec()} creates a \code{score_vec} object from a numeric vector. The
direction of best result (e.g. maximization) can be set along with a numeric
value to use when the value cannot be computed.
}
\details{
direction values are those that you feel unsure about, and would like to
exclude from performance calculations or other metrics.
}
\examples{
set.seed(1)
score_vec(rnorm(10), "zero")
}
