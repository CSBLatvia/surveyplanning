\name{round2}
\alias{round2}

\title{Rounding numbers}

\description{
The function rounds the values in its first argument to the specified number of decimal places (default 0).

Usage
}

\usage{
round2(x, n)
}

\arguments{

\item{x}{a numeric vector.}

\item{n}{integer indicating the number of decimal places.}
}

\value{
  Rounded value
}

\seealso{\code{\link{expsize}}, \code{\link{dom_optimal_allocation}}}

\examples{

dar <- 100 * runif(3))
dar
round2(dar, 1)
}

\keyword{surveyplanning}
