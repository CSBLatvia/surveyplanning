
\name{expsize}

\alias{expsize}

\title{Sample size calculation}

\description{
The function computes minimum sample size for each stratum to achieve defined precision (CV) for the estimates of totals in each stratum. The calculation takes into account expected totals, population variance, expected response rate and design effect in each stratum.
}

\usage{
expsize(Yh, H, s2h, poph, Rh = NULL, deffh = NULL, CVh, dataset = NULL)
}

\arguments{
\item{Yh}{The expected totals for variables of interest in each stratum. Object convertible to \code{data.table}, variable names as character vector, or column numbers.}

\item{H}{The stratum variable. One dimensional object convertible to one-column \code{data.table}, variable name as character, or column number.}

\item{s2h}{The expected population variance \eqn{S^2} for variables of interest in each stratum. Object convertible to \code{data.table}, variable name as character vector, or column numbers.}

\item{poph}{Population size in each stratum. One dimensional object convertible to one-column \code{data.table}, variable name as character, or column number.}

\item{Rh}{The expected response rate in each stratum (optional). If not defined, it is assumed to be 1 in each stratum (full-response). Object convertible to one-column \code{data.table}, variable name as character, or column number.}

\item{deffh}{The expected design effect for the estimates of totals (optional). If not defined, it is assumed to be 1 for each variable in each stratum. Object convertible to \code{data.table}, variable name as character vector, or column numbers.}

\item{CVh}{Coefficient of variation (in percentage) to be achieved for each stratum. One dimensional object convertible to one-column \code{data.table}, variable name as character, or column number.}

\item{dataset}{Optional survey data object convertible to \code{data.table} with one row for each stratum.}
}

\value{

A \code{data.table} is returned by the function, with variables:

       \code{H} - stratum, \cr
       \code{variable} - the name of variable of interest, \cr
       \code{estim} - total value, \cr
       \code{deffh} - the expected design effect, \cr
       \code{s2h} - population variance \eqn{S^2}, \cr
       \code{CVh} - the expected coefficient of variation, \cr
       \code{Rh} - the expected response rate, \cr
       \code{poph} - population size, \cr
       \code{nh} - minimal sample size to achieve defined precision (CV).
}


\examples{

data <- data.table(H=1:3, Yh=10*1:3, Yh1=10*4:6,
                   s2h=10*runif(3), s2h2=10*runif(3),
                   CVh=rep(4.9, 3),
                   poph=8*1:3, Rh=rep(1,3),
                   deffh=rep(2,3), deffh2=rep(3,3))

size <- expsize(Yh=c("Yh", "Yh1"), H="H",
                s2h=c("s2h", "s2h2"), poph="poph",
                Rh="Rh", deffh=c("deffh", "deffh2"),
                CVh="CVh", dataset = data)
size
}

\seealso{
 \code{\link{expvar}}, \code{\link{optsize}}}

\keyword{surveyplanning}
