\name{expvar}

\alias{expvar}

\title{Expected precision for the estimates of totals}

\description{
The function computes expexted precision as variance, standard error, and coefficient of variation for the estimates.
}

\usage{
expvar(Yh, Zh=NULL, H, s2h, nh, poph, Rh = NULL, deffh = NULL, Dom = NULL, dataset = NULL)
}

\arguments{
\item{Yh}{The expected totals for variables of interest in each stratum. Object convertible to \code{data.table}, variable names as character vector, or column numbers.}

\item{Zh}{Optional variables of denominator for the expected ratio estimation in each stratum. Object convertible to \code{data.table}, variable names as character vector, or column numbers.}

\item{H}{The stratum variable. One dimensional object convertible to one-column \code{data.table}, variable name as character, or column number.}

\item{s2h}{The expected population variance \eqn{S^2} for variables of interest in each stratum. Variables is defined the same arrangement as \code{Yh}. Object convertible to \code{data.table}, variable name as character vector, or column numbers.}

\item{nh}{Sample size in each stratum. One dimensional object convertible to one-column \code{data.table}, variable name as character, or column number.}

\item{poph}{Population size in each stratum. One dimensional object convertible to one-column \code{data.table}, variable name as character, or column number.}

\item{Rh}{The expected response rate in each stratum (optional). If not defined, it is assumed to be 1 in each stratum (full-response). Object convertible to one-column \code{data.table}, variable name as character, or column number.}

\item{deffh}{The expected design effect for the estimates of totals (optional). If not defined, it is assumed to be 1 for each variable in each stratum. If is defined, then variables is defined the same arrangement as \code{Yh}. Object convertible to \code{data.table}, variable name as character vector, or column numbers.}

\item{Dom}{Optional variables used to define population domains. Only domains as unions of strata can be defined. If supplied, estimated precision is calculated for each domain. An object convertible to \code{data.table}, variable names as character vector, or column numbers.}

\item{dataset}{Optional survey data object convertible to \code{data.table} with one row for each stratum.}
}

\value{
  A list with three data objects:

  \item{resultH}{An object as \code{data.table}, with variables: \cr
    \code{H} - stratum, \cr
    \code{variableY} - the name of variable of interest, \cr
    \code{variableZ} - the name of optional variable of denominator for the expected ratio estimation, \cr
    \code{estim} - total value, \cr
    \code{deffh} - the expected design effect, \cr
    \code{s2h} - population variance \eqn{S^2}, \cr
    \code{nh} - sample size, \cr
    \code{Rh} - the expected response rate, \cr
    \code{poph} - population size, \cr
    \code{nrh} - expected number of respondents, \cr
    \code{var} - expected variance, \cr
    \code{se} - expected standard error, \cr
    \code{cv} - expected coeficient of variance.}

  \item{resultDom}{An object as \code{data.table}, with variables: \cr
    \code{Dom} - domain, \cr
    \code{variableY} - the name of variable of interest, \cr
    \code{variableZ} - the name of optional variable of denominator for the expected ratio estimation, \cr
    \code{poph} - the population size, \cr
    \code{nh} - sample size, \cr
    \code{nrh} - expected number of respondents, \cr
    \code{estim} - total value, \cr
    \code{var} - the expected variance, \cr
    \code{se} - the expected standart error, \cr
    \code{cv} - the expected coeficient of variance.}

  \item{result}{An object as \code{data.table}, with variables: \cr
    \code{variableY} - the name of variable of interest, \cr
    \code{variableZ} - the name of optional variable of denominator for the expected ratio estimation, \cr
    \code{poph} - the population size, \cr
    \code{nh} - sample size, \cr
    \code{nrh} - expected number of respondents, \cr
    \code{estim} - total value, \cr
    \code{var} - the expected variance, \cr
    \code{se} - the expected standart error, \cr
    \code{cv} - the expected coeficient of variance.}
}

\seealso{\code{\link{expsize}}, \code{\link{optsize}}, \code{\link{dom_optimal_allocation}}}

\examples{

data <- data.table(H = 1:3,
                   Yh = 10 * 1:3, Yh1 = 10 * 4:6,
                   s2h = 10 * runif(3), s2h2 = 10 * runif(3),
                   nh = rep(4 * 1:3),
                   poph = 8 * 1:3,
                   Rh = rep(1, 3),
                   deffh = rep(2, 3), deffh2 = rep(3, 3),
                   dd = c(1, 1, 1))

vars <- expvar(Yh = c("Yh", "Yh1"), H = "H",
               s2h = c("s2h", "s2h2"),
               nh = "nh", poph = "poph",
               Rh = "Rh", deffh = c("deffh", "deffh2"),
               dataset = data)

vars
}

\keyword{surveyplanning}
