\name{expsize}

\alias{expsize}

\title{Expexted size for sample}

\description{
  The function computes expexted size for sample.
}

\usage{
  expsize(Yh, H, s2h, poph, Rh, deffh,  CVh, dataset = NULL)
}

\arguments{
\item{Yh}{Estimated variables of interest by stratum. Object convertible to \code{data.table} or variable names as character, column numbers or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{H}{The stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{s2h}{The estimated s2 for variables of interest. Object convertible to \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{poph}{Population size. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{Rh}{Response rate. Object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{deffh}{Design effect for variables of interest. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{CVh}{Planned coefficient of variation (in percentage) for each stratum. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{dataset}{Optional survey data object convertible to \code{data.table}.}
}



\value{

A \code{data.table} is returned by the function, which containing variables:

       \code{H} - the stratum, \cr
       \code{variable} - the names of variables of interest, \cr
       \code{estim} - the estimated value, \cr
       \code{deffh} - the estimated design effect, \cr
       \code{s2h} - the estimated s2, \cr
       \code{CVh} - the planned coefficient of variation, \cr
       \code{Rh} - response rate, \cr
       \code{poph} - the population size, \cr
       \code{nh} - the expected sample size.
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

