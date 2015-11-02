\name{expvar}

\alias{expvar}

\title{Expexted variance for variables of interest.}

\description{
  The function computes expexted variance for variables of interest.
}

\usage{
expvar(Yh, H, s2h, nh, poph, Rh, deffh, Dom=NULL, dataset = NULL, confidence = .95)
}

\arguments{
\item{Yh}{Estimated variables of interest. Object convertible to \code{data.table} or variable names as character, column numbers or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{H}{The stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{s2h}{The estimated s2 for variables of interest. Object convertible to \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{nh}{Sample size. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one  \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{poph}{Population size. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{Rh}{Response rate. Object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{deffh}{Design effect for variables of interest. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{Dom}{Optional variables used to define population domains. If supplied, estimated variables of interest are calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

\item{dataset}{Optional survey data object convertible to \code{data.table}.}

\item{confidence}{Optional positive value for confidence interval. This variable by default is 0.95.}
}

\seealso{\code{\link{expsize}}, \code{\link{optsize}}}

\value{

  A list with objects are returned by the function:

  \item{resultH}{A \code{data.table}, which containing variables:

       \code{H} - the stratum, \cr
       \code{variable} - the names of variables of interest, \cr
       \code{estim} - the estimated value, \cr
       \code{deffh} - the estimated design effect, \cr
       \code{s2h} - the estimated s2, \cr
       \code{nh} - the planned coefficient of variation, \cr
       \code{Rh} - response rate, \cr
       \code{poph} - the population size, \cr
       \code{nrh} - the corrected sample size by response rate, \cr
       \code{var} - the expected variance, \cr
       \code{se} - the expected standart error, \cr
       \code{cv} - the expected coeficient of variance.}

  \item{result}{A \code{data.table}, which containing variables:

       \code{variable} - the names of variables of interest, \cr
       \code{estim} - the estimated value, \cr
       \code{var} - the expected variance, \cr
       \code{se} - the expected standart error, \cr
       \code{cv} - the expected coeficient of variance.}
}

\examples{

data <- data.table(H=1:3, Yh=10*1:3, Yh1=10*4:6,
                   s2h=10*runif(3), s2h2=10*runif(3),
                   nh=rep(4*1:3), poph=8*1:3, 
                   Rh=rep(1,3), deffh=rep(2,3),
                   deffh2=rep(3,3), dd=c(1,1,1))
                   
vars <- expvar(Yh=c("Yh", "Yh1"), H="H", 
               s2h=c("s2h", "s2h2"), 
               nh="nh", poph="poph",
               Rh="Rh", deffh=c("deffh", "deffh2"),
                 dataset = data)
vars


}
 
\keyword{surveyplanning}


