\name{optsize}
\alias{optsize}

\title{Optimal sample size for variables of interest.}

\description{
  The function computes optimal sample size for variables of interest.
}

\usage{
optsize(H, s2h, n, poph, Rh, dataset = NULL)
}

\arguments{
\item{H}{The stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{s2h}{The estimated s2 for variables of interest. Object convertible to \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{n}{Total sample size. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one  \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{poph}{Population size. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{Rh}{Response rate. Object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{dataset}{Optional survey data object convertible to \code{data.table}.}

}

\seealso{\code{\link{expsize}}, \code{\link{optsize}}}

\value{

  A \code{data.table} is returned by the function, which containing variables:

       \code{H} - the stratum, \cr
       \code{variable} - the names of variables of interest, \cr
       \code{s2h} - the estimated s2, \cr
       \code{Rh} - response rate, \cr
       \code{poph} - the population size, \cr
       \code{pnh} - the corrected population size, \cr
       \code{nh} - the sample size.
}

\examples{

data <- data.table(H=1:3, Yh=10*1:3, Yh1=10*4:6,
                   s2h=10*runif(3), s2h2=10*runif(3),
                   poph=8*1:3, Rh=rep(1,3),  dd=c(1,1,1))
                   
vars <- optsize(H="H", s2h=c("s2h", "s2h2"), 
                n=10, poph="poph",
                Rh="Rh", dataset = data)
vars

}
 
\keyword{surveyplanning}
