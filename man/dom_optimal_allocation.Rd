\name{dom_optimal_allocation}
\alias{dom_optimal_allocation}

\title{Optimal sample size allocation}

\description{
The function computes optimal sample size allocation over strata and domain for population.
}

\usage{
dom_optimal_allocation(id, Dom, H, Y, indicator, 
                                         sup_w, sup_cv, min_size=3,
                                         correction_before=FALSE,
                                         dataset=NULL)
}

\arguments{

\item{id}{Variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{Dom}{Optional variables used to define population domains. If supplied, values are calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.}

\item{H}{The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{Y}{Variable of interest. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{indicator}{Variable for full sus of interest. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{sup_w}{Variable for weight limit in domain of stratum. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{sup_cv}{Variable for maximum coeficient of variation (CV) in percentage for domain. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{correction_before}{by default FALSE; correction of sample size is made before ending, if true, correction of sample size is made at the end.}

\item{min_size}{A numeric value for sample size.}

\item{dataset}{Optional survey data object convertible to \code{data.table} with one row for each stratum.}
}


\value{

  An object as \code{data.table}, with variables:

       \code{H} - stratum, \cr
       \code{variable} - the name of variable for population variance \eqn{S^2}, \cr
       \code{s2h} - population variance \eqn{S^2}, \cr
       \code{Rh} - response rate, \cr
       \code{poph} - population size, \cr
       \code{nh} - sample size.
}

\seealso{\code{\link{expsize}}, \code{\link{optsize}}}

\examples{

#vars <- dom_optimal_allocation(id, dom, H, Y, indicator, 
#                                         sup_w, sup_cv, min_size=3,
#                                         correction_before=FALSE,
#                                         dataset=data)
#vars
}

\keyword{surveyplanning}



