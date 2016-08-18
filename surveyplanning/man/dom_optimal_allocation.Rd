\name{dom_optimal_allocation}
\alias{dom_optimal_allocation}

\title{Optimal sample size allocation}

\description{
The function computes optimal sample size allocation over strata and domain for population.
}

\usage{
dom_optimal_allocation(id, Dom, H, Y, Rh=NULL, deffh=NULL,
                               indicator, sup_w, sup_cv,
                               min_size=3, correction_before=FALSE,
                               dataset=NULL)
}

\arguments{

\item{id}{Variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{Dom}{Optional variables used to define population domains. If supplied, values are calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.}

\item{H}{The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{Y}{Variable of interest. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{Rh}{The expected response rate in each stratum (optional). If not defined, it is assumed to be 1 in each stratum (full-response). Object convertible to one-column \code{data.table}, variable name as character, or column number.}

\item{deffh}{The expected design effect for the estimate of variable (optional). If not defined, it is assumed to be 1 for each variable in each stratum. If is defined, then variables is defined the same arrangement as \code{Yh}. Object convertible to \code{data.table}, variable name as character vector, or column numbers.}

\item{indicator}{Variable for detection fully surveyed units. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{sup_w}{Variable for weight limit in domain of stratum. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{sup_cv}{Variable for maximum coeficient of variation (CV) in percentage for domain. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{correction_before}{by default FALSE; correction of sample size is made before ending, if true, correction of sample size is made at the end.}

\item{min_size}{A numeric value for sample size.}

\item{dataset}{Optional survey data object convertible to \code{data.table} with one row for each stratum.}
}


\value{

  A list with eights data objects:

  \item{data}{An object as \code{data.table}, with variables: \cr
        \code{id} - variable with unit ID codes, \cr
        \code{Dom} - optional variables used to define population domains, \cr
        \code{H} - the unit stratum variable, \cr
        \code{Y} - variable of interest, \cr
        \code{Rh} - the expected response rate in each stratum, \cr
        \code{deffh} - the expected design effect, \cr
        \code{indicator} - variable for full surveys, \cr
        \code{sup_w} - variable for weight limit in domain of stratum, \cr
        \code{sup_cv} - Variable for maximum coeficient of variation, \cr
        \code{poph} - population size, \cr
        \code{nh} - sample size .}

   \item{nh_larger_then_Nh}{An object as \code{data.table}, with variables: \cr
         \code{H} - the stratum variable, \cr
         \code{nh} - sample size, 
         \code{poph} - population size.}

   \item{dom_strata_size}{An object as \code{data.table}, with variables: \cr
        \code{H} - the unit stratum variable, \cr
        \code{Dom} - optional variables used to define population domains, \cr
        \code{sup_w} - variable for weight limit in domain of stratum, \cr
        \code{poph} - population size, \cr
        \code{nh} - sample size, \cr
        \code{sample100} - sample size for fully surveyed units, \cr
        \code{design_weights} - design weigts. }

   \item{dom_size}{An object as \code{data.table}, with variables: \cr
        \code{Dom} - optional variables used to define population domains, \cr
        \code{poph} - population size, \cr
        \code{nh} - sample size, \cr
        \code{sample100} - sample size for fully surveyed units, \cr
        \code{design_weights} - design weigts. }

   \item{size}{An object as \code{data.table}, with variables: \cr
        \code{poph} - population size, \cr
        \code{nh} - sample size, \cr
        \code{sample100} - sample size for fully surveyed units. }

   \item{dom_strata_expected_precision}{An object as \code{data.table}, with variables: \cr
        \code{H} - stratum, \cr
        \code{variable} - the name of variable of interest, \cr
        \code{estim} - total value, \cr
        \code{deffh} - the expected design effect, \cr
        \code{s2h} - population variance \eqn{S^2}, \cr
        \code{nh} - sample size, \cr
        \code{Rh} - the expected response rate, \cr
        \code{deffh} - the expected design effect, \cr
        \code{poph} - population size, \cr
        \code{nrh} - expected number of respondents, \cr
        \code{var} - expected variance, \cr
        \code{se} - expected standard error, \cr
        \code{cv} - expected coeficient of variance.}

   \item{dom_expected_precision}{An object as \code{data.table}, with variables: \cr
        \code{Dom} - domain, \cr
        \code{variable} - the name of variable of interest, \cr
        \code{poph} - the population size, \cr
        \code{nh} - sample size, \cr
        \code{nrh} - expected number of respondents, \cr
        \code{estim} - total value, \cr
        \code{var} - the expected variance, \cr
        \code{se} - the expected standart error, \cr
        \code{cv} - the expected coeficient of variance.}

   \item{ total_expected_precision}{An object as \code{data.table}, with variables: \cr
        \code{variable} - the name of variable of interest, \cr
        \code{poph} - the population size, \cr
        \code{nh} - sample size, \cr
        \code{nrh} - expected number of respondents, \cr
        \code{estim} - total value, \cr
        \code{var} - the expected variance, \cr
        \code{se} - the expected standart error, \cr
        \code{cv} - the expected coeficient of variance.}
}

\seealso{\code{\link{expsize}}, \code{\link{optsize}}}

\examples{

data(eusilc)

#vars <- dom_optimal_allocation(id, dom, H, Y, indicator, 
#                                         sup_w, sup_cv, min_size=3,
#                                         correction_before=FALSE,
#                                         dataset=data)
#vars
}

\keyword{surveyplanning}