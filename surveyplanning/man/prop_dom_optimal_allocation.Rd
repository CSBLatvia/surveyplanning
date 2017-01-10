\name{prop_dom_optimal_allocation}
\alias{prop_dom_optimal_allocation}

\title{Optimal sample size allocation for proportion}

\description{
The function computes optimal sample size allocation over strata and domain for proportion.
}

\usage{
prop_dom_optimal_allocation(H, Dom, poph,
                            Rh, deffh,
                            se_max = 0.5,
                            prop = 0.5,
                            min_size = 3,
                            step = 1,
                            dataset = NULL)
}

\arguments{

\item{H}{The stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{Dom}{Variables used to define population domains. An object convertible to \code{data.table} or variable names as character vector, column numbers.}

\item{poph}{The population size in each stratum.}

\item{Rh}{The expected response rate in each stratum (optional). If not defined, it is assumed to be 1 in each stratum (full-response). Object convertible to one-column \code{data.table}, variable name as character, or column number.}

\item{deffh}{The expected design effect for the estimate of variable (optional). If not defined, it is assumed to be 1 for each variable in each stratum. If is defined, then variables is defined the same arrangement as \code{Yh}. Object convertible to \code{data.table}, variable name as character vector, or column numbers.}

\item{se_max}{Variable for maximum standarterror (se) in domain.}

\item{prop}{The excepted ratio proportion.}

\item{min_size}{A numeric value for minimal sample size.}

\item{step}{A value for pace.}

\item{dataset}{Optional agrregated survey data object convertible to \code{data.table} with one row for each stratum.}
}


\value{

  A list with two data objects:

  \item{datah}{An object as \code{data.table}, with variables: \cr
        \code{H} - the unit stratum variable, \cr
        \code{Dom} - variables used to define population domains, \cr
        \code{poph} - the population size in each stratum, \cr
        \code{Rh} - the expected response rate in each stratum, \cr
        \code{deffh} - the expected design effect, \cr
        \code{s2h} - variance in domain of stratum, \cr
        \code{sup_cv} - Variable for maximum coeficient of variation, \cr
        \code{poph} - population size, \cr
        \code{nh} - sample size .}

   \item{aggr_Dom}{An object as \code{data.table}, with variables: \cr
        \code{Dom} - optional variables used to define population domains, \cr		
        \code{pop_Dom} - population size, \cr
        \code{sample_size_Dom} - optional variables used to define population domains, \cr
        \code{sample_size} - optional variables used to define population domains, \cr
        \code{pop} - sample size}

}

\seealso{\code{\link{expsize}}, \code{\link{optsize}}}

\examples{

data(eusilc)
eusilc <- data.table(eusilc)
data <- eusilc[, .(poph = sum(db090)), by = c("db040")]
data[, dom := "1"]
res <- prop_dom_optimal_allocation(H = "db040", Dom = "dom",
                                    poph = "poph", Rh = NULL, 
                                    deffh = NULL, se_max = 0.5,
                                    prop = 0.5, min_size = 3,
                                    step = 1, dataset = data)
}

\keyword{surveyplanning}