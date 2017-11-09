\name{s2}
\alias{s2}

\title{Population variance}

\description{
The function to estimate population variance \eqn{S^2}.
}

\usage{
s2(y, w)
}

\arguments{

\item{y}{Study variable}

\item{w}{Survey weight (optional). If not defined, it is assumed to be 1 for each element.}

}

\details{
If \code{w} is not defined, the result is equal to the result of the function \code{var}.
}

\value{
Population variance \eqn{S^2} or the estimate of population variance \eqn{s^2}.

Returns \code{NA} if \code{length(y) == 1L}.
}

\examples{

s2(1:10)
s2(1:10, rep(1:2, each = 5))
all.equal(s2(1:10), var(1:10))

}
