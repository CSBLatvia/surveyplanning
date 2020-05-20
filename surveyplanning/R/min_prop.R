#' Minimal proportion for the given relative margin of error
#'
#' @description The function computes minimal proportion for the given relative margin of error. The calculation takes into sample size, population size, margin of error, expected response rate and design effect.
#'
#' @param n The expected sample size.
#' @param pop Population size.
#' @param RMoE The expected relative margin of error.
#' @param confidence Optional positive value for confidence interval. This variable by default is 0.95.
#' @param R The expected response rate (optional). If not defined, it is assumed to be 1 (full-response).
#' @param deff_sam The expected design effect of sample design for the estimates (optional). If not defined, it is assumed to be 1.
#' @param deff_est The estimated design effect of estimator for the estimates (optional). If not defined, it is assumed to be 1.
#'
#' @return  The estimate of minimal proportion for the given relative margin of error.
#'
#' @seealso \code{\link{expvar}}, \code{\link{optsize}}, \code{\link{MoE_P}}
#'
#' @keywords surveysampling
#'
#' @examples
#' min_prop(n = 100, pop = 1000, RMoE = 0.1)
#'
#' @import stats
#' @export min_prop


min_prop <- function(n, pop, RMoE, confidence = .95,
                      R = 1, deff_sam = 1, deff_est = 1) {

  if (anyNA(n)) stop("'n' has unknown values")
  if (anyNA(pop)) stop("'pop' has unknown values")
  if (anyNA(RMoE)) stop("'RMoE' has unknown values")
  if (anyNA(confidence)) stop("'confidence' has unknown values")

  Z <- qnorm((1 + confidence) / 2)
  n_R <- n * R
  f <- n_R / pop
  deff <- deff_sam * deff_est

  A <- Z ^ 2 * (1 - f) * pop / (pop - 1) * deff

  minP <- 1 / (n_R * RMoE ^ 2 / A + 1)

  return(minP)
}