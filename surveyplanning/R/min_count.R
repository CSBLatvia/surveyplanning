#' Minimal count of respondents for the given relative margin of error
#'
#' The function computes minimal proportion for the given relative margin of error. The calculation takes into sample size, population size,
#' margin of error, expected response rate and design effect.
#'
#' @param n The expected sample size.
#' @param pop Population size.
#' @param RMoE The expected relative margin of error.
#' @param confidence Optional positive value for confidence interval. This variable by default is 0.95.
#' @param R The expected response rate (optional). If not defined, it is assumed to be 1 (full-response).
#' @param deff_sam The expected design effect of sample design for the estimates (optional). If not defined, it is assumed to be 1.
#' @param deff_est The estimated design effect of estimator for the estimates (optional). If not defined, it is assumed to be 1.
#'
#' @return  The estimate of minimal count of respondents for the given relative margin of error.
#'
#' @seealso \code{\link{expvar}}, \code{\link{optsize}}, \code{\link{MoE_P}}
#'
#' @keywords surveysampling
#'
#' @examples
#' min_count(n = 15e3, pop = 2e6, RMoE = 0.1)
#'
#' \dontrun{
#' library(data.table)
#' min_count(n = c(10e3, 15e3, 20e3), pop = 2e6, 0.1)
#'
#' n <- seq(10e3, 30e3, length.out = 11)
#' # n <- sort(c(n, 22691))
#' n
#'
#' RMoE <- seq(.02, .2, length.out = 10)
#' RMoE
#'
#' dt <- data.table(n = rep(n, each = length(RMoE)), RMoE = RMoE)
#' dt[, Y := min_count(n = n, pop = 2.1e6, RMoE = RMoE, R = 1) / 1e3]
#' dt
#'}
#'
#' @import data.table
#' @export min_count
#'

min_count <- function(n, pop, RMoE, confidence = .95,
                      R = 1, deff_sam = 1, deff_est = 1) {

  minprop <-  min_prop(n = n, pop = pop, RMoE = RMoE,
                       confidence = confidence,
                       R = R, deff_sam = deff_sam,
                       deff_est = deff_est)
  return(pop * minprop)
}