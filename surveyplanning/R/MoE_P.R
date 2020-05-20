#' Margin of error for proportion
#'
#' @description The function computes margin of error for proportion. The calculation takes into proportion, expected response rate and design effect.
#'
#' @param P The expected proportion for variable of interest.
#' @param n The expected sample size.
#' @param pop Population size.
#' @param confidence}{Optional positive value for confidence interval. This variable by default is 0.95.
#' @param R The expected response rate (optional). If not defined, it is assumed to be 1 (full-response).
#' @param deff_sam The expected design effect of sample design for the estimates (optional). If not defined, it is assumed to be 1.
#' @param deff_est The estimated design effect of estimator for the estimates (optional). If not defined, it is assumed to be 1.
#'
#' @return  The estimate of margin of error for proportion.
#'
#' @seealso \code{\link{expvar}}, \code{\link{optsize}}, \code{\link{MoE_Y}}
#'
#' @keywords surveysampling
#'
#' @examples
#' library("data.table")
#' n <- 100
#' pop <- 1000
#'
#' MoE_P(P = 0.5, n = n, pop = pop)
#'
#' DT <- data.table(P = seq(0, 1, 0.01))
#' DT[, Y := round(pop * P)]
#' DT[, AMoE := MoE_P(P, n = 100, pop = 1000)]
#' DT[Y > 0, RMoE := AMoE / Y]
#' DT
#'
#' @import data.table
#' @export MoE_P

MoE_P <- function(P = 0.5, n, pop, confidence = .95,
                  R = 1, deff_sam = 1, deff_est = 1) {

# Margin of error for proportion

  Z <- qnorm((1 + confidence) / 2)
  n_R <- n * R
  f <- n_R / pop
  deff <- deff_sam * deff_est

  calcs <- Z * sqrt((1 - f) / n_R * pop / (pop - 1) * P * (1 - P) * deff)
  return(calcs)
}