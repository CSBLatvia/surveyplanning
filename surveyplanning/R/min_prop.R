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