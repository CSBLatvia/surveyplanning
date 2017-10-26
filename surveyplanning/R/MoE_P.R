
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