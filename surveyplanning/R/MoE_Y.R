# Margin of error for count

MoE_Y <- function(P = 0.5, n, pop, confidence = .95,
                  R = 1, deff_sam = 1, deff_est = 1) {
  calcs <- MoE_P(P = P, n = n, pop = pop, confidence = confidence,
                 R = R, deff_sam = deff_sam, deff_est = deff_est)
  return(pop * calcs)
}