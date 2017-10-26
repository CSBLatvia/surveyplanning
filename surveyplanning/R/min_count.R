min_count <- function(n, pop, RMoE, confidence = .95,
                      R = 1, deff_sam = 1, deff_est = 1) {

  minprop <-  min_prop(n = n, pop = pop, RMoE = RMoE,
                       confidence = confidence,
                       R = R, deff_sam = deff_sam,
                       deff_est = deff_est)
  return(pop * minprop)
}