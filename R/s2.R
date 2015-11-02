# S^2 estimation
s2 <- function(y, w = rep(1, length(y))) {
  N <- sum(w)
  s2 <- (sum(y^2 * w) - sum(y * w)^2 / N) / (N-1)
  return(s2)
}
