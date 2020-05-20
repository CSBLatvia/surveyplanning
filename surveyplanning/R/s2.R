#' Population variance
#'
#' @description The function to estimate population variance \eqn{S^2}.
#'
#' @section Details:
#' If \code{w} is not defined, the result is equal to the result of the function \code{var}.
#'
#' @param y Study variable.
#' @param w Survey weight (optional). If not defined, it is assumed to be 1 for each element.
#'
#' @return Population variance \eqn{S^2} or the estimate of population variance \eqn{s^2}.
#'
#' @examples
#' s2(1:10)
#' s2(1:10, rep(1:2, each = 5))
#' all.equal(s2(1:10), var(1:10))
#'
#' @export s2


# S^2 estimation
s2 <- function(y, w = NULL) {
  if (is.null(w)) w <- rep(1, length(y))
  N <- sum(w)
  n <- length(y)
  s2 <- (N - 1) / N * n / (n - 1) *
    (sum(y ^ 2 * w) - sum(y * w) ^ 2 / N) / (N - 1)
  return(s2)
}

