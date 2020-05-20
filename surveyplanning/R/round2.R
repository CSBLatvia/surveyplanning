#' Rounding numbers
#'
#' @description The function rounds the values in its first argument to the specified number of decimal places (default 0).
#'
#' @param x a numeric vector.
#' @param n integer indicating the number of decimal places.
#'
#' @return  Rounded value
#'
#' @seealso \code{\link{expsize}}, \code{\link{dom_optimal_allocation}}
#'
#' @keywords surveysampling
#'
#' @examples
#' dar <- 100 * runif(3)
#' dar
#' round2(dar, 1)
#'
#' @export round2


round2 <- function(x, n) {
    posneg <- sign(x)
    z <- abs(x) * 10 ^ n
    posneg * trunc(z + 0.5) / 10 ^ n
}