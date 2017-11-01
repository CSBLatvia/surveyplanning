round2 <- function(x, n) {
    posneg <- sign(x)
    z <- abs(x) * 10 ^ n
    posneg * trunc(z + 0.5) / 10 ^ n   
}