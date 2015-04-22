dS3_nlin2_test <- function(dS3, thetaN, alpha, y, exact = FALSE) {
  if (missing(dS3)) {
    dS3 <- rexpar::dS3_nlin2(thetaN, y)
  }
  NdS3 <- sqrt((length(y) - 1) - 3) * (dS3 - 1/8) / sqrt(15/64)
  if (exact == TRUE) {
    q <- quantile(exact_lim_dS3nlin(N = length(y), Reps = 1000), prob = alpha)
    deci <- (((length(y) - 1) - 3) * dS3 < q)
  } else {
    deci <- (NdS3 < qnorm(alpha))
  }
  list(TS = NdS3, phi = deci)
}