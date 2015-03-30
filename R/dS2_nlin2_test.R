dS2_nlin2_test<-function(dS2, thetaN, alpha, y, exact = FALSE) {
  if (missing(dS2)) {
    dS2 <- rexpar::dS2_nlin2(thetaN, y)
  }
  NdS2 <- sqrt(floor((length(y) - 1) / 4) - 1) * (dS2 - 1/8) / sqrt(7/64)
  if (exact == TRUE) {
    deci <- ((dS2 * (floor((length(y) - 1) / 4) - 1)) <
               qbinom(alpha, size = (floor((length(y) - 1) / 4) - 1), prob = 1/8))
  } else {
    deci <- (NdS2 < qnorm(alpha))
  }
  list(TS = NdS2, phi = deci)
}