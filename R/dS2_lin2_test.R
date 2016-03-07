dS2_lin2_test <- function(thetaN, alpha, y, exact = FALSE, cpow = 1, dS2) {
  if (missing(dS2)) {
    if (cpow == 1) {
      dS2 <- rexpar::dS2_lin2(theta = thetaN, y = y)
    }
    if (cpow != 1) {
      dS2 <- rexpar::dS2_lin2(theta = thetaN, y = y, cpow = cpow, model = "linARc")
    }
  }
  NdS2 <- sqrt(floor((length(y) - 1) / 2) - 1) * (dS2 - 1/4) / sqrt(3/16)
  if (exact == TRUE) {
    deci <- ((dS2 * (floor((length(y) - 1) / 2) - 1)) <
               qbinom(alpha, size = (floor((length(y) - 1) / 2) - 1), prob = 1/4))
  } else {
    deci <- (NdS2 < qnorm(alpha))
  }
  list(TS = NdS2, phi = deci)
}