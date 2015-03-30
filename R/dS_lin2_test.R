dS_lin2_test <- function(dS, thetaN, alpha, y, ncores = 1, cpow = 1) {
  if (missing(dS)) {
    if (cpow == 1) {
      dS <- rexpar::dS_lin2(thetaN, y, ncores)
    }
    if (cpow != 1) {
      dS <- rexpar::dS_lin2(thetaN, y, ncores, cpow = cpow, model = "linARc")
    }
  }
  CdS <- (length(y) - 1) * (dS - 1/4)
  deci <- (CdS < SimQuants[round(SimQuants[ ,1], digits = 3) == round((alpha), digits = 3),2])
  list(TS = CdS, phi = deci)
}