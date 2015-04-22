dS_lin2_test <- function(thetaN, alpha, y, ncores = 1, cpow = 1, dS) {
  if (missing(dS)) {
    if (cpow == 1) {
      dS <- rexpar::dS_lin2(theta=thetaN, y=y, ncores=ncores)
    }
    if (cpow != 1) {
      dS <- rexpar::dS_lin2(theta=thetaN, y=y, ncores=ncores, cpow = cpow, model = "linARc")
    }
  }
  CdS <- (length(y) - 1) * (dS - 1/4)
  deci <- (CdS < SimQuants[round(SimQuants[ ,1], digits = 3) == round((alpha), digits = 3),2])
  list(TS = CdS, phi = deci)
}