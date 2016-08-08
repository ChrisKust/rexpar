dS_nlin_test <- function(theta, alpha, y, ncores = 1, dS)
{
  if (missing(dS)) {
    dS <- rexpar::dS_lin2(theta = theta, y = y, ncores = ncores, model = "nlinAR1")
  }
  CdS <- (length(y) - 1) * (dS - 1 / 4)
  deci <- (CdS < SimQuants[round(SimQuants[, 1], digits = 3) == round((alpha), digits = 3), 2])
  list(TS = CdS, phi = deci)
}