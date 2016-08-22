dS_lin1_test <- function(theta, alpha, y, mod = FALSE, dS)
{
  if (missing(dS)) {
    dS <- rexpar::dS_lin1(theta = theta, y = y, mod = mod)
  }
  CdS <- (length(y) - 1) * (dS - 1 / 2)
  deci <- (CdS < 1 / 2 - 1 / 2 * qchisq((1 - alpha), 1)) 
  list(TS = CdS, phi = deci)
}