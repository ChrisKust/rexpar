dS1_nlin2_test_b <- function(ind, thetaNv, alpha, y, exact = FALSE, dS1)
{
  if (missing(dS1)) {
    thetaN <- t(thetaNv[ind, ])
    dS1 <- rexpar::dS1_nlin2(thetaN, y)
  }
  NdS1 <- sqrt(floor((length(y) - 1) / 4)) * (dS1 - 1 / 8) / sqrt(7 / 64)
  if(exact)
  {
    deci <- ((dS1 * floor((length(y) - 1) / 4)) < qbinom(alpha, size = floor((length(y) - 1) / 4), prob = 1 / 8))
  }
  else
  {
    deci <- (NdS1 < qnorm(alpha))
  } 
  list(TS = NdS1, phi = deci)
}