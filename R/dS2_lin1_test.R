dS2_lin1_test <- function(theta, alpha, y, exact = FALSE, dS2)
{
  if (missing(dS2)) {
    dS2 <- rexpar::dS2_lin2(theta = theta, y = y, model = "linAR1woI")
  }
  NdS2 <- sqrt(floor((length(y) - 1) / 2)) * (dS2 - 1 / 2) / sqrt(1 / 4)
  if(!exact)
  {  
  deci <- (NdS2 < qnorm(alpha))
  }
  if(exact)
  {  
    deci <- (floor((length(y) - 1) / 2) * dS2 < qbinom(alpha, size = floor((length(y) - 1) / 2), prob = 1 / 2))
  }
  list(TS = NdS2, phi = deci)
}