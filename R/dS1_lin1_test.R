dS1_lin1_test <- function(thetaN, alpha, y, exact = FALSE)
{
  dS1 <- rexpar::dS1_lin2(theta = thetaN, y = y, model = "linAR1woI")
  NdS1 <- sqrt(floor((length(y) - 1) / 2)) * (dS1 - 1 / 2) / sqrt(1 / 4)
  if(!exact)
  {  
  deci <- (NdS1 < qnorm(alpha))
  }
  if(exact)
  {  
  deci < - (floor((length(y) - 1) / 2) * dS1 < qbinom(alpha, size = floor((length(y) - 1) / 2), prob=1 / 2))
  }
  list(TS = NdS1, phi = deci)
}