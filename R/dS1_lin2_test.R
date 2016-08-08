dS1_lin2_test <- function(theta, alpha, y, exact = FALSE, cpow = 1, dS1)
{
  if(!is.numeric(cpow))
  {
    stop("Power must be a numeric value!")
  }
  if (missing(dS1)) {
    if(cpow == 1)
    {
    dS1 <- rexpar::dS1_lin2(theta = theta, y = y)
    }
    if(cpow != 1)
    {
    dS1 <- rexpar::dS1_lin2(theta = theta, y = y, cpow = cpow, model = "linARc")
    }
  }
  NdS1 <- sqrt(floor((length(y) - 1) / 3)) * (dS1 - 1 / 4) / sqrt(3 / 16)
  if(exact)
  {
  deci <- ((dS1 * floor((length(y) - 1) / 3)) < qbinom(alpha, size = floor((length(y) - 1) / 3), prob = 1 / 4))
  }
  else
  {
  deci <- (NdS1 < qnorm(alpha))
  }
  list(TS = NdS1, phi = deci)
}

