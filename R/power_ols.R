power_ols <- function(thetas, N, R, sv, cont, theta0, alpha)
{
  ind <- 0
  set.seed(round((thetas[1] + thetas[2]) * 100000, digits = 0))
  for(r in 1:R)
  {
    y <- RandomARMod_lin2(N, thetas[1], thetas[2], sv, cont)
    olsstat <- ols_ts(y, theta0)
    ind1 <- (abs(olsstat$Ttheta0) > qnorm(1 - alpha / 4))
    ind2 <- (abs(olsstat$Ttheta1) > qnorm(1 - alpha / 4))
    ind <- ind + ( - ind1 * ind2 + ind1 + ind2)
  }
  return(1 / R * ind)
}