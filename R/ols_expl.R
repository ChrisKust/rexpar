ols_expl <- function(y)
{
  y1 <- y[1:(length(y) - 1)]
  y2 <- y[2:length(y)]
  theta1_h <- sum((y2 - mean(y2)) * (y1 - mean(y1))) / sum((y1 - mean(y1))^2)
  theta0_h <- mean(y2) - theta1_h * mean(y1)
  sigma2_h <- 1 / length(y) * sum((y2 - theta0_h - theta1_h * y1)^2)
  list(theta0 = theta0_h, theta1 = theta1_h, sigma2 = sigma2_h)
}