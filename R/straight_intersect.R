straight_intersect <- function(v1, v2, v3, v4, plots = FALSE)
{
  if(plots)
  {
  points(c(v1[1], v2[1], v3[1], v4[1]), c(v1[2], v2[2], v3[2], v4[2]), col = c(1, 1, 2, 2))
  lines(c(v1[1], v2[1]), c(v1[2], v2[2]))
  lines(c(v3[1], v4[1]), c(v3[2], v4[2]), col = 2)
  }
  #check if prallel
  d1 <- v2 - v1
  d2 <- v4 - v3
  
  if((d1[1] / d2[1] == d1[2] / d2[2]) || (d1[1] == 0 && d1[2] == d2[2]) || (d2[1] == 0 && d1[2] == d2[2]) || (d1[2] == 0 && d1[1] == d2[1])  || (d2[2] == 0 && d1[1] == d2[1])||(d1[2] == 0 && d2[2] == 0) ||(d1[1] == 0 && d2[1] == 0) ||(d1[1] == 0 && d2[2] == 0 && d1[2] == 0 && d2[2] == 0))
  {
  lambda <- NA
  res <- FALSE
  mu <- NA
  }
  
  else{
  nf <- v1[1] * (v4[2] - v3[2]) + v2[1] * (v3[2] - v4[2]) + v4[1] * (v2[2] - v1[2]) + v3[1] * (v1[2] - v2[2])
  lambda <- (v1[1] * (v4[2] - v3[2]) + v3[1] * (v1[2] - v4[2]) + (v3[2] - v1[2]) * v4[1]) / nf
  mu <- - (v1[1] * (v3[2] - v2[2]) + v2[1] * (v1[2] - v3[2]) + (v2[2] - v1[2]) * v3[1]) / nf
  
  interceptA <- v1 + lambda * (v2 - v1)
  interceptB <- v3 + mu * (v4 - v3)
  if(plots)
  {
  points(interceptA[1], interceptA[2], col = 3, pch = 19)
  points(interceptB[1], interceptB[2], col = 4, pch = 1)
  }
  if(lambda < 1 && lambda > 0 && mu < 1 && mu > 0)
  {
   res <- TRUE
   if(plots)
   {
   points(interceptA[1], interceptA[2], col = 5, pch = 19)
   points(interceptB[1], interceptB[2], col = 6, pch = 1)
  }
  }
  else{res <- FALSE}
  }
  list(res = res, mu = mu, lambda = lambda)
  
}


