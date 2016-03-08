mind <- function(Mat, iX)
{
  i1 <- c(iX[1], iX[2], iX[3])
  m <- mean(Mat[i1])
  return(m)
}