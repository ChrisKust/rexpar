eps_ind <- function(Mat, iX, eps)
{
  i1 <- c(iX[1], iX[2], iX[3])
  m <- Mat[i1[1], ] + eps * (((Mat[i1[2], ] + Mat[i1[3], ]) / 2) - Mat[i1[1], ])
  return(m)
}