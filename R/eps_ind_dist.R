eps_ind_dist <- function(v1, Mat, eps)
{
  dists <- apply(Mat, 1, Ele_Norm, center = v1)
  v2 <- Mat[dists == sort(dists)[2], ]
  v3 <- Mat[dists == sort(dists)[3], ]
  m <- v1 + eps * (((v2 + v3) / 2) - v1)
  return(m)
}