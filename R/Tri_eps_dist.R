Tri_Eps_dist <- function(y, perc = 1, eps = 0.1)
{
  LT <- lin2_theta_f(y)
  ll <- cbind(LT$t1, LT$t2)
  l1l <- quantile(LT$t1, 0.5 - perc / 2)
  l1u <- quantile(LT$t1, 0.5 + perc / 2)
  l2l <- quantile(LT$t2, 0.5 - perc / 2)
  l2u <- quantile(LT$t2, 0.5 + perc / 2)
  il1 <- which(ll[, 1] <= l1u & ll[, 1] >= l1l)
  ll <- ll[il1, ]
  il2 <- which(ll[, 2] <= l2u & ll[, 2] >= l2l)
  ll <- ll[il2, ]
  d1 <- ll[, 1]
  d2 <- ll[, 2]
  v1 <- apply(cbind(d1, d2), 2, eps_ind_dist, Mat = cbind(d1, d2), eps = eps)
  v2 <- apply(cbind(d1, d2), 2, eps_ind_dist, Mat = cbind(d1, d2), eps = (-1) * eps)
  sol <- rbind(v1, v2)
  return(sol)
}