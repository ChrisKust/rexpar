oner <- function(resid)
{
  M2 <- matrix(rep(resid[2:(length(resid) - 1)], each = length(resid) - 2), ncol = length(resid) - 2, byrow = TRUE)
  M2[rexpar::inv_tri(M2) == TRUE] <- 0
  M3 <- matrixcalc::hankel.matrix(length(resid) - 2, resid[3:length(resid)])
  M3[rexpar::inv_tri(M3) == TRUE] <- 0
  MT <- (resid[1] > 0) * (M2 < 0) * (M3 > 0) + (resid[1] < 0) * (M2 > 0) * (M3 < 0)#-(resid[1]==0)*(M2==0)*(M3==0)
  return(sum(MT))
}