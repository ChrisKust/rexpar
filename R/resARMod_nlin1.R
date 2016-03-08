resARMod_nlin1 <- function(theta, dat)
{
  dat1 <- dat[1:(length(dat) - 1)]
  dat2 <- dat[2:length(dat)]
  res <- dat2 - theta[1] * dat1^theta[2] - dat1
  return(res)
}