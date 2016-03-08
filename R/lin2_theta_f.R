lin2_theta_f <- function(dat)
{
  dat1 <- dat[1:(length(dat) - 1)]
  dat2 <- dat[2:length(dat)]
  M1 <- matrix(c(rep(dat2[1:length(dat2)], (length(dat2)))), ncol = length(dat2)) - matrix(c(rep(dat2[1:length(dat2)], (length(dat2)))), ncol = length(dat2), byrow = TRUE)
  M2 <- matrix(c(rep(dat1[1:length(dat2)], (length(dat2)))), ncol = length(dat1)) - matrix(c(rep(dat1[1:length(dat2)], (length(dat1)))), ncol = length(dat1), byrow = TRUE)
  Mt1 <- M1 / M2
  Mt2 <- matrix(c(rep(dat2[1:length(dat2)], (length(dat2)))), ncol = length(dat2), byrow = TRUE) - M1 / M2 * matrix(c(rep(dat1[1:length(dat2)], (length(dat1)))), ncol = length(dat1), byrow = TRUE)
  t1 <- c(lower.tri(Mt1) * Mt1)
  t2 <- c(lower.tri(Mt2) * Mt2)

  t1 <- t1[!is.na(t1)]
  t2 <- t2[!is.na(t2)]
  t2 <- t2[!is.na(t2)]
  t1 <- t1[!is.na(t2)]
  t2 <- t2[t1 != 0]
  t1 <- t1[t1 != 0]
  t1 <- t1[t2 != 0]
  t2 <- t2[t2 != 0]
  t2 <- t2[t2 != 0]
  list(t1 = t2, t2 = t1)
}