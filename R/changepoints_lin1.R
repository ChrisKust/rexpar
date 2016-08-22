changepoints_lin1 <- function(y, level, bw, sw, method = "m1", mincper = 1, mincp = 1)
{
  alpha <- (1 - level)
  cands <- seq(bw + 1, length(y) - bw, sw)
  changeind <- numeric(length(cands))

  changeind <- switch(method,
    m1 = {
    for(i in 1:length(cands))
    {
      ASp1 <- lin1_CI(y[(cands[i]-bw):cands[i]], (1-alpha), notion = "dS")
      ASp2 <- lin1_CI(y[cands[i]:(cands[i]+bw)], (1-alpha), notion = "dS")
      changeind[i] <- 1 - (max(ASp1$par[!ASp1$inCI]) > (min(ASp2$par[!ASp2$inCI])))*(min(ASp1$par[!ASp1$inCI]) < (min(ASp2$par[!ASp2$inCI]))) - (max(ASp2$par[!ASp2$inCI]) > (min(ASp1$par[!ASp1$inCI])))*(min(ASp2$par[!ASp2$inCI]) < (min(ASp1$par[!ASp1$inCI])))
    }
    unlist(changeind)

  },
  
  m2 = {
    for(i in 1:length(cands))
    {
      E1 <- est_lin1(y[(cands[i]-bw):cands[i]], notion = "dS")$estimate
      E2 <- est_lin1(y[cands[i]:(cands[i] + bw)], notion = "dS")$estimate
      
      T1 <- dS_lin1_test(theta = E1, alpha = alpha, y = y[cands[i]:(cands[i] + bw)])$phi
      T2 <- dS_lin1_test(theta = E2, alpha = alpha, y = y[(cands[i] - bw):cands[i]])$phi
      changeind[i] <- T1 * T2
    }
    unlist(changeind)
  }, 
  stop("Enter valid method !")
  )
  changepoints <- numeric(length(changeind))
  changepoints[!changeind] <- FALSE
  changepoints[changeind > 0] <- TRUE
  totjumps <- cands[changeind > 0]
  clus <- follow_ups(totjumps, mincper = mincper, steps = sw, mincp = mincp)
  cjumps <- clus$jumps
  if(sum(cjumps) > 0)
  {
    c_temp <- clus$clusters
    rjj <- numeric(max(clus$clusters))
    for(j in 1:max(clus$clusters))
    {  
      rjj[j] <- median(cjumps[c_temp == j])
    }
  }
  else 
  {
    rjj <- 0
    c_temp <- 0  
  }
  list(candidates = cands, changepoints = changepoints, rjumps = rjj)
}

