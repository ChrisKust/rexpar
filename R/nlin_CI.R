nlin_CI <- function(y, level, plots = FALSE, notion = "dS1", ncoresC = 1, addPar = FALSE, spar = 0.8, eps = 1e-19)
{
  cands <- nlin1_theta_f(y)
  cands0 <- cbind(cands$t1, cands$t2) + eps
  #cands1<-cbind(cands$t1,cands$t2)+eps
  #cands2<-cbind(cands$t1,cands$t2)+eps
  #cands3<-cbind(cands$t1,cands$t2)-eps
  #cands4<-cbind(cands$t1,cands$t2)-eps
  #cands5<-cbind(cands$t1+eps,cands$t2-eps)
  #cands6<-cbind(cands$t1-eps,cands$t2+eps)
  #cands<-rbind(cands0,cands1,cands2,cands3,cands4,cands5,cands6)
  cands <- cands0
  
  TS <- switch(notion, 
    "dS1" =
      {
        unlist(apply(cands, 1, dS1_nlin_test, y = y, alpha = (1 - level)))
      },
    "dS2" = 
      {
        unlist(apply(cands, 1, dS2_nlin_test, y = y, alpha = (1 - level)))
      },
    "dS3" =
      {
        unlist(apply(cands, 1, dS3_nlin_test, y = y, alpha = (1 - level)))
      },
    "dS_pre" =
      {
        if(is.numeric(ncoresC) && ncoresC > 1 && ncoresC <= parallel::detectCores() * 2)
        {
          cl <- makeCluster(ncoresC)
          TS_temp <- unlist(parApply(cl, cands, 1, dS1_nlin_test, y = y, alpha = (1 - level)))
          inCIs_temp <- as.vector(TS_temp[seq(2, length(TS_temp), 2)])
          cands <- cands[inCIs_temp == 0, ]
          return(unlist(parApply(cl, cands, 1, dS_nlin_test, y = y, alpha = (1 - level), ncores = 1)))
          stopCluster(cl)
        }
      if(ncoresC == 1)
        {
          TS_temp <- unlist(apply(cands, 1, dS1_nlin_test, y = y, alpha = (1 - level)))
          inCIs_temp <- as.vector(TS_temp[seq(2, length(TS_temp), 2)])
          cands <- cands[inCIs_temp == 0, ]
          unlist(apply(cands, 1, dS_nlin_test, y = y, alpha = (1 - level), ncores = 1))
        }
      },
    "dS" = 
      {
        if(is.numeric(ncoresC) && ncoresC > 1 && ncoresC <= detectCores() * 2)
        {
          cl <- makeCluster(ncoresC)
          return(unlist(parApply(cl, cands, 1, dS_nlin_test, y = y, alpha = (1 - level), ncores = 1)))
          stopCluster(cl)
        }
        if(ncoresC == 1)
        {
          unlist(apply(cands, 1, dS_nlin_test, y = y, alpha = (1 - level), ncores = 1))
        }
      },
    stop("Insert a valid notion!")
  )
  
  inCIs <- as.vector(TS[seq(2, length(TS), 2)])
  
  if(plots && requireNamespace("alphahull"))
  {
    requireNamespace("alphahull")
    a <- which(inCIs == 0)
    points(cands[a, ], col = 2)
    ah <- alphahull::ashape(cands[a, 1], cands[a, 2], alpha = spar)
    plot(ah, add = addPar, col = 3)
  }
  list(par = cands, inCI = inCIs)
}