lin2_CI <- function(y, level, plots = FALSE, notion = "dS1", ncoresC = 1, mid = FALSE, eps = 1e-26)
{
  if(!mid)
  {  
  cands<-lin2_theta_f(y)
  cands0<-cbind(cands$t1,cands$t2)+eps
  cands1<-cbind(cands$t1,cands$t2)-eps
  #cands1<-cbind(cands$t1,cands$t2)+0.0000000001
  #cands2<-cbind(cands$t1,cands$t2)+0.0000000001
  #cands3<-cbind(cands$t1,cands$t2)-0.0000000001
  #cands4<-cbind(cands$t1,cands$t2)-0.0000000001
  #cands5<-cbind(cands$t1+0.0000000001,cands$t2-0.0000000001)
  #cands6<-cbind(cands$t1-0.0000000001,cands$t2+0.0000000001)
 #cands<-rbind(cands0,cands1,cands2,cands3,cands4,cands5,cands6)
 cands <- rbind(cands0, cands1) 
  }
  if(mid)
  {
    cands <- Tri_Mid(y)
  }
 
  TS <- switch(notion, 
    "dS1" =
    {
      unlist(apply(cands, 1, dS1_lin2_test, y = y, alpha = (1 - level)))
    },
    "dS2" =
    {
      unlist(apply(cands, 1, dS2_lin2_test, y = y, alpha = (1 - level)))
    },
    "dS3" =
    {
      unlist(apply(cands, 1, dS3_lin2_test, y = y, alpha = (1 - level)))
    },
    "dS_pre" =
    {
    if(is.numeric(ncoresC) && ncoresC > 1 && ncoresC <= parallel::detectCores() * 2)
    {
      cl <- makeCluster(ncoresC)      
      TS_temp <- unlist(parApply(cl, cands, 1, dS1_lin2_test, y = y, alpha = (1 - level)))
      inCIs_temp <- as.vector(TS_temp[seq(2, length(TS_temp), 2)])
      cands <- cands[inCIs_temp == 0, ]
      return(unlist(parApply(cl, cands, 1, dS_lin2_test, y = y, alpha = (1 - level), ncores = 1)))
      stopCluster(cl)
    }
    if(ncoresC == 1)
    {
      TS_temp <- unlist(apply(cands, 1, dS1_lin2_test, y = y, alpha = (1 - level)))
      inCIs_temp <- as.vector(TS_temp[seq(2, length(TS_temp), 2)])
      cands <- cands[inCIs_temp == 0, ]
      unlist(apply(cands, 1, dS_lin2_test, y = y, alpha = (1 - level), ncores = 1))
    }
    },
    "dS" = 
    {
      if(is.numeric(ncoresC) && ncoresC > 1 && ncoresC <= detectCores() * 2)
      {
        cl <- makeCluster(ncoresC)
        return(unlist(parApply(cl, cands, 1, dS_lin2_test, y = y, alpha = (1 - level), ncores = 1)))
        stopCluster(cl)
      }
      if(ncoresC==1)
      {
      unlist(apply(cands, 1, dS_lin2_test, y = y, alpha = (1 - level), ncores = 1))
      }
      },
    stop("Insert a valid notion!")
  )
  inCIs <- as.vector(TS[seq(2, length(TS), 2)])
  
  if(plots)
  {
    a <- which(inCIs == 0)
    plot(cands)
    points(cands[a, ], col = 2)
    convex_hull_plot(cands[a, 1], cands[a, 2], col = 2)
  }
  list(par = cands, inCI = inCIs)
} 