est_lin1 <- function(y, maxit = 25, candy = FALSE, acc = 0.1, plots = FALSE, eps = 0.000001, unique = TRUE, notion = "dS")
{
  LT <- lin1_theta_eps(y, eps)$t1
  sv <- median(LT)
  dists <- sapply(LT, Ele_Norm, center = sv)
  cands <- LT[dists < quantile(dists, acc)]
  ev <- switch(notion, 
    "dS" =
      {
      sapply(cands,dS_lin1,y=y)
      },
    "dS1" =
      {
      sapply(cands,dS1_lin2,model="linAR1woI",y=y)
      },
    "dS2" =
      {
      sapply(cands,dS2_lin2,model="linAR1woI",y=y)
      },
    "dS3" =
      {
      sapply(cands,dS3_lin2,model="linAR1woI",y=y)
    },
    stop("Select a valid notion!")
  )
  
  se <- cands[ev == max(ev)]
  itc <- 1
  
  for(i in 1:maxit)
  {
    itc <- itc + 1
    sv <- se
    dists <- sapply(LT, Ele_Norm, center = sv)
    cands <- LT[dists < quantile(dists, acc)]
    ev <- switch(notion,
      "dS" =
        {
        sapply(cands,dS_lin1,y=y)
        },
      
      "dS1" =
        {
        sapply(cands,dS1_lin2,model="linAR1woI",y=y)
        },
      "dS2" =
        {
        sapply(cands,dS2_lin2,model="linAR1woI",y=y)
        },
      
      "dS3" =
        {
        sapply(cands,dS3_lin2,model="linAR1woI",y=y)
        },
      stop("Select a valid notion!")
    )
    se <- cands[ev == max(ev)]
    if(sv[1] == se[1])
    {break}
    
  }
  max(ev)
  if(unique)
  {
    se <- mean(se)
  }
  list(estimate = se, value = max(ev), numit = itc)
}