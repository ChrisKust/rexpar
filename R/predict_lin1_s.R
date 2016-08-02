predict_lin1_s <- function(y, CritLen, simL_fac = 30, CritTime, NSim, alpha, restrict = FALSE, start, eps = 1e-9)
{
  N <- length(y)
  if (missing(CritLen)) {
    simL_fac <- (CritTime - N)/N
  } else {
    simL_fac <- simL_fac
  }
  predLength <- ceiling(simL_fac * N)
  
  cands <- lin1_theta_eps(y,eps)$t1
  depth <- unlist(lapply(cands, rexpar::dS_lin1, y = y))
  if(restrict)
  {
    theta_min <- min(cands[(N - 1) * (depth - 1 / 2) >= - 1 / 2 * qchisq(1 - alpha, df = 1) + 1 / 2])
    theta_max <- max(cands[(N - 1) * (depth - 1 / 2) >= - 1 / 2 * qchisq(1 - alpha, df = 1) + 1 / 2])
  }
  if(!restrict)
  {
    theta_min <- min(cands)
    theta_max <- max(cands)
  }  
  ResMat <- matrix(nrow = (N + predLength), ncol = NSim)
  for(i in 1:NSim)
  {
    y_t <- numeric(N + predLength)
    y_t[N] <- start
    theta_t <- draw_from_depth(depthI = depth, testvec = cands, lower = theta_min, upper = theta_max)
    y_o <- y[1:(N - 1)]
    y_p <- y[2:N]
    Res <- y_p - theta_t * y_o
    Res_t <- sample(Res, predLength, replace = TRUE)
    
    for(j in (N + 1):(length(y_t)))
    {
      y_t[j] <- theta_t * y_t[j - 1] + Res_t[j - N]
    }
    ResMat[, i] <- y_t
    
    
  }
  
  ## Calculation of Interval, Mean and Median estimates
  mean_est_CT <- mean(ResMat[CritTime, ])
  median_est_CT <- median(ResMat[CritTime, ])
  CI_CT <- quantile(ResMat[CritTime, ], prob = c(alpha / 2, 1 - alpha / 2))
  if (!missing(CritLen)) {
    TimeInd <- seq(1, length(y_t))
    T_est <- apply(ResMat, 2, function(x) {
      tmp <- TimeInd[x >= CritLen]
      if(length(tmp) > 0) {
        return(min(TimeInd[x >= CritLen]))
      } else {
        return(NA)
      }
    })
    warning(paste0(sum(is.na(T_est)), " out of ", NSim, " simulations did not reach the critical length ", round(CritLen,2), 
                   ". \n You might consider a value for simL_fac larger than ", simL_fac, "."))
    mean_est_CL <- mean(T_est, na.rm = TRUE)
    median_est_CL <- median(T_est, na.rm = TRUE)
    CI_CL <- quantile(T_est, prob = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
    return(list(estimation_time = CritTime, mean_CT = mean_est_CT, med_CT = median_est_CT, 
                confintCT = CI_CT, alpha = alpha, estimation_lenght = CritLen, mean_CL = mean_est_CL, 
                med_CL = median_est_CL, confintCL = CI_CL, simulations = ResMat))
  } else {
    return(list(estimation_time = CritTime, mean_CT = mean_est_CT, med_CT = median_est_CT, confintCT = CI_CT, 
                alpha = alpha, simulations = ResMat))
  }
  
  
}




