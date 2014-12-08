predict_lin1<-function(y,CritLen,CritTime,NSim,alpha,restrict=F)
{
  cands<-lin1_theta_eps(y,0.000000001)$t1
  unlist(lapply(cands,rexpar::dS_lin1,y=y))->depth
  #plot(cands,depth)
  if(restrict==T)
  {
    theta_min<-min(cands[(length(y)-1)*(depth-1/2)>=-1/2*qchisq(1-alpha,df=1)+1/2])
    theta_max<-max(cands[(length(y)-1)*(depth-1/2)>=-1/2*qchisq(1-alpha,df=1)+1/2])
  }
  if(restrict==F)
  {
    theta_min<-min(cands)
    theta_max<-max(cands)
  }  
  abline(v=c(theta_min,theta_max),lty=2,col=2)
  ResMat<-matrix(nrow=(length(y)+3*length(y)),ncol=NSim)
  for(i in 1:NSim)
  {
    y_t<-numeric(length(y)+3*length(y))
    y_t[length(y)]<-y[length(y)]
    theta_t<-draw_from_depth(depthI=depth,testvec=cands,lower=theta_min,upper=theta_max)
    y_o<-y[1:(length(y)-1)]
    y_p<-y[2:length(y)]
    Res<-y_p-theta_t*y_o
    Res_t<-sample(Res,3*length(y),replace=T)
    
    for(j in (length(y)+1):(length(y_t)))
    {
      y_t[j]<-theta_t*y_t[j-1]+Res_t[j-length(y)]
    }
    ResMat[,i]<-y_t
    
    
  }
  
  ## Calculation of Interval, Mean and Median estimates
  mean_est_CT<-mean(ResMat[CritTime,])
  median_est_CT<-median(ResMat[CritTime,])
  CI_CT<-quantile(ResMat[CritTime,],prob=c(alpha/2,1-alpha/2))
  TimeInd<-seq(1,length(y_t))
  apply(ResMat,2,function(x) min(TimeInd[x>=CritLen]))->T_est
  mean_est_CL<-mean(T_est)
  median_est_CL<-median(T_est)
  CI_CL<-quantile(T_est,prob=c(alpha/2,1-alpha/2))
  
  list(estimation_time=CritTime,mean_CT=mean_est_CT,med_CT=median_est_CT,confintCT=CI_CT,alpha=alpha,estimation_lenght=CritLen,mean_CL=mean_est_CL,med_CL=median_est_CL,confintCL=CI_CL,simulations=ResMat)
}







#predict_lin1(y,alpha=0.05,restric=F,NSim=100)->rr