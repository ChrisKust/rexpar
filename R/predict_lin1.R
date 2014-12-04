predict_lin1<-function(y,CritLen,CritTime,NSim,alpha,restrict=F)
{
  cands<-lin1_theta_eps(y,0.000000001)$t1
  unlist(lapply(cands,rexpar::dS_lin1,y=y))->depth
  plot(cands,depth)
  if(restrict==T)
  {
    theta_min<-min(cands[(length(y)-1)*(depth-1/2)>=-1/2*qchisq(1-alpha,df=1)+1/2])
    print(theta_min)
    theta_max<-max(cands[(length(y)-1)*(depth-1/2)>=-1/2*qchisq(1-alpha,df=1)+1/2])
    print(theta_max)
  }
  if(restrict==F)
  {
    theta_min<-min(cands)
    theta_max<-max(cands)
  }  
  
  ResMat<-matrix(nrow=(length(y)+3*length(y)),ncol=NSim)
  for(i in 1:NSim)
  {
    y_t<-numeric(length(y)+3*length(y))
    y_t[length(y)]<-y[length(y)]
    theta_t<-draw_from_depth(depth,cands,theta_min,theta_max)
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
  
  abline(v=c(theta_min,theta_max),lty=2,col=2)
  return(ResMat)
}







#predict_lin1(y,alpha=0.05,restric=F,NSim=100)->rr