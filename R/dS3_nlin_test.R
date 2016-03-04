dS3_nlin_test<-function(thetaN,alpha,y,exact=FALSE)
{
  dS3<-rexpar::dS3_lin2(theta=thetaN,y=y,model="nlinAR1")
  NdS3<-sqrt((length(y)-1)-2)*(dS3-1/4)/sqrt(5/16)
  if(exact==TRUE)
  {
    q<-quantile(exact_lim_dS3lin(N=length(y),Reps=1000), prob=alpha)
    deci<-(((length(y)-1)-2)*dS3<q)
  }
  else
  {
    deci<-(NdS3<qnorm(alpha))
  }
  list(TS=NdS3,phi=deci)
}