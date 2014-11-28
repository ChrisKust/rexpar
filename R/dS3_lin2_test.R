dS3_lin2_test<-function(thetaN,alpha,y)
{
  dS3<-rexpar::dS3_lin2(thetaN,y)
  NdS3<-sqrt((length(y)-1)-2)*(dS3-1/4)/sqrt(5/16)
  deci<-(NdS3<qnorm(alpha))
  list(TS=NdS3,phi=deci)
}