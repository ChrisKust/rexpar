dS3_lin1_test<-function(thetaN,alpha,y)
{
  dS3<-rexpar::dS3_lin2(thetaN,y,model="linAR1woI")
  NdS3<-sqrt((length(y)-1)-1)*(dS3-1/2)/sqrt(1/4)
  deci<-(NdS3<qnorm(alpha))
  list(TS=NdS3,phi=deci)
}