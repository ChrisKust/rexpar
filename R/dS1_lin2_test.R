dS1_lin2_test<-function(thetaN,alpha,y)
{
  dS1<-rexpar::dS1_lin2(thetaN,y)
  NdS1<-sqrt(floor((length(y)-1)/3))*(dS1-1/4)/sqrt(3/16)
  deci<-(NdS1<qnorm(alpha))
  list(TS=NdS1,phi=deci)
}