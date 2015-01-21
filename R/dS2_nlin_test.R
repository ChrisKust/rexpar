dS2_nlin_test<-function(thetaN,alpha,y)
{
  dS2<-rexpar::dS2_lin2(thetaN,y,model="nlinAR1")
  NdS2<-sqrt(floor((length(y)-1)/2)-1)*(dS2-1/4)/sqrt(3/16)
  deci<-(NdS2<qnorm(alpha))
  list(TS=NdS2,phi=deci)
}