dS2_lin1_test<-function(thetaN,alpha,y)
{
  dS2<-rexpar::dS2_lin2(thetaN,y,model="linAR1woI")
  NdS2<-NdS2<-sqrt(floor(length(y)/2))*(dS2-1/2)/sqrt(1/4)
  deci<-(NdS2<qnorm(alpha))
  list(TS=NdS2,phi=deci)
}