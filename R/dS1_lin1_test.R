dS1_lin1_test<-function(thetaN,alpha,y)
{
  dS1<-rexpar::dS1_lin2(thetaN,y,model="linAR1woI")
  NdS1<-sqrt(floor((length(y)-1)/2))*(dS1-1/2)/sqrt(1/4)
  deci<-(NdS1<qnorm(alpha))
  list(TS=NdS1,phi=deci)
}