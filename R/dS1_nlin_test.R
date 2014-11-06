dS1_nlin_test<-function(thetaN,alpha,y)
{
  dS1<-rexpar::dS1_lin2(thetaN,y,model="nlinAR1")
  NdS1<-sqrt(floor(length(y)/3))*(dS1-1/4)/sqrt(3/16)
  deci<-(NdS1<qnorm(alpha))
  list(TS=NdS1,phi=deci)
}