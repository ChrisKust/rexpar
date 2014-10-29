dS_lin2_test<-function(thetaN,alpha,y)
{
  dS<-rexpar::dS_lin1(thetaN,y)
  CdS<-(length(y)-1)*(dS-1/2)
  deci<-(CdS<1/2-1/2*qchisq(1-alpha,1))
  list(TS=CdS,phi=deci)
}