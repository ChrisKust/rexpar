dS_lin2_test<-function(thetaN,alpha,y,ncores=1)
{
  dS<-rexpar::dS_lin2(thetaN,y,ncores)
  CdS<-(length(y)-1)*(dS-1/4)
  deci<-(CdS<SimQuants[SimQuants[,1]==round((1-alpha),digits=3),2])
  list(TS=CdS,phi=deci)
}