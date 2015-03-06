dS_lin1_test<-function(thetaN,alpha,y)
{
  dS<-rexpar::dS_lin1(thetaN,y)
  CdS<-(length(y)-1)*(dS-1/2)
  deci<-(CdS<1/2-1/2*qchisq((1-alpha),1)) ## important remark alpha = 0.05 üblich=> level, d.h. in nur 5 % wird h0 falsch verworfen!!
  list(TS=CdS,phi=deci)
}