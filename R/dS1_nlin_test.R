dS1_nlin_test<-function(thetaN,alpha,y,exact=F)
{
  dS1<-rexpar::dS1_lin2(theta=thetaN, y=y, model="nlinAR1")
  NdS1<-sqrt(floor((length(y)-1)/3))*(dS1-1/4)/sqrt(3/16)
  if(exact==T)
  {
    deci<-((dS1*floor((length(y)-1)/3))<qbinom(alpha,size=floor((length(y)-1)/3),prob=1/4))
  }
  else
  {
    deci<-(NdS1<qnorm(alpha))
  } 
  list(TS=NdS1,phi=deci)
}