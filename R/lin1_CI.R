lin1_CI<-function(y,level,plots=FALSE)
{
  cands<-lin1_theta(y)$t1

  unlist(lapply(cands,dS_lin1_test,y=y,alpha=(1-level)))->TS
  inCIs<-as.vector(TS[seq(2,length(TS),2)])
  
  if(plots==TRUE)
  {
    a<-which(inCIs==0)
    plot(cands,TS)
  }
  list(par=cands,inCI=inCIs)
}