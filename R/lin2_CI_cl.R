lin2_CI_cl<-function(y,level,plots="off",notion="dS1",cluster)
{
  cands<-lin2_theta_f(y)
  cands<-cbind(cands$t1,cands$t2)
  if(notion=="dS1")
  {
  unlist(apply(cands,1,dS1_lin2_test,y=y,alpha=(1-level)))->TS
  }
  if(notion=="dS2")
  {
    unlist(apply(cands,1,dS2_lin2_test,y=y,alpha=(1-level)))->TS
  }
  if(notion=="dS3")
  {
    unlist(apply(cands,1,dS3_lin2_test,y=y,alpha=(1-level)))->TS
  }
  if(notion=="dS_pre")
  {
      cl<-cluster
     unlist(parApply(cl,cands,1,dS1_lin2_test,y=y,alpha=(1-level/2)))->TS_temp
     inCIs_temp<-as.vector(TS_temp[seq(2,length(TS_temp),2)])
     cands<-cands[inCIs_temp==0,]
     unlist(parApply(cl,cands,1,dS_lin2_test,y=y,alpha=level,ncores=1))->TS
    

  }
  if(notion=="dS")
  {

      cl<-cluster
      unlist(parApply(cl,cands,1,dS_lin2_test,y=y,alpha=level,ncores=1))->TS
    
    
  }
  inCIs<-as.vector(TS[seq(2,length(TS),2)])
  
  if(plots=="on")
  {
    a<-which(inCIs==0)
    plot(cands)
    points(cands[a,],col=2)
    convex_hull_plot(cands[a,1],cands[a,2],col=2)
  }
  list(par=cands,inCI=inCIs)
}