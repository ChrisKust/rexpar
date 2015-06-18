est_nlin1<-function(y,maxit=25,candy=FALSE,perc=0.1,acc=0.1,plots=FALSE,normtype=1,pv=2,wgt=c(1,0.5),unique=TRUE,notion=dS3_lin2,optim_rude=FALSE)
{
  LTs<-Tri_Mid_n1(y,perc,candy)
  LT<-nlin1_theta_f(y)
  sv<-c(median(LT$t1),median(LT$t2))
  #sv<-c(0,0)
  if(optim_rude==TRUE)
  {
    print(sv)
    optim(c(sv[1],sv[2]),fn=notion,model="nlinAR1",y=y,control=list(fnscale=-1))->sol
    se<-sol$par
    ev<-sol$value
    itc<-sol$counts
  }
  else
  {
  dists<-apply(LTs,1,Ele_Norm,center=sv,nortype=normtype,pv=pv,wgt=wgt)
  cands1<-LTs[,2][dists<quantile(dists,acc)]
  cands2<-LTs[,1][dists<quantile(dists,acc)]
  cands<-cbind(cands1,cands2)
  
  apply(cands,1,notion,y=y,model="nlinAR1")->ev
  se<-cbind(cands[ev==max(ev),1],cands[ev==max(ev),2])
  if(plots==TRUE)
  {	plot(LT$t1,LT$t2,xlim=c(-10,10),ylim=c(-10,10))
    points(cands,pch=19,col=2)
    points(se,col=3,pch=19)
  }
  itc<-1
  for(i in 1:maxit)
  {
    itc<-itc+1
    
    sv<-se
    dists<-apply(LTs,1,Ele_Norm,center=c(sv[1,1],sv[1,2]),nortype=normtype,pv=pv,wgt=wgt)
    cands1<-LTs[,1][dists<quantile(dists,acc)]
    cands2<-LTs[,2][dists<quantile(dists,acc)]
    cands<-cbind(cands1,cands2)
    apply(cands,1,notion,y=y,model="nlinAR1")->ev
    se<-cbind(cands[ev==max(ev),1],cands[ev==max(ev),2])
    if(plots==TRUE)
    {
      points(cands,pch=19,col=itc)
      points(se,pch=1,col=(itc+1))
    }
    #print(se)
    #print(sv)
    #print(max(ev))	
    if(sv[1]==se[1] && sv[2]==se[2])
    {	
      break
    }
  }
  max(ev)
  #a<-proc.time()-ptm
  #print(a)
  if(unique==TRUE)
  {
    se<-c(mean(se[,1]),mean(se[,2]))
  }
  }
  
  list(estimate=se,value=max(ev),numit=itc)
}


