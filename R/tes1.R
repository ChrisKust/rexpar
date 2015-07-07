exact_lim_dS3lin0<-function(N,Reps,plot=FALSE)
{
  R<-Reps
  dSs<-numeric(R)
  for(i in 1:R)
  {
    res<-(rbinom((N-1),size=1,prob=1/2)-1/2)
    r1<-res[seq(1,length(res)-2,1)]
    r2<-res[seq(2,length(res)-2,1)]
    m<-min(c(length(r1),length(r2)))
    r1<-r1[1:m]
    r2<-r2[1:m]
    InD<-(r1>0)*(r2<0)+(r1<0)*(r2>0)+(1-(r1!=0)*(r2!=0))
    sum(InD)->dSs[i]
  }
  if(plot==TRUE)
  {
    par(mfrow=c(1,1))
    hist(dSs,plot=F,breaks=seq(min(dSs),max(dSs),1))->h
    plot(h$mids,h$counts/R,type="h",xlim=c(min(dSs),max(dSs)),ylim=c(0,0.25),xlab="value",ylab="rel. freq.")
    #h$counts/R
    rr<-rbinom(R,size=floor(N-2),prob=(1/4))
    floor(N/3)
    hist(rr,plot=F,breaks=seq(min(rr)-1,max(rr),1))->hh
    lines(hh$mids+0.4,hh$counts/R,type="h",col=2)
  }
  return(dSs)
}

exact_lim_dS3lin0(100,10000,plot=TRUE)