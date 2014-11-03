lin1_theta_eps<-function(dat,eps)
{
  dat1<-dat[1:(length(dat)-1)]
  dat2<-dat[2:length(dat)]
  t1<-0
  t2<-0
  cand_ttest<-numeric(length(dat1))
  cand_t1<-numeric(2*length(dat1))
  for(i in 1:(length(dat1)))
  {
    cand_ttest[i]<-(dat2[i]-dat1[i])/(dat1[i])#-(dat2[i]-dat1[i])/(dat1[i]))/2
    cand_t1[i]<-(dat2[i]-dat1[i])/(dat1[i])+eps#-(dat2[i]-dat1[i])/(dat1[i]))/2
    cand_t1[(i+length(dat1))]<-(dat2[i]-dat1[i])/(dat1[i])-eps#-(dat2[i]-dat1[i])/(dat1[i]))/2
  }
  
  t1<-cand_t1+1
  if(abs(min(diff(sort(cand_ttest))))<eps)
    print('eps to large')
  
  list(t1=t1)
}