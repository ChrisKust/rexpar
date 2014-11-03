lin1_theta<-function(dat)
{
  dat1<-dat[1:(length(dat)-1)]
  dat2<-dat[2:length(dat)]
  t1<-0
  t2<-0
  cand_t1<-numeric(length(dat1))
  for(i in 1:(length(dat1)))
  {
    cand_t1[i]<-(dat2[i]-dat1[i])/(dat1[i])#-(dat2[i]-dat1[i])/(dat1[i]))/2
  }
  cand_t1S<-sort(cand_t1)
  for(i in 1:(length(cand_t1S)-1))
  {
    for(j in (i+1):length(cand_t1S))
    {
      t1<-c(t1,(cand_t1S[j]+cand_t1S[i])/2)
    }
  }
  
  t1<-c(t1)+1
  
  
  
  
  list(t1=t1)
}