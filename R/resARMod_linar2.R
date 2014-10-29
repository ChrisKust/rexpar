resARMod_linar2<-function(theta,dat)
{
  dat1<-dat[1:(length(dat)-2)]
  dat2<-dat[2:(length(dat)-1)]
  dat3<-dat[3:length(dat)]
  res<-dat3-theta[1]*dat2-theta[2]*dat1
  return(res)
}