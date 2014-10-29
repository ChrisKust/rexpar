resARMod_lin2<-function(theta,dat)
{
  dat1<-dat[1:(length(dat)-1)]
  dat2<-dat[2:length(dat)]
  res<-dat2-theta[2]*dat1-theta[1]
  return(res)
}