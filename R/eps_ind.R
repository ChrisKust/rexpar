eps_ind<-function(Mat,iX,eps)
{
  i1<-c(iX[1],iX[2],iX[3])
  m1<-Mat[i1[1],]+eps*(((Mat[i1[2],]+Mat[i1[3],])/2)-Mat[i1[1],])
  m2<-Mat[i1[1],]-eps*(((Mat[i1[2],]+Mat[i1[3],])/2)-Mat[i1[1],])
  m<-rbind(m1,m2)
  return(m)
}