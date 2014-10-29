intfun2<-function(t2,t1)
{
  r<-integrate(find2,0,1,c(t1,t2))$value-integrate(find1,0,1,t1)$value*integrate(find1,0,1,t2)$value
  return(r)  
}