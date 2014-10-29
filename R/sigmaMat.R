sigmaMat<-function(t,nclust=1)            
{
  
  if(is.numeric(nclust)==F)
  {
   stop("Number of clusters has to be an integer") 
  }
  
  SMat<-matrix(ncol=length(t),nrow=length(t))
  
  if((is.numeric(nclust)==T) && (nclust==1))
  {  
  for(i in 1:length(t))
  {
    for(j in 1:length(t))
    {
      SMat[i,j]<-integrate(find2,0,1,c(t[i],t[j]))$value-integrate(find1,0,1,t[i])$value*integrate(find1,0,1,t[j])$value
    }
  }
  }
  if((is.numeric(nclust)==T) && (nclust>1) && (nclust<=detectCores()))
  {
  cl<-makeCluster(nclust)
  SMat<-matrix(ncol=length(t),nrow=length(t))
  for(i in 1:length(t))
  {
    SMat[i,]<-unlist(parLapply(cl,t,intfun2,t1=t[i]))
  }
  }
 
  return(SMat)
}