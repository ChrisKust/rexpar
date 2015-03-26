dS_lin2 <- function(theta, y, ncores = 1, model = c("linAR1", "nlinAR1", "linARc"), cpow = 1) {
  model <- match.arg(model)
  y1<-y[-1]
  y0<-y[-length(y)]
  if(model=="linAR1")
  {
  resy<-y1-theta[1]-theta[2]*y0
  }
  else if(model=="nlinAR1")
  {
  resy<-y1-y0-theta[1]*y0^theta[2]  
  }
  else if(model=="linARc")
  {
  resy <- y1 - theta[1] - theta[2] * y0^cpow
  }

  Matsd<-0 
  
  Reslist<-vector("list",length(resy)-3)
  for(j in 1:(length(resy)-3))
  {
    Reslist[[j]]<-resy[j:length(resy)]
  }
  
  if(ncores=="auto")
  {
    ncores<-parallel::detectCores()-1
  }
  
  if(is.numeric(ncores)==T && ncores>1 && ncores <= parallel::detectCores()*2)
  {
    cl<-makeCluster(ncores)
    #clusterEvalQ(cl,{library(matrixcalc)})
    #clusterEvalQ(cluster,{library(parallel)})
    
    sum(unlist(parLapply(cl,Reslist,oner)))->Matsd
    stopCluster(cl)
    
  }
  
  if(ncores==1)
  {
    sum(unlist(lapply(Reslist,oner)))->Matsd
  }
  
  if((is.numeric(ncores)==T)&& (ncores>parallel::detectCores()*2)) 
  {
    stop("Number of cores has to be lower than the available cores on the system")
  }
  
  if((is.numeric(ncores)==F) && (ncores!="auto"))
  {
    stop("Unknown expression for number of cores")
  }
  
  
  Matsd<-Matsd+(resy[length(resy)-2]>0)*(resy[length(resy)-1]<0)*(resy[length(resy)]>0)+(resy[length(resy)-2]<0)*(resy[length(resy)-1]>0)*(resy[length(resy)]<0)
  
  sd<-(1/choose(length(resy),3))*Matsd
  
  return(sd)
  
}