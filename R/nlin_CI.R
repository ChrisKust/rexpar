nlin_CI<-function(y,level,plots=FALSE,notion="dS1",ncoresC=1,addPar=FALSE,spar=0.8)
{
  cands<-nlin1_theta_f(y)
  cands0<-cbind(cands$t1,cands$t2)
  #cands1<-cbind(cands$t1,cands$t2)+0.0000000001
  #cands2<-cbind(cands$t1,cands$t2)+0.0000000001
  #cands3<-cbind(cands$t1,cands$t2)-0.0000000001
  #cands4<-cbind(cands$t1,cands$t2)-0.0000000001
  #cands5<-cbind(cands$t1+0.0000000001,cands$t2-0.0000000001)
  #cands6<-cbind(cands$t1-0.0000000001,cands$t2+0.0000000001)
  #cands<-rbind(cands0,cands1,cands2,cands3,cands4,cands5,cands6)
  cands<-cands0
  
  if(notion=="dS1")
  {
  unlist(apply(cands,1,dS1_nlin_test,y=y,alpha=(1-level)))->TS
  }
  if(notion=="dS2")
  {
    unlist(apply(cands,1,dS2_nlin_test,y=y,alpha=(1-level)))->TS
  }
  if(notion=="dS3")
  {
    unlist(apply(cands,1,dS3_nlin_test,y=y,alpha=(1-level)))->TS
  }
  if(notion=="dS_pre")
  {
    if(is.numeric(ncoresC)==T && ncoresC>1 && ncoresC <= parallel::detectCores()*2)
    {
      cl<-makeCluster(ncoresC)#,type="MPI")
      #clusterEvalQ(cl,{library(matrixcalc)})
      #clusterEvalQ(cl,{library(parallel)})
      #clusterEvalQ(cl,{library(rexpar)})
      #clusterEvalQ(cl,{source("R/dS_lin2.R")})
      #,lib.loc="~/CTest/ChangeTests/locL/")})
      
     unlist(parApply(cl,cands,1,dS1_nlin_test,y=y,alpha=(1-level)))->TS_temp
     inCIs_temp<-as.vector(TS_temp[seq(2,length(TS_temp),2)])
     cands<-cands[inCIs_temp==0,]
     unlist(parApply(cl,cands,1,dS_nlin_test,y=y,alpha=(1-level),ncores=1))->TS
     stopCluster(cl)
    }
    
    
    if(ncoresC==1)
    {
      unlist(apply(cands,1,dS1_nlin_test,y=y,alpha=(1-level)))->TS_temp
      inCIs_temp<-as.vector(TS_temp[seq(2,length(TS_temp),2)])
      cands<-cands[inCIs_temp==0,]
      unlist(apply(cands,1,dS_nlin_test,y=y,alpha=(1-level),ncores=1))->TS
    }
  }
  if(notion=="dS")
  {
    if(is.numeric(ncoresC)==T && ncoresC>1 && ncoresC <= detectCores()*2)
    {
      cl<-makeCluster(ncoresC)#,type="MPI")
      #clusterEvalQ(cl,{library(matrixcalc)})
      #clusterEvalQ(cl,{library(parallel)})
      #clusterEvalQ(cl,{library(rexpar)})
      #clusterEvalQ(cl,{source("R/dS_lin2.R")})
      #,lib.loc="~/CTest/ChangeTests/locL/")})
      
      unlist(parApply(cl,cands,1,dS_nlin_test,y=y,alpha=(1-level),ncores=1))->TS
      stopCluster(cl)
    }
    
    
    if(ncoresC==1)
    {
      unlist(apply(cands,1,dS_nlin_test,y=y,alpha=(1-level),ncores=1))->TS
    }
  }
  inCIs<-as.vector(TS[seq(2,length(TS),2)])
  
  if(plots==TRUE)
  {
    a<-which(inCIs==0)
    #plot(cands)
    points(cands[a,],col=2)
    ashape(cands[a,1],cands[a,2],alpha=spar)->ah
    plot(ah,add=addPar,col=3)
  }
  list(par=cands,inCI=inCIs)
}