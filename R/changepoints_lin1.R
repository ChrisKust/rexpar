changepoints_lin1<-function(y,level,bw,sw,plots=FALSE,method=1,mincper=1,mincp=1)
{
  alpha<-(1-level)
  cands<-seq(bw+1,length(y)-bw,sw)
  changeind<-numeric(length(cands))

  if(method==1)
  {
    for(i in 1:length(cands))
    {
      lin1_CI(y[(cands[i]-bw):cands[i]],alpha,notion="dS")->ASp1
      lin1_CI(y[cands[i]:(cands[i]+bw)],alpha,notion="dS")->ASp2
      print(i)
    print(c(min(ASp1$par[ASp1$inCI==0]),max(ASp1$par[ASp1$inCI==0])))
    print(c(min(ASp2$par[ASp2$inCI==0]),max(ASp2$par[ASp2$inCI==0])))
    print("next")
    
          
     1-(max(ASp1$par[ASp1$inCI==0])>(min(ASp2$par[ASp2$inCI==0])))-(min(ASp1$par[ASp1$inCI==0])<(max(ASp2$par[ASp2$inCI==0])))->changeind[i]
    }
    
    changepoints<-numeric(length(changeind))
    changepoints[changeind==0]<-TRUE
    changepoints[changeind>0]<-FALSE
    totjumps<-cands[changeind==0]
  }
  
  if(method==2)
  {
    for(i in 1:length(cands))
    {
      est_lin1(y[(cands[i]-bw):cands[i]],notion="dS")$estimate->E1
      est_lin1(y[cands[i]:(cands[i]+bw)],notion="dS")$estimate->E2
      
      dS_lin1_test(E1,alpha,y[cands[i]:(cands[i]+bw)])$phi->T1
      dS_lin1_test(E2,alpha,y[(cands[i]-bw):cands[i]])$phi->T2
      changeind[i]<-T1*T2
    }
    changepoints<-numeric(length(changeind))
    changepoints[changeind==1]<-TRUE
    changepoints[changeind==0]<-FALSE
    totjumps<-cands[changeind==1]
  }
  
  clus<-follow_ups(totjumps,mincper=mincper,steps=sw,mincp=mincp)
  cjumps<-clus$jumps
  if(sum(cjumps)>0)
  {
    c_temp<-clus$clusters
    rjj<-numeric(max(clus$clusters))
    for(j in 1:max(clus$clusters))
    {  
      rjj[j]<-median(cjumps[c_temp==j])
    }
  }
  else 
  {
    rjj<-0
    c_temp<-0  
  }
  
  
  list(candidates=cands,changepoints=changepoints,rjumps=rjj)
}

