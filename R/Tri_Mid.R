Tri_Mid<-function(y,perc=1,candy=FALSE)
{
  LT<-lin2_theta_f(y)
  ll<-cbind(LT$t1,LT$t2)
  l1l<-quantile(LT$t1,0.5-perc/2)
  l1u<-quantile(LT$t1,0.5+perc/2)
  l2l<-quantile(LT$t2,0.5-perc/2)
  l2u<-quantile(LT$t2,0.5+perc/2)
  which(ll[,1]<l1u & ll[,1]>l1l)->il1
  ll<-ll[il1,]
  which(ll[,2]<l2u & ll[,2]>l2l)->il2
  ll<-ll[il2,]
  d1<-ll[,1]
  d2<-ll[,2]
  if(candy==TRUE)
  {#generate indices
    iX<-seq(1,length(d1),1)
    p1<-combn(seq(1,length(d1),1),m=3)
    apply(p1,2,mind,Mat=d1)->v1
    apply(p1,2,mind,Mat=d2)->v2
    sol<-cbind(v1,v2)
  }
  if(candy==FALSE)
  {
    sol<-ll
  }
  return(sol)
}