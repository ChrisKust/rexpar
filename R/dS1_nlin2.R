dS1_nlin2<-function(theta,y)
{
y1<-y[-length(y)]
y2<-y[-1]
res<-(y2-y1-theta[3]-theta[1]*y1^theta[2])

    r1<-res[seq(1,length(res),4)]
    r2<-res[seq(2,length(res),4)]
    r3<-res[seq(3,length(res),4)]
    r4<-res[seq(4,length(res),4)]
    m<-min(c(length(r1),length(r2),length(r3),length(r4)))
    r1<-r1[1:m]
    r2<-r2[1:m]
    r3<-r3[1:m]
    r4<-r4[1:m]
    InD<-(r1>0)*(r2<0)*(r3>0)*(r4<0)+(r1<0)*(r2>0)*(r3<0)*(r4>0)+(1-(r1!=0)*(r2!=0)*(r3!=0)*(r4!=0))
    #depth<-1/(floor((length(y))/3))*sum(InD)
    depth<-1/(m)*sum(InD)
  
  return(depth)
  
}