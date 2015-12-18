RandomARMod_linar2<-function(nobs,arp1,arp2,start=c(0,0),cont=0)
{
  y<-numeric(nobs)
  y[1]<-start[1]
  y[2]<-start[2]
  if(cont==0)
  {
    for(i in 3:length(y))
      y[i]<-arp1*y[i-1]+arp2*y[i-2]+0.1*rnorm(1)
  }
  
  if(cont==1)
  {
    u1<-rnorm(nobs)*0.1
    u2<-(5+rnorm(nobs)*1)
    p1<-rpois(nobs,5/100)
    e<-u1+p1*u2
    for(i in 3:length(y))
      y[i]<-arp1*y[i-1]+arp2*y[i-2]+e[i]- 0.007127848 
    
  }
  
  if(cont==2)
  {
    alpha<-10
    beta<--3.665129
    u<-runif(nobs)
    e<-beta-alpha*log((-log(u)))
    for(i in 3:length(y))
      y[i]<-arp1*y[i-1]+arp2*y[i-2]+e[i]
    
  }
  
  if(cont==3)
  {
    alpha<-1.928
    beta<--2
    gamma<-10
    u<-runif(nobs)
    e<-beta+alpha*(-log(u))^(-1/gamma)
    for(i in 3:length(y))
      y[i]<-arp1*y[i-1]+arp2*y[i-2]+e[i]
    
  }
  
  if(cont==4)
  {
    alpha<-1
    beta<-1
    e<-rgamma(nobs,alpha,beta)-0.6932
    for(i in 3:length(y))
      y[i]<-arp1*y[i-1]+arp2*y[i-2]+e[i]
    
  }
  
  return(y)
}