RandomARMod_nlin2<-function(nobs,intercept=0,arp,power,start=0,cont=0)
{
  y<-numeric(nobs)
  y[1]<-start
  if(cont==0)
  {
    for(i in 2:length(y))
      y[i]<-y[i-1]+arp*y[i-1]^power+intercept+0.2*rnorm(1)
  }
  
  if(cont==1)
  {
    u1<-rnorm(nobs)*0.2
    u2<-(5+rnorm(nobs)*1)
    p1<-rpois(nobs,5/100)
    e<-u1+p1*u2
    for(i in 2:length(y))
      y[i]<-y[i-1]+arp*y[i-1]^power+intercept+e[i]- 0.0127892 #(für N(0,0.2)+P(5/100)*N(5,1)) # +0.01 #median korrektur und verfälschung!
    
  }
  
  if(cont==2)
  {
    alpha<-10
    beta<--3.665129
    u<-runif(nobs)
    e<-beta-alpha*log((-log(u)))
    for(i in 2:length(y))
      y[i]<-y[i-1]+arp*y[i-1]^power+intercept+e[i]
    
  }
  
  if(cont==3)
  {
    alpha<-1.928
    beta<--2
    gamma<-10
    u<-runif(nobs)
    e<-beta+alpha*(-log(u))^(-1/gamma)
    for(i in 2:length(y))
      y[i]<-y[i-1]+arp*y[i-1]^power+intercept+e[i]
    
  }
  
  if(cont==4)
  {
    alpha<-1
    beta<-1
    e<-rgamma(nobs,alpha,beta)-0.6932
    for(i in 2:length(y))
      y[i]<-y[i-1]+arp*y[i-1]^power+intercept+e[i]
    
  }
  
  if(cont==5)
  {
    u1<-rnorm(nobs)*1
    u2<-150#+rnorm(nobs)*1)
    p1<-rpois(nobs,2/200)
    e<-u1+p1*u2
    for(i in 2:length(y))
      y[i]<-y[i-1]+arp*y[i-1]^power+intercept+e[i]#- 0.007127848 # +0.01 #median korrektur und verfälschung!
    
  }
  
  
  if(cont==6)
  {
    alpha<-100
    beta<--36.65129
    u<-runif(nobs)
    e<-beta-alpha*log((-log(u)))
    for(i in 2:length(y))
      y[i]<-y[i-1]+arp*y[i-1]^power+intercept+e[i]
    
  }
  
  
  if(cont==7)
  {
    for(i in 2:length(y))
      y[i]<-y[i-1]+arp*y[i-1]^power+intercept
  }
  
  return(y)
}