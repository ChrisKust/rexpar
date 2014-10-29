RandomARMod_nlin1<-function(nobs,arp,power=1,start=0,cont=0)
{
  y<-numeric(nobs)
  y[1]<-start
  if(cont==0)
  {
    for(i in 2:length(y))
      y[i]<-y[i-1]+arp*y[i-1]^power+0.1*rnorm(1)
  }
  
  if(cont==1)
  {
    u1<-rnorm(nobs)*0.1
    u2<-(5+rnorm(nobs)*1)
    p1<-rpois(nobs,5/100)
    e<-u1+p1*u2
    for(i in 2:length(y))
      y[i]<-y[i-1]+arp*y[i-1]^power+e[i]- 0.007127848 # +0.01 #median korrektur und verfälschung!
    
  }
  
  if(cont==2)
  {
    alpha<-10
    beta<--3.661513
    u<-runif(nobs)
    e<-beta-alpha*log((-log(u)))
    for(i in 2:length(y))
      y[i]<-y[i-1]+arp*y[i-1]^power+e[i]
    
  }
  
  if(cont==3)
  {
    alpha<-1.928
    beta<--2
    gamma<-10
    u<-runif(nobs)
    e<-beta+alpha*(-log(u))^(-1/gamma)
    for(i in 2:length(y))
      y[i]<-y[i-1]+arp*y[i-1]^power+e[i]
    
  }
  
  if(cont==4)
  {
    alpha<-1
    beta<-1
    e<-rgamma(nobs,alpha,beta)-0.6932
    for(i in 2:length(y))
      y[i]<-y[i-1]+arp*y[i-1]^power+e[i]
    
  }
  
  return(y)
}
