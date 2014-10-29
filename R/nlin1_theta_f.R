nlin1_theta_f<-function(dat)
{
  
  dat1<-dat[1:(length(dat)-1)]
  dat2<-dat[2:length(dat)]
  ldat1<-log(dat1)
  ldat2<-log(dat2)
  
  matrix(c(rep(ldat2[1:length(ldat2)],(length(ldat2)))),ncol=length(ldat2))-matrix(c(rep(ldat2[1:length(ldat2)],(length(ldat2)))),ncol=length(ldat2),byrow=T)->M1
  matrix(c(rep(ldat1[1:length(ldat2)],(length(ldat2)))),ncol=length(ldat1))-matrix(c(rep(ldat1[1:length(ldat2)],(length(ldat1)))),ncol=length(ldat1),byrow=T)->M2
  M1/M2->Mt1
  matrix(c(rep(dat2[1:length(dat2)],(length(dat2)))),ncol=length(dat2),byrow=T)*matrix(c(rep(dat2[1:length(dat2)],(length(dat2)))),ncol=length(dat2),byrow=T)^((-1)*M1/M2)->Mt2a
  matrix(c(rep(dat2[1:length(dat2)],(length(dat2)))),ncol=length(dat2),byrow=T)*matrix(c(rep(dat1[1:length(dat1)],(length(dat1)))),ncol=length(dat2),byrow=T)^((-1)*M1/M2)->Mt2b
  Mt2<-Mt2a-Mt2b
  #(matrix(c(rep(dat2[1:length(dat2)],(length(dat2)))),ncol=length(dat2),byrow=T)-matrix(c(rep(dat1[1:length(dat2)],(length(dat1)))),ncol=length(dat1),byrow=T))/(matrix(c(rep(dat2[1:length(dat2)],(length(dat2)))),ncol=length(dat2),byrow=T)^(Mt1))->Mt2
  t1=c(lower.tri(Mt1)*Mt1)
  t2=c(lower.tri(Mt2)*Mt2)
  t2=t2[is.na(t1)==F]
  t1=t1[is.na(t1)==F]
  t1=t1[is.na(t2)==F]
  t2=t2[is.na(t2)==F]
  t2=t2[t1!=0]
  t1=t1[t1!=0]
  t1=t1[t2!=0]
  t2=t2[t2!=0]
  
  list(t1=-t2, t2=t1)
}
