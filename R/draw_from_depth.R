draw_from_depth<-function(depthI,testvec,lower,upper)
{
  dd<-depthI[(testvec>=lower) & (testvec <= upper)]
  cands<-testvec[(testvec>=lower) & (testvec <= upper)]
  u<-runif(1)
  cands_sI<-sort(cands,index.return=T)
  depth_s<-dd[cands_sI$ix]
  cands_s<-cands_sI$x
  int<- sum((cands_s[2:length(cands_s)]-cands_s[1:(length(cands_s)-1)])*((depth_s[2:length(depth_s)]+depth_s[1:(length(depth_s)-1)])/2))
  d2<-dd/int
  d2_s<-d2[cands_sI$ix]*(c(0,cands_s[2:length(cands_s)]-cands_s[1:(length(cands_s)-1)]))
  ind<-seq(1,length(cands_s),1)
  theta_t<-cands_s[min(ind[cumsum(d2_s)>=u])]
  return(theta_t)
}