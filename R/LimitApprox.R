LimitApprox<-function(g,Y)
{
gs<-g
n<-length(g)

resdist<-3/4+3/4*Y$Y2[which(gs==0)]^2-3/4*sum(Y$Y1[(1:(n-1))]^2+Y$Y1[(2:n)]^2)*(abs(max(gs)-min(gs))/length(gs))

return(resdist)
}