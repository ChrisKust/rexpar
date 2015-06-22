convex_hull_plot<-function(x,y,col=1,lwd=1)
{
  #Ref: http://chitchatr.wordpress.com/2011/12/30/convex-hull-around-scatter-plot-in-r/
  p1<-chull(x=x,y=y)
  p1<-c(p1,p1[1])
  lines(x[p1],y[p1],col=col,lwd=lwd)
}