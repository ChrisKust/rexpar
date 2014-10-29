dS_lin2_loop<-function(theta,y)
{
  sd<-0
  y1<-y[2:length(y)]
  y0<-y[1:(length(y)-1)]
  resy<-y1-theta[1]-theta[2]*y0
  for(ki in 1:(length(resy)-2))
  {
    for(kii in ki:(length(resy)-1))
    {
      for(kiii in kii:(length(resy)))
      {
        sd<-sd+(resy[ki]<0)*(resy[kii]>0)*(resy[kiii]<0)+(resy[ki]>0)*(resy[kii]<0)*(resy[kiii]>0)
      }
    }
    
  }
  sd<-1/choose(length(resy),3)*sd
  return(sd)
}