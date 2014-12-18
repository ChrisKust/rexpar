follow_ups<-function(dat,mincper=0.75,steps=1,mincp=1)
{
  i<-1
  indi<-numeric(length(dat))
  indi[1]<-1
  dd<-diff(dat)
  if(length(dat)>1)
  {
    for(j in 2:length(dat))
    {
      if(dd[j-1]==steps)
      {
        indi[j]<-i
      }
      else if(dd[j-1]>steps)
      {
        i<-i+1
        indi[j]<-i
      }
      
    }
  }
  
  
  jps<-dat
  minc<-sort(table(indi),decreasing=T)[mincp]*mincper
  
  for(j in 1:i)
  {
    if(sum(indi==j)<minc)
    {
      jps<-jps[indi!=j]
      indi<-indi[indi!=j]
    }
  }
  list(clusters=indi,jumps=jps)
}