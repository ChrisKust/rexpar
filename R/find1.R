find1<-function(x,pars)
{
  Ret<-((x-pars)>-0.5)*((x-pars)<0.5)
  return(Ret)
}