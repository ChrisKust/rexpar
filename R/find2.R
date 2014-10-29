find2<-function(x,pars)
{
  Ret<-((x-pars[1])>-0.5)*((x-pars[1])<0.5)*((x-pars[2])>-0.5)*((x-pars[2])<0.5)
  return(Ret)
}