muVec <- function(t, y)
{
  MVec <- numeric(length(t))
  for(i in 1:length(t))
  {
    MVec[i] <- y * integrate(find1, 0, 1, t[i])$value
  }
  return(MVec)
}