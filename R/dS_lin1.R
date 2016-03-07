dS_lin1 <- function(theta, y , mod = FALSE)
{
  y1 <- y[2:length(y)]
  y0 <- y[1:(length(y) - 1)]
  if(!mod)
  {
  resy <- (y1 - theta * y0)
  }
  if(mod)
  {  
    resy <- (y1 - theta * y0) * y0
  }
  r1 <- (resy > 0)
  r2 <- (resy < 0)
  d <- sum(r1) * sum(r2) * 1 / choose(length(resy), 2)
  return(d)
}