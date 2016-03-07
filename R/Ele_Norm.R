Ele_Norm <- function(Cvec, center, pv = 2, nortype = 1, wgt = c(1, 0.5))
{
  n <- switch(nortype,
    "1" =
      {
      (sum(abs(Cvec-center)^pv))^(1/pv)
      },
    "2" =
      {
      (sum(wgt*(Cvec-center)^pv))^(1/pv)
      },
    stop("Enter a valid type of norm!")
  )
  return(n)
}