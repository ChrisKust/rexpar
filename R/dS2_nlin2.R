dS2_nlin2 <- function(theta, y, resy)
{
    if (missing(resy)) {
      y1 <- y[-length(y)]
      y2 <- y[-1]
      resy <- (y2 - y1 - theta[3] - theta[1] * y1^theta[2])
    }
    r1 <- resy[seq(1, floor((length(resy) - 1) / 2), 2)]
    r2 <- resy[seq(2, floor((length(resy) - 1) / 2), 2)]
    r3 <- resy[seq((length(resy) - floor((length(resy) - 1) / 2) + 1), length(resy), 2)]
    r4 <- resy[seq((length(resy) - floor((length(resy) - 1) / 2)), length(resy), 2)]
    m <- min(c(length(r1), length(r2), length(r3)))
    r1 <- r1[1:m]
    r2 <- r2[1:m]
    r3 <- r3[m:1]
    r4 <- r4[m:1]
    InD <- (r1 > 0) * (r2 < 0) * (r3 > 0) * (r4 < 0) + (r1 < 0) * (r2 > 0) * (r3 < 0) * (r4 > 0) #+(1-(r1!=0)*(r2!=0)*(r3!=0)*(r4!=0))
    depth<-1 / (m) * sum(InD)
    return(depth)
}