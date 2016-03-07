convex_hull_intersect <- function(points1, points2, alpha = 0.05, y1 = 0, y2 = 0, notion = "dS", plots = FALSE)
{
  p1 <- chull(points1[, 1], points1[, 2])
  p1 <- c(p1, p1[1])
  p2 <- chull(points2[, 1], points2[, 2])
  p2 <- c(p2, p2[1])
  if(plots)
  {
  plot(points1[p1, 1], points1[p1, 2])
  lines(points1[p1, 1], points1[p1, 2])
  points(points2[p2,1], points2[p2, 2], col = 2)
  lines(points2[p2, 1], points2[p2, 2], col = 2)
  }
  
  sumi <- 0
  for(i in 1:(length(p1) - 1))
  {
      for(k in 1:(length(p2) - 1))
      {

         ints <- straight_intersect(points1[p1[i], ], points1[p1[i + 1], ], points2[p2[k], ], points2[p2[k + 1], ], plots = plots)  
         sumi <- sumi + ints$res
      }
  }
  
  if(sumi == 0)
  {
    tt <- switch(notion,
                 "dS" = 
    {
      t1 <- dS_lin2_test(points1[1, ], alpha, y2, ncores = 1)
      t2 <- dS_lin2_test(points2[2, ], alpha, y1, ncores = 1)
      tt <- list(t1 = t1, t2 = t2)
    },
    
    "dS2" = 
    {
      t1 <- dS1_lin2_test(points1[1, ], alpha, y2)
      t2 <- dS1_lin2_test(points2[2, ], alpha, y1)
      tt <- list(t1 = t1, t2 = t2)
    },
    "dS2" = 
    {
      t1 <- dS2_lin2_test(points1[1, ], alpha, y2)
      t2 <- dS2_lin2_test(points2[2, ], alpha, y1)
      tt <- list(t1 = t1, t2 = t2)
    },
    "dS3" = 
    {
      t1 <- dS3_lin2_test(points1[1, ], alpha, y2)
      t2 <- dS3_lin2_test(points2[2, ], alpha, y1)
    },
    stop("Enter valid depth notion !")
    )
    
    if(tt$t1$phi==0 || tt$t2$phi==0)
    {  
    sumi <- 1
    }
  }
  list(p1 = cbind(points1[p1, 1], points1[p1, 2]), p2 = cbind(points2[p2, 1], points2[p2, 2]), sumint = sumi)
}
