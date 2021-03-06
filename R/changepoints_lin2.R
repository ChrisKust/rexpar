changepoints_lin2 <- function(y, level, bw, sw, plots = FALSE, method = "m1", ncoresCP = 1, mincper = 1, mincp = 1)
{
alpha <- (1 - level)
cands <- seq(bw + 1,length(y) - bw, sw)
changeind <- numeric(length(cands))
if(plots)
{  
par(mfrow = c(1, 1))
plot(0, 0, xlab = expression(theta[1]), ylab = expression(theta[2]), main = "", xlim=c(-1, 1), ylim=c(0.9, 1.1))
}
changeind <- switch(method,
    "m1" = 
{
for(i in 1:length(cands))
{
  ASp1 <- lin2_CI(y[(cands[i] - bw):cands[i]], (1 - alpha), notion = "dS_pre", ncoresC = ncoresCP)
  ASp2 <- lin2_CI(y[cands[i]:(cands[i]+bw)], (1 - alpha), notion = "dS_pre", ncoresC = ncoresCP)
  ASp1$par[is.na(ASp1$inCI)] <- c(0, 0)
  ASp1$inCI[is.na(ASp1$inCI)] <- 1
  ASp2$par[is.na(ASp2$inCI)] <- c(0, 0)
  ASp2$inCI[is.na(ASp2$inCI)] <- 1
  if(plots)
  {  
    lx1 <- min(c(ASp1$par[ASp1$inCI == 0, 1], ASp2$par[ASp2$inCI == 0, 1]), na.rm=TRUE)
    if(lx1 == -Inf | is.na(lx1))
    {lx1 <- -10}
    lx2 <- max(c(ASp1$par[ASp1$inCI == 0, 1], ASp2$par[ASp2$inCI == 0, 1]), na.rm=TRUE)
    if(lx2 == Inf | is.na(lx2))
    {lx2 <- 10}
    ly1 <- min(c(ASp1$par[ASp1$inCI == 0, 2], ASp2$par[ASp2$inCI == 0, 2]), na.rm=TRUE)
    if(ly1 == -Inf | is.na(ly1))
    {ly1 <- 0.5}
    ly2 <- max(c(ASp1$par[ASp1$inCI == 0, 2],ASp2$par[ASp2$inCI == 0, 2]), na.rm=TRUE)
    if(ly2 == Inf | is.na(ly2))
    {ly2 <- 1.5}
  plot(0, 0, xlab=expression(theta[1]), ylab=expression(theta[2]), main=cands[i], xlim=c(lx1, lx2), ylim=c(ly1, ly2))
  convex_hull_plot(ASp1$par[ASp1$inCI == 0, 1], ASp1$par[ASp1$inCI == 0, 2], col = 1)
  convex_hull_plot(ASp2$par[ASp2$inCI == 0, 1], ASp2$par[ASp2$inCI == 0, 2], col = 2)
  }
  changeind[i] <- 1 - (convex_hull_intersect(cbind(ASp1$par[!ASp1$inCI, 1], ASp1$par[!ASp1$inCI, 2]), cbind(ASp2$par[!ASp2$inCI, 1], ASp2$par[!ASp2$inCI, 2]), y1 = y[(cands[i] - bw):cands[i]], y2 = y[cands[i]:(cands[i] + bw)])$sumint > 0)

}
  unlist(changeind)
},

    "m2" = 
{
 
  for(i in 1:length(cands))
  {
  E1 <- est_lin2(y[(cands[i] - bw):cands[i]], notion = dS_lin2, perc = 1)$estimate
  E2 <- est_lin2(y[cands[i]:(cands[i] + bw)], notion= dS_lin2, perc = 1)$estimate
  if(plots)
  {  
  ASp1 <- lin2_CI(y[(cands[i] - bw):cands[i]], (1 - alpha), notion = "dS_pre", ncoresC = ncoresCP)
  ASp2 <- lin2_CI(y[cands[i]:(cands[i] + bw)], (1 - alpha), notion = "dS_pre", ncoresC = ncoresCP)
  lx1 <- min(c(ASp1$par[!ASp1$inCI, 1], ASp2$par[!ASp2$inCI, 1]), na.rm=TRUE)
  if(lx1 == -Inf || is.na(lx1))
  {lx1 <- -10}
  lx2 <- max(c(ASp1$par[!ASp1$inCI, 1], ASp2$par[!ASp2$inCI, 1]), na.rm=TRUE)
  if(lx2 == Inf || is.na(lx2))
  {lx2 <- 10}
  ly1 <- min(c(ASp1$par[!ASp1$inCI, 2], ASp2$par[!ASp2$inCI, 2]), na.rm=TRUE)
  if(ly1 == -Inf || is.na(ly1))
  {ly1 <- 0.5}
  ly2 <- max(c(ASp1$par[!ASp1$inCI, 2], ASp2$par[!ASp2$inCI, 2]), na.rm=TRUE)
  if(ly2 == Inf || is.na(ly2))
  {ly2 <- 1.5}
  plot(0, 0, xlab=expression(theta[1]), ylab=expression(theta[2]), main=cands[i], xlim=c(lx1, lx2), ylim=c(ly1, ly2))
  ASp1$par[is.na(ASp1$inCI)] <- c(0, 0)
  ASp1$inCI[is.na(ASp1$inCI)] <- 1
  ASp2$par[is.na(ASp2$inCI)] <- c(0, 0)
  ASp2$inCI[is.na(ASp2$inCI)] <- 1
  convex_hull_plot(ASp1$par[!ASp1$inCI, 1], ASp1$par[!ASp1$inCI, 2], col=1)
  points(E1[1], E1[2], pch=19, col=1)
  convex_hull_plot(ASp2$par[!ASp2$inCI, 1], ASp2$par[!ASp2$inCI, 2], col=2)
  points(E2[1], E2[2], pch=19, col=2)
  }
  T1 <- dS_lin2_test(E1, alpha, y[cands[i]:(cands[i] + bw)], 1)$phi
  T2 <- dS_lin2_test(E2, alpha, y[(cands[i] - bw):cands[i]], 1)$phi
  changeind[i] <- T1 * T2

  }
  unlist(changeind)
},
stop("Enter valid method !")
)

changepoints <- numeric(length(changeind))
changepoints[changeind > 0] <- TRUE
changepoints[changeind == 0] <- FALSE
totjumps <- cands[changeind == 1]
clus <- follow_ups(totjumps, mincper = mincper, steps = sw, mincp = mincp)
cjumps <- clus$jumps
if(sum(cjumps) > 0)
{
  c_temp <- clus$clusters
  rjj <- numeric(max(clus$clusters))
  for(j in 1:max(clus$clusters))
  {	
    rjj[j] <- median(cjumps[c_temp==j])
  }
}
else 
{
  rjj <- 0
  c_temp <- 0	
}


list(candidates = cands, changepoints = changepoints, rjumps = rjj)
}


