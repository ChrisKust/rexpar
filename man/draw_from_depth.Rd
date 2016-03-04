\name{draw_from_depth}
\alias{draw_from_depth}
\title{ Random number generation based on empirical simplicial depth
}
\description{
This function transforms a given empirical simplicial depth to a distribution function and generates random numbers from this distribution by inversion as proposed in Kustosz (2016).
}
\usage{
draw_from_depth(depthI, testvec, lower, upper)
}
\arguments{
  \item{depthI}{
Values of an empirical depth calculation.
}
  \item{testvec}{
Parameters of the empirical depth vector defined by depthI.
}
  \item{lower}{
A lower candidate value to restrict the depth on an interval \eqn{[lower,upper]}.
}
  \item{upper}{
A upper candidate value to restrict the depth on an interval \eqn{[lower,upper]}.
}
}
\details{
For details see Kustosz (2016).
}
\value{
The function returns a simulated parameter theta. 
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
}
\author{
Christoph Kustosz and Sebastian Szugat
}

\seealso{
 \code{\link{predict_lin1}}
}
\examples{
set.seed(234)
y <- RandomARMod_lin2(100, 0, 1.01, 15, 0)

#evaluate depth on grid
thetas <- seq(0.97, 1.05, 0.001)
dS <- unlist(lapply(thetas, dS_lin1, y=y))

#plot emprirical depth
par(mfrow=c(2, 2))
plot(thetas, dS)

#generate random parameters 
tmi <- min(thetas[dS>0])
tma <- max(thetas[dS>0])
rand_theta <- replicate(10000, draw_from_depth(dS, thetas, lower=tmi, upper=tma))

#plot 'discrete' distribution on grid and empirical distribution
plot(table(rand_theta)/length(rand_theta), type="h")
plot(ecdf(rand_theta))

#plot process and generate trajectories with estimated variance
#we now assume normally distributed errors with mean 0 for simulations
plot(y, type="l")
yy <- matrix(ncol=100, nrow=100)
for(i in 1:100)
{
rt <- sample(rand_theta, 1)
res <- resARMod_lin2(c(0, rt), y)
sd_est <- sd(res) 
yy[i, ] <- RandomARMod_lin2(100, 0, rt, y[1], sd=sd_est)
if(floor(i/10)==i/10)
{
lines(yy[i, ],col=i)
}
mean_f <- apply(yy, 2, mean)
lines(mean_f, col=2, lwd=2)
}

}
