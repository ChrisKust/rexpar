\name{changepoints_lin2}
\alias{changepoints_lin2}
\title{
Change-point detection for a linear AR(1) model with intercept
}
\description{
This function detects change points for parameter changes of a linear AR(1) model with intercept based on simplicial depth. The model is
defined by \deqn{Y_n = \theta_1 Y_{n-1} + \theta_0 + E_n} with \eqn{med(E_n)=0}. The main idea is based on estimators and confidence intervals based on simplicial depth as proposed by Kustosz (2016). Further, a preselection of candidates by a simplified depth notion is used. 
}
\usage{
changepoints_lin2(y, level, bw, sw, plots, method, ncoresCP, mincper, mincp)
}

\arguments{
  \item{y}{
An observed AR(1) process with intercept.
}
\item{level}{
The level for which the confidence intervals for the phase change detection are calculated.
}
\item{bw}{
A bandwidth defining the sample size for sub-samples which are evaluated in the algorithm.
}
\item{sw}{
A step-width to reduce the candidate points in which the change points are evaluated.
}
\item{plots}{
A switch (TRUE and FALSE) to determine, if plots shall be produced for each tested candidate.
}
\item{method}{
This parameter determines the method to detect change points. If \code{method = "m1"} is selected a candidate is marked as change, if the confidence intervals do not overlap. For \code{method = "m2"} the change point is marked, if the estimates are not inside of the non respective confidence intervals.
}
\item{ncoresCP}{
Number of CPU cores for parallel computing.
}
\item{mincper}{
Percentage to define relevant change-point clusters based on the length of the 'mincp' longest detected cluster.
}
\item{mincp}{
The 'mincp' longest cluster is used to define a threshold for detected clusters which are considered for final break-point estimates. This implies, that (if enough clusters exist), the number also defines the minimal number of changes, detected by the algorithm.
}
}
\details{
For details see Kustosz (2016).
}
\value{
\item{candidates}{A vector of observation indices in which the algorithm tested for change points.}
\item{changepoints}{A vector indicating, if a change point was detected in the respective candidate.}
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
}
\author{
Kustosz, Christoph
}
\seealso{
  \code{\link{RandomARMod_lin2}}, \code{\link{lin2_CI}},   \code{\link{convex_hull_plot}},   \code{\link{convex_hull_intersect}},  \code{\link{est_lin2}},  \code{\link{dS_lin2}},  \code{\link{dS_lin2_test}},\code{\link{dS1_lin2_test}}
}
\examples{
int1 <- 0.1
int2 <- 0.3
arp1 <- 1.015
arp2 <- 1.001

set.seed(123)
y1 <- RandomARMod_lin2(50, int1, arp1, 15, 0)
y2 <- RandomARMod_lin2(50, int2, arp2, y1[length(y1)], 0)
y <- c(y1,y2)

\dontrun{
CP1 <- changepoints_lin2(y, 0.99, 25, 5, plots=FALSE, method="m1", ncoresCP=4)
CP2 <- changepoints_lin2(y, 0.99, 25, 5, plots=FALSE, method="m2", ncoresCP=4)

par(mfrow = c(2, 1))
plot(y, type = "l")
abline(v=CP1$candidates[CP1$changepoints==1], col=2,lty=2)
abline(v=CP2$candidates[CP2$changepoints==1], col=3,lty=3)

plot(y, type="l")
abline(v=CP1$rjumps, col=2,lty=2,lwd=2)
abline(v=CP2$rjumps, col=3,lty=3,lwd=3)
}
}