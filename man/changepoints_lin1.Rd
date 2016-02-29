\name{changepoints_lin1}
\alias{changepoints_lin1}
\title{
Change-point detection for a linear AR(1) model without intercept
}
\description{
This function detects change points for parameter changes of a linear AR(1) model without intercept based on simplicial depth. The model is
defined by \deqn{Y_n = \theta_{1} \cdot Y_{n-1} + E_n} with \eqn{med(E_n)=0}. The main idea is based on estimation and confidence intervalconstruction based on simplicial depth as proposed in Kustosz (2016). Thereby, the applied depth notion is given by the full simplicial depth.
}
\usage{
changepoints_lin1(y,level,bw,sw,plots=FALSE,method=1,mincper=1,mincp=1)
}
\arguments{
  \item{y}{
An observed AR(1) process without intercept.
}
\item{level}{
The level for which the confidence intervals for the phase change detection are calculated.
}
\item{bw}{
A bandwidth, defining the sample size for sub-samples which are evaluated in the algorithm.
}
\item{sw}{
A step-width to reduce the candidate points in which the change points are evaluated.
}
\item{plots}{A switch, activating plots while the calculations are preformed. The plots show the confidence regions for the actually considered windows.}
\item{method}{
This parameter determines the method to detect change points. If \code{method=1} is selected a candidate is marked as change, if the confidence intervals do not overlap. For \code{method=2} the change point is marked, if the estimates are not inside of the non respective confidence intervals.
}
\item{mincper}{
Percentage to define relevant change-point clusters based on the length of the 'mincp' longest detected cluster.
}
\item{mincp}{
The 'mincp' longest cluster is used to define a threshold for detected clusters which are considered for final break-point estimates. This implies, that (if enough clusters exist), the number also defines the minimal number of changes, found by the algorithm.
}

}
\details{.
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
  \code{\link{RandomARMod_lin2}}, \code{\link{lin1_CI}},  \code{\link{est_lin1}},  \code{\link{dS_lin1}},  \code{\link{dS_lin1_test}}}
\examples{
int1 <- 0
int2 <- 0.2
arp1 <- 1.001
arp2 <- 1.004


y1 <- RandomARMod_lin2(50, int1, arp1, 15, 3)
y2 <- RandomARMod_lin2(50, int2, arp2, y1[length(y1)], 3)
y <- c(y1, y2)

\dontrun{
par(ask=T)

CP <- changepoints_lin1(y, 0.99, 25, 10, plots=FALSE, method=1)

plot(y, type="l")
abline(v=CP$candidates[CP$changepoints], col=2)
}
}
