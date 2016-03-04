\name{changepoints_lin2_cl}
\alias{changepoints_lin2_cl}
\title{
Change-point detection for a linear AR(1) model with intercept (cluster version)
}
\description{
This function detects change points for parameter changes of a linear AR(1) model with intercept based on simplicial depth. The model is
defined by \deqn{Y_n = \theta_1 Y_{n-1} + \theta_0 + E_n} with \eqn{med(E_n)=0}. The main idea is based on estimators and confidence intervals based on simplicial depth as proposed by Kustosz (2016). Further, a preselection of candidates by a simplified depth notion is used. 
This is a version which uses a preset cluster which hast to be passed as argument when calling the function.
}
\usage{
changepoints_lin2_cl(y, level, bw, sw, plots, method, cluster, mincper, mincp)
}
\arguments{
  \item{y}{
An observed AR(1) process with intercept.
}
\item{level}{
The level for which the confidence intervals for the phase change detection are calculated.
}
\item{bw}{
A bandwidth defining the sample size for sub-samples which are evaluated in the detection algorithm.
}
\item{sw}{
A step-width to reduce the candidate points in which the change points are evaluated.
}
\item{plots}{
A switch (TRUE and FALSE) to determine if plots shall be produced for each tested candidate
}
\item{method}{
This parameter determines the method to detect change points. If \code{method=1} is selected a candidate is marked as change, if the confidence intervals do not overlap. For \code{method=2} the change point is marked, if the estimates are not inside of the non respective confidence intervals.
}
\item{cluster}{
Initialized cluster for parallel computing.
}
\item{mincper}{
Percentage to define relevant change-point clusters based on the length of the 'mincp' longest detected cluster.
}
\item{mincp}{
The 'mincp' longest cluster is used to define a threshold for detected clusters which are considered for final break-point estimates. This implies, that (if enough clusters exist), the number also defines the minimal amount of detected breakpoints. 
}
}
\details{
For details see Kustosz (2016).
}
\value{
\item{candidates }{A vector of observation indices in which the algorithm tested for change points.}
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
  \code{\link{RandomARMod_lin2}}, \code{\link{lin2_CI_cl}},   \code{\link{convex_hull_plot}},   \code{\link{convex_hull_intersect}},  \code{\link{est_lin2}},  \code{\link{dS_lin2}},  \code{\link{dS_lin2_test}},  \code{\link{dS1_lin2_test}}
}

\examples{
int1 <- 0.1
int2 <- 0.2
arp1 <- 1.001
arp2 <- 1.004


y1 <- RandomARMod_lin2(50, int1, arp1, 15, 3)
y2 <- RandomARMod_lin2(50, int2, arp2, y1[length(y1)], 3)
y <- c(y1,y2)



\dontrun{
par(ask=T)
cl <- makeCluster(2)

CP <- changepoints_lin2_cl(y, 0.99, 25, 10, plots=FALSE, method=1, cl=cl)

plot(y, type="l")
abline(v=CP$candidates[CP$changepoints], col=2)
stopCluster(cl)
}
}


