\name{Tri_Eps_dist}
\alias{Tri_Eps_dist}
\title{
Calculation of candidates, restricted for faster estimation by simplicial depth
}
\description{
This function calculates candidates for estimates by simplicial depth based on intersections from residual straights and epsilon variation. Thereby for large samples the candidates can be restricted by quantiles to reduce computational costs. Further the inner points are just calulated by simplices defineded by the colsest neighbours of each point. 
}
\usage{
Tri_Eps_dist(y, perc, eps)
}
\arguments{
  \item{y}{
A series form from a linear AR(1) process with intercept for which the candidates have to be calculated.
}
\item{perc}{
A value between 0 and 1 defining what range of candidates around of their median has to be considered for the optimisation. A value close to 0 gives a low precision, while a value of 1 evaluates all candidates.
}
\item{eps}{
Value for epsilon to vary the corner of a considered data generated simplex.
}
}
\details{
The main idea for the candidate construction is presented in Kustosz (2016).
}
\value{
The returned value is a matrix of candidate parameters whereby the first column is \eqn{\theta_1} and the second \eqn{\theta_0}.
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
 \code{\link{est_nlin1}},  \code{\link{dS_lin2_test}}
}

\examples{
set.seed(17)
y <- RandomARMod_lin2(nobs = 20, intercept = 0.2, arp = 1.001, start = 15, cont = "0")
tt <- Tri_Eps_dist(y = y, perc = 0.5, eps = 1e-3)
plot(tt[,1], tt[,2])
}
