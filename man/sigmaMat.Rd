\name{sigmaMat}
\alias{sigmaMat}
\title{
Calculation of covariance matrix for the limit process of full simplical depth for two parameters on a grid
}
\description{
This function calculates the covariance matrix of the limit process in Kustosz, Leucht and Mueeller (2016) for a given grid.
}
\usage{
sigmaMat(t, nclust = 1)
}
\arguments{
  \item{t}{
Grid on [-2,2] on which the process later will be evaluated.
}
  \item{nclust}{
Number of processors on which the calculation can be splitted. Default value is given by 1.
}
}
\details{
Details on the derivation of the limit are given in Kustosz, Leucht and Mueller (2016). Remarks on the implementation are presented in Kustosz (2016).
}
\value{
The function retruns the variance covariance matrix on the predefined grid, which then can be used to simulate the process. 
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
Kustosz C., Leucht A. and Mueller Ch. H. (2016). Tests based on simplicial depth for AR(1) models with
explosion. Journal of Time Series Analysis. In press.\cr
\cr
}
\author{
Kustosz, Christoph
}

\seealso{
\code{\link{simulateGP}}, \code{\link{muVec}}
}
\examples{
g <- seq(-2, 2, 0.1)
sigma <- sigmaMat(g)
Y <- simulateGP(g, sigma)
plot(Y$Y1)
lines(Y$Y2)
}
