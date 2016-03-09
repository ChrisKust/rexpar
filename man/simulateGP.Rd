\name{simulateGP}
\alias{simulateGP}
\title{
Simulation of the gaussian limit process from simplicial depth based on two parameter models and alternating residuals
}
\description{
This function simulates the gaussian limit process defined in Kustosz, Leucht, and M\"{u}eller (2016). Thereby a grid can be definied and the covariance matrix has to be precalculated to enable a faster generation of repetitions. 
}
\usage{
simulateGP(g, sigma)
}
\arguments{
  \item{g}{
One dimensional grid between on [-2,2] on which the process is simulated.
}
  \item{sigma}{
Precalculated covariance matrix based on the predefined grid. This can be done by \code{\link{sigmaMat}}.
}
}
\details{
Details on the derivation of this limit process are presented in Kustosz, Leucht and M\"{u}ller (2016). More information on the implementation can be found in Kustosz (2016).
}
\value{
The function retruns the bivariate process as list
 \item{Y1}{Variable component of the process.}
 \item{Y2}{Second compontent of the process, which is time independent.}
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
Kustosz C., Leucht A. and M\"{u}ller Ch. H. (2016). Tests based on simplicial depth for AR(1) models with
explosion. Journal of Time Series Analysis. In press.\cr
\cr
}
\author{
Kustosz, Christoph
}

\seealso{
\code{\link{sigmaMat}}, \code{\link{muVec}}, \code{\link{find1}},\code{\link{find2}},\code{\link{intfun2}}
}
\examples{
g <- seq(-2, 2, 0.1)
sigma <- sigmaMat(g)
Y <- simulateGP(g, sigma)
plot(g, Y$Y1)
lines(g, Y$Y2)
}

