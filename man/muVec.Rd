\name{muVec}
\alias{muVec}
\title{
Mean vector function for limit process
}
\description{
This function evaluates the mean vector function for the limit process derived in Kustosz, Leucht and Mueller (2016). It is just useful in the internal calculations of the Limit simulation.
}
\usage{
muVec(t, y)
}

\arguments{
  \item{t}{
Grid vector to define set, where the process has to be evaluated later on.
}

  \item{y}{
Value of second marginal process, which defines the full mean function after a simulation.
}
}
\details{
The theoretical details can be found in Kustosz, Leucht and Mueller (2016). Details on the implementation can be found in Kustosz (2016).}

\value{
Returns a vector with the dimensions of the grid, which defines the conditional mean function, when the second marginal process is already known.
}
\references{
Kustosz C., Leucht A. and Mueller Ch. H. (2016). Tests based on simplicial depth for AR(1) models with
explosion. Journal of Time Series Analysis. In press.\cr
\cr
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
}
\author{
Kustosz, Christoph
}

\seealso{
\code{\link{simulateGP}}, \code{\link{sigmaMat}}
}
\examples{
g <- seq(-2, 2, 0.5)
y <- 0.25
muVec(g, y)
}
