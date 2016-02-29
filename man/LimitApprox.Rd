\name{LimitApprox}
\alias{LimitApprox}
\title{
Approximation of the integrated limit based on a simulated bivaraite gaussian process
}
\description{
This function approximates the integrated limit based on the bivariate gaussian limit process discussed in Kustosz, Leucht and Mueller (2016).
}
\usage{
LimitApprox(g,Y)
}
\arguments{
  \item{g}{
Grid, on which the bivariate proces has been evaluated.
}
  \item{Y}{
A realisation of the bivariate Gaussian limit process.
}
}
\details{
The theoretical background can be found in Kustosz, Leucht and Mueller (2016). Details on the usage in the calculation be found in Kustosz (2016).
}
\value{
A approximation of the integrated process as defined in Leucht, Mueller and Kustosz (2014)
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

\examples{
g<-seq(-2,2,0.1)
sigma<-sigmaMat(g)

Y<-simulateGP(g,sigma)

R<-LimitApprox(g,Y)
}

