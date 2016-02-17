\name{simLimit}
\alias{simLimit}
\docType{data}
\title{
Simulation of the final limit process from simplicial depth based on two parameter models and alternating residuals
}
\description{
The dataset contains results form 200.000 simulations of the limit process for a rescaled and centered version of the full simplicial depth statistic dS derived in Leucht, Mueller and Kustosz (2014). This data is necessary to conduct the full simplicial depth test for explosive AR processes included in this package. In addition to the gaussian process simulation, here the integration of the two processes is calculated to compute the final limit process.
}
\usage{data(simLimit)}
\format{
  A data frame with 200000 observations on the following 1 variables.
  \describe{
    \item{\code{Sim}}{a numeric vector of simulated values of the limit process.}
  }
}
\details{
Details on the derivation of this limit process are presented in Kustosz, Leucht and M\"{u}ller (2016). More information on the implementation can be found in Kustosz (2016).
}
\source{
Data generated as in Kustosz, Leucht and M\"{u}ller (2016).
}
\seealso{
\code{\link{simulateGP}}

}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
Kustosz C., Leucht A. and M\"{u}ller Ch. H. (2016). Tests based on simplicial depth for AR(1) models with
explosion. Journal of Time Series Analysis. In press.\cr
\cr
}
\examples{
data(simLimit)
hist(simLimit[,1])
}
\keyword{datasets}
