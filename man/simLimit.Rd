\name{simLimit}
\alias{simLimit}
\docType{data}
\title{
Simulations from a limit process
}
\description{
The dataset contains results form 200.000 simulations of the limit process for a rescaled and centered version of the full simplicial depth statistic dS derived in Leucht, Mueller and Kustosz (2014). This data is necessary to conduct the full simplicial depth test for explosive AR processes included in this package
}
\usage{data(simLimit)}
\format{
  A data frame with 200000 observations on the following 1 variables.
  \describe{
    \item{\code{Sim}}{a numeric vector of simulated values of the limit process}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Data generated as in Leucht, Mueller and Kustosz (2014)
}
\seealso{
\code{\link{simulateGP}}

}
\references{
Leucht, Kustosz and Mueller (2014). Tests based on simplicial depth for AR(1) models with
explosion. Submitted.
}
\examples{
data(simLimit)
hist(simLimit[,1])
}
\keyword{datasets}
