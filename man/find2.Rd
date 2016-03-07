\name{find2}
\alias{find2}
\title{
Auxilliary covariance function 
}
\description{
This function evaluates a part of the covariance matrix for the Limit process derived in Kustosz, Leucht Mueeller (2016). It is just useful for internal calculations in the simulation of the limit distribution.}
\usage{
find2(x, pars)
}
\arguments{
  \item{x}{
Integration variable.
}

  \item{pars}{
Value t, where the integral has to be evaluated.
}
}

\details{
The theoretical background can be found in Kustosz, Leucht and Mueller (2016). Details on the usage in the calculation be found in Kustosz (2016).
}
\value{
Returns the value of the integral.
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
\note{
Auxiliary function for internal usage in some more central functions of this package.
}


\seealso{
\code{\link{find1}}, \code{\link{find2}},\code{\link{intfun2}}, \code{\link{oner}}
}
\examples{
find2(1, 0)
}

