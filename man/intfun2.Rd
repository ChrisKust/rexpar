\name{intfun2}
\alias{intfun2}
\title{
Covariance function integral
}
\description{
This function evaluates a part of the covariance matrix for the Limit process derived in Kustosz, Leucht and M\"{u}ller (2016). It is just useful for internal calculations in the simulation of the limit distribution.
}
\usage{
intfun2(t2,t1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t2}{
Value for the point in time t2 in the formula.
}
  \item{t1}{
Value for the point in time t2 in the formula.
}
}
\details{
The theoretical background can be found in Kustosz, Leucht and M\"{u}ller (2016). Details on the usage in the calculation be found in Kustosz (2016).
}
\value{
Returns the value of the integral.
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
\note{
Auxiliary function for internal usage in some more central functions of this package.
}


\seealso{
\code{\link{find1}}, \code{\link{find2}},\code{\link{intfun2}}, \code{\link{oner}}
}
\examples{
intfun2(0,0)
}
