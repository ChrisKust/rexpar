\name{dS_lin2_loop}
\alias{dS_lin2_loop}
\title{
Straightforward calculation of simplicial depth for AR(1) processes with intercept
}
\description{
The function calculates simplicial depth for AR(1) processes with intercept as discussed in Leucht, Mueller and Kustosz (2014) by nested loops. The default model thereby is defined by \deqn{Y_n= \theta_0+\theta_1 Y_{n-1} + E_n} and \eqn{med(E_n)=0}. However, other models are selectable.
}
\usage{
dS_lin2_loop(theta,y)
}
\arguments{
  \item{theta}{
Parameter \eqn{\theta} in which dS is evaluated.
}
  \item{y}{
Observed data \eqn{y = (y_0,...,y_N)} for which dS is evaluated. 
}
}
\details{
The theoretical details can be found in Kustosz, Leucht and Mueller (2016). The computational 
details are in Kustosz (2016).
}

\value{
The function returns dS for given theta and y. 
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
The function is not really efficient for large samples. Use dS_lin2 instead. 
}

\seealso{
 \code{\link{dS_lin2}}
}
\examples{
y<-RandomARMod_lin2(15,0.2,1.01,15,0)
theta<-c(0.2,1.01)
dS_lin2_loop(theta,y)
}
 
