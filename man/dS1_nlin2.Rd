\name{dS1_nlin2}
\alias{dS1_nlin2}
\title{
dS1 for the non-linear AR(1) model with intercept
}
\description{
This function calculates a simplified version of simplicial depth for explosive AR(1) 
processes, when non overlapping residuals in sequences of 3 are evaluated. Thereby
the parameter \eqn{\theta} and the process y are fixed. The assumed model is given by 
\deqn{Y_n = Y_{n-1}+theta_1 *Y_{n-1}^theta_2  + theta_0 +E_n,}
whreby \eqn{E_n} i.i.d. errors with \eqn{med(E_n) = 0}.
}
\usage{
dS1_nlin2(theta,y)
}
\arguments{
  \item{theta}{
\eqn{\theta = (\theta_0,\theta_1,\theta_2)}, parameter vector to evaluate dS1 in.
}
  \item{y}{
\eqn{y = (y_0,...,y_N)}, oberserved process to evaluate dS1 in.
}
}
\details{
The theoretical details can be found in Kustosz, Muller and Wendler (2016). The computational 
details are in Kustosz (2016).
}
\value{
The result is a real number which gives the depth of theta based on the obervation y
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
Kustosz C., Muller Ch. H. and Wendler M. (2016). Simplified Simplicial Depth for Regression and
Autoregressive Growth Processes. Journal of Statistical Planning and Inference. In press.\cr
\cr
}
\author{
Kustosz, Christoph
}
\note{
This expression is a simplification of dS, which is the full simplicial depth for explosive AR(1) processes
}


\seealso{
\code{\link{resARMod_lin2}}, \code{\link{dS_lin2}},\code{\link{dS1_lin2}}, \code{\link{dS2_lin2}}, \code{\link{dS3_lin2}}
}
\examples{
y<-RandomARMod_nlin2(100,0,0.2,1.01,15,0)
theta<-c(0,0.2,1.01)
dS1_nlin2(theta,y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
