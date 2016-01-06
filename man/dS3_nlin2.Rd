\name{dS3_nlin2}
\alias{dS3_nlin2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dS3 for the non-linear AR(1) model with intercept
}
\description{
This function calculates the simplified version of simplicial depth for explosive AR(1) 
processes,  when overlapping residuals in sequences of 3 are evaluated. Thereby
the parameter theta and the process y are fixed. The assumed model is given by 
y_n = y_{n-1}+theta_1 *y_{n-1}^theta_2  + theta_0 +e_n, e_n i.i.d. errors with med(e_n) = 0.
}
\usage{
dS3_nlin2(theta,y)
}
\arguments{
  \item{theta}{
theta = (theta0,theta1), parameter vector to evaluate dS1, 
}
  \item{y}{
y = (y_0,...,y_N), oberserved process to evaluate dS1 in 
}
}
%\details{
%}
\value{
Result is a real number which gives the depth of theta based on the obervation y
}
\references{
Kustosz, Mueller and Wendler (2014). Simplified Simplicial Depth for Regression and
Autoregressive Growth Processes. Submitted. ++
}
\author{
Kustosz, Christoph
}
\note{
This expression is a simplification of dS, which is the full simplicial depth for explosive AR(1) processes with intercept
}


\seealso{
\code{\link{resARMod_lin2}}, \code{\link{dS_lin2}},\code{\link{dS1_lin2}}, \code{\link{dS2_lin2}}, \code{\link{dS3_lin2}}
}
\examples{
y<-RandomARMod_nlin2(100,0,0.2,1.01,15,0)
theta<-c(0,0.2,1.01)
dS3_nlin2(theta,y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
