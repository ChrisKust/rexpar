\name{dS_lin2_sym}
\alias{dS_lin2_sym}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dS for AR(1) with two parameters in a symmetrised version
}
\description{
This function calculates the  simplicial depth for explosive AR(1) 
processes with two parameters. Thereby the parameter \eqn{\theta} and the process y are fixed. The assumed model given by the argument model. Further the kernel is replaced by a symmetrised version. This in particular allows the application of standard limit theorems for U-Statistics
}
\usage{
dS_lin2_sym(theta, resy, y, ncores = 1, 
model = c("linAR1", "nlinAR1", "linARc"), cpow = 1)
}
\arguments{
  \item{theta}{
theta = (theta0,theta1), parameter vector to evaluate dS1, 
}
\item{resy}{
Instead of a model and a parameter theta, residuals can be plugged in directly. Then just the sign changes 
are calculated and the statistic is evaluated

}
  \item{y}{
y = (y_0,...,y_N), oberserved process to evaluate dS2 in 
}
\item{ncores}{
This value allows the usage of multiple cores to calculate the statistic
}
  \item{model}{
here the model for the calculation of the underlying residuals is specified. currently the following
models are available\cr
"linAR1" = linear AR(1) model with intercept\cr
"nlinAR1" = linear AR(1) model without intercept but with power paremeter\cr
"linARc" = linear AR(1) model with intercept and fixed power parameter cpow\cr
}
\item{cpow}{
fixed and known power parameter for the y(n) = theta2*y(n)^cpow + theta1 model
}
}
%\details{
%}
\value{
Result is a real number which gives the depth of theta based on the obervation y
}
\references{
Kustosz, Mueller and Wendler (2014). Simplified Simplicial Depth for Regression and
Autoregressive Growth Processes. Submitted.
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
y<-RandomARMod_lin2(100,0.2,1.01,15,0)
theta<-c(0.2,1.01)
dS_lin2_sym(theta,y,model="linAR1")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
