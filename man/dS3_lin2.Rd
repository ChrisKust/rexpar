\name{dS3_lin2}
\alias{dS3_lin2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dS3 for AR(1) with intercept
}
\description{
This function calculates the simplified version of simplicial depth for explosive AR(1) 
processes, when overlapping residuals in sequences of 3 are evaluated. Thereby
the parameter \eqn{\theta} and the process y are fixed. The assumed model given by the argument model. 
}
\usage{
dS3_lin2(theta,res,y,model=c("linAR1", "linAR1woI", "nlinAR1", "linAR2", "linARc"),cpow=1)
}
\arguments{
  \item{theta}{
theta = (theta0,theta1), parameter vector to evaluate dS3, 
}

  \item{res}{
Here the evaluated residuals can be inserted, instead of a parameter theta and data y. Then the statistic 
is evaluated based on these residuals by calculation of their signs
}
  \item{y}{
y = (y_0,...,y_N), oberserved process to evaluate dS3 in 
}
  \item{model}{
here the model for the calculation of the underlying residuals is specified. currently the following
models are available\cr
"linAR1woI" = linear AR(1) model without intercept\cr
"linAR1" = linear AR(1) model with intercept\cr
"linAR2" = linear AR(2) model without intercept\cr
"nlinAR1" = linear AR(1) model without intercept but with power paremeter\cr
"linARc" = linear AR(1) model with intercept and fixed and known power parameter cpow\cr
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
dS3_lin2(theta,y)


## The function is currently defined as
function(theta,y)
{
  res=resARMod_lin2(c(theta[1],theta[2]),y)
  r1=res[seq(1,length(res)-2,1)]
  r2=res[seq(2,length(res)-1,1)]
  r3=res[seq(3,length(res),1)]
  m=min(c(length(r1),length(r2),length(r3)))
  r1=r1[1:m]
  r2=r2[1:m]
  r3=r3[1:m]
  InD=(r1>0)*(r2<0)*(r3>0)+(r1<0)*(r2>0)*(r3<0)
  depth<-1/(length(y)-2)*sum(InD)
  
  
  return(depth)
  
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
