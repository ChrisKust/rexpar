\name{dS2_lin2}
\alias{dS2_lin2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dS2 for AR(1) with intercept
}
\description{
This function calculates the simplified version of simplicial depth for explosive AR(1) 
processes, when non partially overlapping residuals with the middle residual fixed are evaluated. Thereby the parameter \eqn{\theta} and the process y are fixed. The assumed model given by the argument model.
}
\usage{
dS2_lin2(theta,y,model)
}
\arguments{
  \item{theta}{
theta = (theta0,theta1), parameter vector to evaluate dS1, 
}
  \item{y}{
y = (y_0,...,y_N), oberserved process to evaluate dS2 in 
}
  \item{model}{
here the model for the calculation of the underlying residuals is specified. currently the following
models are available\cr
"linAR1woI" = linear AR(1) model without intercept\cr
"linAR1" = linear AR(1) model with intercept\cr
"linAR2" = linear AR(2) model without intercept\cr
"nlinAR1" = linear AR(1) model without intercept but with power paremeter\cr
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
dS2_lin2(theta,y)

## The function is currently defined as
function(theta,y)
{
  res=resARMod_lin2(c(theta[1],theta[2]),y)
  r1=res[seq(1,floor((length(res)+1)/2)-1,1)]
  r2=rep(res[floor((length(res)+1)/2)],floor((length(res)+1)/2))
  r3=res[seq(floor((length(res)+1)/2)+1,length(res),1)]
  m=min(c(length(r1),length(r2),length(r3)))
  r1=r1[1:m]
  r2=r2[1:m]
  r3=r3[1:m]
  InD=(r1>0)*(r2<0)*(r3>0)+(r1<0)*(r2>0)*(r3<0)
  depth<-1/(m)*sum(InD)
  
  
  return(depth)
  
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
