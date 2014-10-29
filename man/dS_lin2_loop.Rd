\name{dS_lin2_loop}
\alias{dS_lin2_loop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Straightforward calculation of simplicial depth for AR(1) processes with intercept
}
\description{
The function calculates simplicial depth for AR(1) processes with intercept as discussed in Leucht, Mueller and Kustosz (2014) by nested loops. The main model thereby is defined by \deqn{Y_n= \theta_0+\theta_1 Y_{n-1} + E_n} and \eqn{med(E_n)=0}.
}
\usage{
dS_lin2_loop(theta,y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
Parameter for which dS has to be evaluated. theta = (theta0,theta1)
}
  \item{y}{
Observed data for which dS has to be evaluated. 
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The function returns dS for given theta and y. 
}
\references{
Leucht, Mueller and Kustosz (2014). Tests based on simplicial depth for AR(1) models with
explosion. Submitted.
}
\author{
Kustosz, Christoph
}
\note{
The function is not really efficient for large samples. Use dS_lin2 instead. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{dS_lin2}}
}
\examples{
y<-RandomARMod_lin2(15,0.2,1.01,15,0)
theta<-c(0.2,1.01)
dS_lin2_loop(theta,y)


## The function is currently defined as
function(theta,y)
{
  sd<-0
  y1<-y[2:length(y)]
  y0<-y[1:(length(y)-1)]
  resy<-y1-theta[1]-theta[2]*y0
  for(ki in 1:(length(resy)-2))
  {
    for(kii in ki:(length(resy)-1))
    {
      for(kiii in kii:(length(resy)))
      {
        sd<-sd+(resy[ki]<0)*(resy[kii]>0)*(resy[kiii]<0)+(resy[ki]>0)*(resy[kii]<0)*(resy[kiii]>0)
      }
    }
    
  }
  sd<-1/choose(length(resy),3)*sd
  return(sd)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
