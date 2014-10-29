\name{intfun2}
\alias{intfun2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Covariance function integral 2
}
\description{
This function evaluates a part of the covariance matrix for the Limit process derived in Leucht, Mueller and Kustosz (2014). It is just useful forinternal calculations in the simulation of the limit distribution.
}
\usage{
intfun2(t2,t1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t2}{
Value for the point in time t2 in the formula
}
  \item{t1}{
Value for the point in time t2 in the formula
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns the value of the integral
}
\references{
Leucht, Kustosz and Mueller (2014). Tests based on simplicial depth for AR(1) models with
explosion. Submitted.
}
\author{
Kustosz, Christoph
}
\note{
Auxiliary function for internal usage in some more central functions of this package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{find1}}, \code{\link{find2}},\code{\link{intfun2}}, \code{\link{oner}}
}
\examples{
intfun2(0,0)

## The function is currently defined as
function(t2,t1)
{
  r<-integrate(find2,0,1,c(t1,t2))$value-integrate(find1,0,1,t1)$value*integrate(find1,0,1,t2)$value
  return(r)  
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
