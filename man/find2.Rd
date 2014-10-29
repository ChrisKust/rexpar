\name{find2}
\alias{find2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Covariance function par  1b
}
\description{
This function evaluates a part of the covariance matrix for the Limit process derived in Leucht, Mueller and Kustosz (2014). It is just useful forinternal calculations in the simulation of the limit distribution.
}
\usage{
find2(x,pars)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
integration variable
}

  \item{pars}{
value t, where the integral has to be evaluated
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
find2(1,0)

## The function is currently defined as
function(x,pars)
{
  Ret<-((x-pars[1])>-0.5)*((x-pars[1])<0.5)*((x-pars[2])>-0.5)*((x-pars[2])<0.5)
  return(Ret)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
