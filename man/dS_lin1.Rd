\name{dS_lin1}
\alias{dS_lin1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simplicial depth for explosive AR(1) processes without intercept
}
\description{
This function calculates simplicial depth for explosive AR(1) processes as defined in Kustosz and Mueller (2014). The basic model is defined by \deqn{Y_n = \theta_1 Y_ {n-1}+E_n}, with \eqn{Y_n} being an increasing process and \eqn{E_n} satisfying \eqn{med(E_n) = 0}.
}
\usage{
dS_lin1(theta,y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
Parameter theta for which simplicial depth has to be evaluated. 
}
  \item{y}{
Observed proces y=(y0,...,yN) for which simplicial depth has to be evaluated.
}

}

\value{
The function returns the simplicial depth of the parameter theta for an observed process y.
}
\references{
Kustosz and Mueller (2014). ???
}
\author{
Kustosz, Christoph
}




\seealso{
\code{\link{resARMod_lin2}}, \code{\link{dS_lin2}},\code{\link{dS1_lin2}}, \code{\link{dS2_lin2}}, \code{\link{dS3_lin2}}, \code{\link{oner}}
}
\examples{
y<-RandomARMod_lin2(100,0,1.01,15,0)
theta<-1.01
 \dontrun{
dS_lin2(theta,y)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
