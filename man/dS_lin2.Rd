\name{dS_lin2}
\alias{dS_lin2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simplicial depth for explosive AR(1) processes
}
\description{
This function calculates simplicial depth for explosive AR(1) processes as defined in Leucht, Kustosz and Mueller (2014). Thereby it is capable to autodetects the number of available cores or uses a user defined number of cores to speed up computation. The basic model is defined by \deqn{Y_n = \theta_0 + \theta_1 Y_ {n-1}+E_n}, with \eqn{Y_n} being an increasing process and \eqn{E_n} satisfying \eqn{med(E_n) = 0}.
}
\usage{
dS_lin2(theta,y,ncores=1,model,cpow=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
Parameter theta = (theta0,theta1) for which simplicial depth has to be evaluated. 
}
  \item{y}{
Observed proces y=(y0,...,yN) for which simplicial depth has to be evaluated.
}
  \item{ncores}{
Number of cores for parallel computation. If no value is selected the method sets the cores to 1 and no parallel comutation is applied. If the value is set to 'auto', the function detects the available number of cores and uses the maximal number minus 1. The function produces errors, if more cores than available or a wrong argument is inserted.
}
  \item{model}{
Allows the selection of the linear AR(1) model with intercept by "linAR1", the nonlinear AR model without intercept by "nlinAR1" or the linear AR(1) model with intercept and fixed power cpow "linARc". The swich therfore changes the definition of the residuals used in the calculation.
}
\item{cpow}{
fixed and known power parameter for the y(n) = theta2*y(n)^cpow + theta1 model
}
}

\value{
The function returns the simplicial depth of the parameter theta for an observed process y.
}
\references{
Leucht, Kustosz and Mueller (2014). Tests based on simplicial depth for AR(1) models with
explosion. Submitted.
}
\author{
Kustosz, Christoph
}
\note{
The function depends on internal functions.
}



\seealso{
\code{\link{resARMod_lin2}}, \code{\link{dS_lin2}},\code{\link{dS1_lin2}}, \code{\link{dS2_lin2}}, \code{\link{dS3_lin2}}, \code{\link{oner}}
}
\examples{
y<-RandomARMod_lin2(100,0.2,1.01,15,0)
theta<-c(0.2,1.01)
 \dontrun{
dS_lin2(theta,y)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
