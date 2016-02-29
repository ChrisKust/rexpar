\name{dS_lin1}
\alias{dS_lin1}
\title{Simplicial depth for explosive AR(1) processes without intercept
}
\description{
This function calculates simplicial depth for explosive AR(1) processes as defined in Kustosz and Mueller (2014). The basic model is defined by \deqn{Y_n = \theta_1 Y_ {n-1}+E_n}, with \eqn{Y_n} being an increasing process and \eqn{E_n} satisfying \eqn{med(E_n) = 0}.
}
\usage{
dS_lin1(theta,y,mod=FALSE)
}
\arguments{
  \item{theta}{
Parameter \eqn{\theta} for which simplicial depth has to be evaluated. 
}
  \item{y}{
Observed proces \eqn{y=(y_0,...,y_N)} for which simplicial depth has to be evaluated.
}
  \item{mod}{
Switch to enable full tangential depth derivative (multiplication with \eqn{y_{n-1}}) in the test statistic (TRUE).
}

}

\value{
The function returns the simplicial depth of the parameter theta for an observed process y.
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
Kustosz C. and Mueller Ch. H. (2014). Analysis of crack growth with robust distribution-
free estimators and tests for nonstationary autoregressive processes. Statistical
Papers 55, 125-140.\cr
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
dS_lin1(theta,y)
}

