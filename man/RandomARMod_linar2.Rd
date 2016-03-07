\name{RandomARMod_linar2}
\alias{RandomARMod_linar2}
\title{
Ramdom linear AR(2) Series 
}
\description{
The function generates an random linear AR(2) process with given autoregression parameters and starting
value. Further the errors can be specified by 4 predefined examples. The main equation 
is given by \deqn{Y_n = \theta_1 * Y_{n-1} + \theta_2 * Y_{n-2} + E_n,}, whereby \eqn{E_n} are i.i.d with \eqn{med(E_n)=0} and \eqn{y_0} is fixed and known.
}
\usage{
RandomARMod_linar2(nobs, arp1, arp2, start, cont)
}
\arguments{
  \item{nobs}{
Number of observations for the process to generate.
}

  \item{arp1}{
Autoregression parameter \eqn{\theta_1}.
}
  \item{arp2}{
Autoregression parameter \eqn{\theta_2}.
}
  \item{start}{
Starting value of the process \eqn{y_0, y_1} given by a vector c(y0,y1).
}
  \item{cont}{
Error distribution defined by value in (0,1,2,3,4).
\cr
0 : E_n is i.i.d. N(0,sd) distributed.
\cr
1 : E_n is i.i.d. N(0,sd) distributed, but in Pois(5/100) drawn points in time N(5,1) errors are added.
\cr
2 : E_n is i.i.d. Gumbel distributed with parameters 10 and -3.665129.
\cr
3 : E_n is i.i.d. Frechet distributed with parameters 1.928, -2, 10.
\cr
4 : E_n is i.i.d. Gamma distributed with paramters 1 and 1. Further the errors are centered by -0.6932 to satisfy a med(E_n)=0 condition.
\cr
}
}
\details{
All error distributions are chosen to satistify med(E_n)=0. Remarks on the error distributions can be found in Kustosz (2016).
}
\value{
the function returns a vector \eqn{(y_0,...,y_N)} which is a simulation of the AR process given by the input paramters
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
}
\author{
Kustosz, Christoph
}

\seealso{
\code{\link{dS1_lin2}}, \code{\link{dS2_lin2}}, \code{\link{dS3_lin2}}, \code{\link{dS_lin2}}
}
\examples{
y <- RandomARMod_linar2(100, 1.001, 1.001, c(5, 5), 1)
plot(y, type="l")
}
