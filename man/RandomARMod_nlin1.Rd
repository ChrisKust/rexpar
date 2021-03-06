\name{RandomARMod_nlin1}
\alias{RandomARMod_nlin1}
\title{
Ramdom non-linear AR(1) Series 
}
\description{
The function generates an random non-linear AR(1) process with given power, autoregression and starting
value. Further the errors can be specified by 4 predefined examples. The main equation 
is given by \deqn{Y_n = Y_{n-1} + \theta_1 * Y_{n-1}^{\theta_2} + E_n,}, whereby \eqn{E_n} are i.i.d with \eqn{med(E_n)=0} and \eqn{y_0} is fixed and known.
}
\usage{
RandomARMod_nlin1(nobs, arp, power = 1, start = 0, cont = "0", sd = 0.2)
}
\arguments{
  \item{nobs}{
Number of observations for the process to generate.
}

  \item{arp}{
Autoregression parameter \eqn{\theta_1}.
}
  \item{power}{
Power parameter \eqn{\theta_2}.
}
  \item{start}{
Starting value of the process \eqn{y_0}.
}
  \item{cont}{
Error distribution defined by value in ("0","1","2","3","4").
\cr
"0" : E_n is i.i.d. N(0,sd) distributed.
\cr
"1" : E_n is i.i.d. N(0,sd) distributed, but in Pois(5/100) drawn points in time N(5,1) errors are added.
\cr
"2" : E_n is i.i.d. Gumbel distributed with parameters 10 and -3.665129.
\cr
"3" : E_n is i.i.d. Frechet distributed with parameters 1.928, -2, 10.
\cr
"4" : E_n is i.i.d. Gamma distributed with paramters 1 and 1. Further the errors are centered by -0.6932 to satisfy a med(E_n)=0 condition.
\cr
"5" :  E_n is i.i.d. N(0,1) distributed, but in Pois(2/200) drawn points in time the value 150 is added.
\cr
"6" : E_n is i.i.d. Gumbel distributed with parameters 100 and -36.65129.
\cr
"7" : Here the errors are neglected and just the determinisic process is generated.
}
\item{sd}{Defines the standard deviation of normally distributed errors for cont in \{0,1\}}.
}
\details{
All error distributions are chosen to satistify \eqn{med(E_n)=0}. 
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
set.seed(124)
y <- RandomARMod_nlin1(nobs = 300, arp = 0.005, power = 1.002, start = 15, cont = "0")
y[1:10]
plot(y, type="l")
}
