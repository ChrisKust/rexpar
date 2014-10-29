\name{RandomARMod_lin2}
\alias{RandomARMod_lin2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ramdom AR(1) Series with Intercept
}
\description{
The function generates an random AR(1) process with given intercept, slope and starting
value. Further the errors can be specified by 4 predefined examples. The main equation 
is given by \deqn{y_n = theta_1 * y_{n-1} + theta_0 + e_n,} \eqn{y_0} fixed.
}
\usage{
RandomARMod_lin2(nobs,intercept,arp,start,cont)
}
\arguments{
  \item{nobs}{
number of observations for the process to generate
}

  \item{intercept}{
intercept parameter theta_0
}
  \item{arp}{
autoregressive parameter theta_1
}
  \item{start}{
starting value of the process y_0
}
  \item{cont}{
error distribution defined by value in (0,1,2,3,4).

0 : e_n is i.i.d. N(0,0.1) distributed

1 : e_n is i.i.d. N(0,0.1) distributed, but in Pois(5/100) drawn points in time N(5,1) errors are added

2 : e_n is i.i.d. Gumbel distributed with parameters 10 and -3.3661513

3 : e_n is i.i.d. Frechet distributed with parameters 1.928, -2, 10

4 : e_n is i.i.d. Gamma distributed with paramters 1 and 1. Further the errors are centered by -0.6932 to satisfy a med(E_n)=0 condition
}
}
\details{
All error distributions are chosen to satistify med(E_n)=0. 
}
\value{
the function returns a vector (y_0,...,y_N) which is a simulation of the AR process given by the input paramters
}
\references{
Kustosz, C. (2014). Implementation of simplicial depth for AR processes under non standard conditions. Working Paper, TU Dortmund.
}
\author{
Kustosz, Christoph
}
%\note{
%
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dS1_lin2}}, \code{\link{dS2_lin2}}, \code{\link{dS3_lin2}}, \code{\link{dS_lin2}}
}
\examples{
y<-RandomARMod_lin2(100,0.02,1.01,5,3)
plot(y,type="l")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
