\name{RandomARMod_nlin2}
\alias{RandomARMod_nlin2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ramdom non-linear AR(1) Series with three paramters
}
\description{
The function generates an random non-linear AR(1) process with given power, autoregression and starting
value. Further the errors can be specified by 4 predefined examples. The main equation 
is given by \deqn{y_n = y_n-1 + theta_1 * y_{n-1}^theta_2 + theta_0 + e_n,} \eqn{y_0} fixed.
}
\usage{
RandomARMod_nlin2(nobs,intercept=0,arp,power,start=0,cont=0,sd=0.1)
}
\arguments{
  \item{nobs}{
Number of observations for the process to generate
}
\item{intercept}{
Intercept parameter theta_0
}
  \item{arp}{
Autoregressive parameter theta_1
}
  \item{power}{
Power parameter theta_2
}
  \item{start}{
starting value of the process y_0
}
  \item{cont}{
error distribution defined by value in (0,1,2,3,4,5,6,7).

0 : e_n is i.i.d. N(0,sd) distributed

1 : e_n is i.i.d. N(0,sd) distributed, but in Pois(5/100) drawn points in time N(5,1) errors are added

2 : e_n is i.i.d. Gumbel distributed with parameters 10 and -3.3661513

3 : e_n is i.i.d. Frechet distributed with parameters 1.928, -2, 10

4 : e_n is i.i.d. Gamma distributed with paramters 1 and 1. Further the errors are centered by -0.6932 to satisfy a med(E_n)=0 condition

5 : 

6 : 

7 : 
}
\item{sd}{Defines the standard deviation of normally distributed errors for cont in \{0,1\}}
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
\code{\link{dS1_nlin2}}, \code{\link{dS2_nlin2}}, \code{\link{dS3_nlin2}}
}
\examples{
y<-RandomARMod_nlin2(300,0.001,0.005,1.002,15,0)
plot(y,type="l")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
