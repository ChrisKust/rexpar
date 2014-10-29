\name{resARMod_nlin1}
\alias{resARMod_nlin1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
resARMod_nlin1
}
\description{
The function calculates the residuals of an AR(1) process
with intercept for given parameter and data based on
\deqn{r_n = y_n - y_{n-1} - theta_1 y_{n-1}^theta_3}
}
\usage{
resARMod_nlin1(theta,dat)
}

\arguments{
  \item{theta}{
  parameter vector consisting of (theta_1,theta_3)
}
  \item{dat}{
  one dimensional observation vector $y=(y_0,...,y_N)$
}
}
\value{
The output is a vector with $N$ elements defined by $r = (r_1,...,r_N)$

}
\references{
Kustosz, C. (2014). Implementation of simplicial depth for AR processes under non standard conditions. Working Paper, TU Dortmund.
}
\author{
Kustosz, Christoph
}
%\note{
%}
\seealso{
\code{\link{RandomARMod_nlin1}},\code{\link{dS_lin2}},\code{\link{dS1_lin2}},\code{\link{dS2_lin2}},\code{\link{dS3_lin2}}
}


\examples{
## Minimal Example
y<-c(1,2,3)
theta<-c(1,1)
resARMod_nlin1(theta,y)

## Generating an example process in advance
theta<-c(0.002,1.001)
N<-100
y0<-1
y<-RandomARMod_nlin1(N,theta,y0)

resARMod_nlin1(theta,y)->res
par(mfrow=c(2,1))
plot(res)
hist(res)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{residuals }
\keyword{autoregression}% __ONLY ONE__ keyword per line
