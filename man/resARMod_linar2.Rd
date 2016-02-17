\name{resARMod_linar2}
\alias{resARMod_linar2}
\title{
Residuals of a AR(2) process without intercept
}
\description{
The function calculates the residuals of an AR(2) process
without intercept for given parameter and data based on
\deqn{r_n = y_n - \theta_1 y_{n-1} - \theta_2 y_{n-2}}
}
\usage{
resARMod_linar2(theta,dat)
}

\arguments{
  \item{theta}{
  Parameter vector \eqn{\theta= (\theta_1,\theta_2)}.
}
  \item{dat}{
  One dimensional observation vector \eqn{y=(y_0,...,y_N)}.
}
}
\value{
The output is a vector with N elements defined by \eqn{r = (r_1,...,r_N)}.

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
\code{\link{RandomARMod_linar2}},\code{\link{dS_lin2}},\code{\link{dS1_lin2}},\code{\link{dS2_lin2}},\code{\link{dS3_lin2}}
}


\examples{
## Minimal Example
y<-c(1,2,3)
theta<-c(1,1)
resARMod_linar2(theta,y)

## Generating an example process in advance
theta<-c(0.002,1.001)
N<-100
y0<-1
y<-RandomARMod_linar2(N,theta,y0)

resARMod_linar2(theta,y)->res
par(mfrow=c(2,1))
plot(res)
hist(res)


}

