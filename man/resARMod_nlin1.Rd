\name{resARMod_nlin1}
\alias{resARMod_nlin1}
\title{
Residuals of a non-linear autoregressive process without intercept
}
\description{
The function calculates the residuals of an AR(1) process
with intercept for given parameter and data based on
\deqn{r_n = y_n - y_{n-1} - \theta_1 y_{n-1}^{\theta_2}}
}
\usage{
resARMod_nlin1(theta,dat)
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

\details{
Details can be found in Kustosz (2016).
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
