\name{dS1_lin2}
\alias{dS1_lin2}
\title{
dS1 for AR(1) with intercept
}
\description{
This function calculates a simplified version of simplicial depth for explosive AR(1) 
processes, when non overlapping residuals in sequences of 3 are evaluated. Thereby
the parameter \eqn{\theta_1} and the process y are fixed. The assumed model given by the argument of the parameter model. 
}
\usage{
dS1_lin2(theta, y, model, cpow=1)
}
\arguments{
  \item{theta}{
\eqn{\theta}, parameter (vector) to evaluate dS1 in.
}
  \item{y}{
\eqn{y = (y_0,...,y_N)}, oberserved process to evaluate dS1 in. 
}
  \item{model}{
Here the model for the calculation of the underlying residuals is specified. currently the following
models are available\cr
"linAR1woI" = linear AR(1) model without intercept          \deqn{Y_n = \theta_1 Y_{n-1} + E_n}
"linAR1" = linear AR(1) model with intercept                 \deqn{Y_n = \theta_1 Y_{n-1} \theta_0 + E_n}
"linAR2" = linear AR(2) model without intercept               \deqn{Y_n = \theta_1 Y_{n-1} + \theta_2 Y_{n-2} + E_n}
"nlinAR1" = linear AR(1) model without intercept but with power parameter\deqn{Y_n = Y_{n-1} + \theta_1 Y_{n-1}^{\theta_3} + E_n}
"linARc" = linear AR(1) model with intercept and fixed and knwon power cpow \deqn{Y_n = \theta_1 Y_{n-1}^{cpow} + \theta_0 + E_n}
}
\item{cpow}{
fixed and known power parameter for the \eqn{Y_n = \theta_1 Y_{n-1}^{cpow} + \theta_0 + E_n}.
}
}
\details{
The theoretical details can be found in Kustosz, Mueller and Wendler (2016). The computational 
details are in Kustosz (2016).
}
\value{
The result is a real number which gives the depth of \eqn{\theta} based on the obervation vector y.
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
Kustosz C., Mueller Ch. H. and Wendler M. (2016). Simplified Simplicial Depth for Regression and
Autoregressive Growth Processes. Journal of Statistical Planning and Inference. In press.\cr
\cr
}
\author{
Kustosz, Christoph
}
\note{
This expression is a simplification of dS, which is the full simplicial depth for explosive AR(1) processes
}


\seealso{
\code{\link{resARMod_lin2}}, \code{\link{dS_lin2}},\code{\link{dS1_lin2}}, \code{\link{dS2_lin2}}, \code{\link{dS3_lin2}}
}
\examples{
y<-RandomARMod_lin2(100, 0.2, 1.01 ,15 ,0)
theta <- c(0.2, 1.01)
dS1_lin2(theta, y)
}

