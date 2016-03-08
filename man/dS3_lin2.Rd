\name{dS3_lin2}
\alias{dS3_lin2}
\title{
dS3 for AR(1) with intercept
}
\description{
This function calculates a simplified version of simplicial depth for explosive AR(1) 
processes, when overlapping residuals are evaluated. Thereby
the parameter \eqn{\theta} and the process y are fixed. The assumed model given by the model defined by the model parameter.
}
\usage{
dS3_lin2(theta, res, y, model = c("linAR1", "linAR1woI", "nlinAR1",
"linAR2", "linARc"), cpow = 1)
}
\arguments{
  \item{theta}{
\eqn{\theta}, parameter vector to evaluate dS3 in.
}

  \item{res}{
Here the evaluated residuals can be inserted, instead of a parameter theta and data y. Then the statistic 
is evaluated based on these residuals by calculation of their signs.
}
  \item{y}{
\eqn{y = (y_0,...,y_N)}, oberserved process to evaluate dS3 in. 
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
Fixed and known power parameter for the \eqn{Y_n = \theta_1*Y_{n-1}^{cpow} + \theta_0} model.
}
}
\details{
The theoretical details can be found in Kustosz, Mueller and Wendler (2016). The computational 
details are in Kustosz (2016).
}
\value{
The result is a real number which gives the depth of theta based on the obervation y.
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
Christoph Kustosz and Sebastian Szugat
}
\note{
This expression is a simplification of dS, which is the full simplicial depth for explosive AR(1) processes
}


\seealso{
\code{\link{resARMod_lin2}}, \code{\link{dS_lin2}},\code{\link{dS1_lin2}}, \code{\link{dS2_lin2}}, \code{\link{dS3_lin2}}
}
\examples{
y <- RandomARMod_lin2(100, 0.02, 1.01, 15, "0")
theta <- c(0.02, 1.01)
dS3_lin2(theta = theta, y = y)
dS3_lin2(theta = theta + 0.1, y = y)
}
