\name{dS_lin2_sym}
\alias{dS_lin2_sym}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dS for AR(1) with two parameters in a symmetrised version
}
\description{
This function calculates the  simplicial depth for explosive AR(1) 
processes with two parameters. Thereby the parameter \eqn{\theta} and the process y are fixed. The assumed model given by the argument model. Further the kernel is replaced by a symmetrised version. This in particular allows the application of standard limit theorems for U-Statistics.
}
\usage{
dS_lin2_sym(theta, resy, y, ncores = 1, 
model = c("linAR1", "nlinAR1", "linARc"), cpow = 1)
}
\arguments{
  \item{theta}{
\eqn{\theta}, parameter (vector) to evaluate dS1 in.
}

\item{resy}{
Instead of a model and a parameter theta, residuals can be plugged in directly. Then just the sign changes 
are calculated and the statistic is evaluated

}
  \item{y}{
\eqn{y = (y_0,...,y_N)}, oberserved process to evaluate dS1 in. 
}

\item{ncores}{
This value allows the usage of multiple cores to calculate the statistic
}

  \item{model}{
Here the model for the calculation of the underlying residuals is specified. currently the following
models are available\cr
"linAR1" = linear AR(1) model with intercept                 \deqn{Y_n = \theta_1 Y_{n-1} \theta_0 + E_n}
"nlinAR1" = linear AR(1) model without intercept but with power parameter\deqn{Y_n = Y_{n-1} + \theta_1 Y_{n-1}^{\theta_3} + E_n}
"linARc" = linear AR(1) model with intercept and fixed and knwon power cpow \deqn{Y_n = \theta_1 Y_{n-1}^{cpow} + \theta_0 + E_n}
}

\item{cpow}{
fixed and known power parameter for the y(n) = theta2*y(n)^cpow + theta1 model
}
}
\details{
The theoretical details can be found in Kustosz, M\"{uller} and Wendler (2016). The computational 
details are in Kustosz (2016).
}
\value{
Result is a real number which gives the depth of theta based on the obervation y.
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
Kustosz C., M\"{uller} Ch. H. and Wendler M. (2016). Simplified Simplicial Depth for Regression and
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
y <- RandomARMod_lin2(100, 0.2, 1.01, 15, "0")
theta <- c(0.2, 1.01)
dS_lin2_sym(theta = theta, y = y, model = "linAR1")
dS_lin2_sym(theta = theta+0.1, y = y, model = "linAR1")
}

