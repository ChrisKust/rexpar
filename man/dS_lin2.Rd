\name{dS_lin2}
\alias{dS_lin2}
\title{Simplicial depth for explosive AR(1) processes
}
\description{
This function calculates simplicial depth for explosive AR(1) processes as defined in Kustosz, Mueller and Leucht (2016). Thereby it is capable to autodetects the number of available cores or uses a user defined number of cores to speed up computation. The basic model is defined by \deqn{Y_n = \theta_0 + \theta_1 Y_ {n-1}+E_n}, with \eqn{Y_n} being an increasing process and \eqn{E_n} satisfying \eqn{med(E_n) = 0}.
}
\usage{
dS_lin2(theta, resy, y, ncores = 1,
model = c("linAR1", "nlinAR1", "linARc"), cpow = 1)
}
\arguments{
  \item{theta}{
Parameter \eqn{\theta} for which simplicial depth has to be evaluated. Thereby the dimension of the parameter is defined by the selected model.
}

  \item{resy}{
Here residuals of an arbitrary model with two parameter can be inserted, instead of defining a parameter theta and y, to evaluate depth directly on the signs of these residuals.
}
  \item{y}{
Observed proces \eqn{y=(y_0,...,y_N)} for which simplicial depth has to be evaluated.
}
  \item{ncores}{
Number of cores for parallel computation. If no value is selected the method sets the cores to 1 and no parallel comutation is applied. If the value is set to 'auto', the function detects the available number of cores and uses the maximal number minus 1. The function produces errors, if more cores than available or a wrong argument is inserted.
}
   \item{model}{
Here the model for the calculation of the underlying residuals is specified. currently the following
models are available\cr
"linAR1" = linear AR(1) model with intercept                 \deqn{Y_n = \theta_1 Y_{n-1} \theta_0 + E_n}
"nlinAR1" = linear AR(1) model without intercept but with power parameter\deqn{Y_n = Y_{n-1} + \theta_1 Y_{n-1}^{\theta_3} + E_n}
"linARc" = linear AR(1) model with intercept and fixed and knwon power cpow \deqn{Y_n = \theta_1 Y_{n-1}^{cpow} + \theta_0 + E_n}
}
\item{cpow}{
Fixed and known power parameter for the \eqn{Y_n = \theta_1*Y_{n-1}^{cpow} + \theta_0} model.
}
}

\details{
The theoretical details can be found in Kustosz, Leucht and Mueller (2016). The computational 
details are in Kustosz (2016).
}

\value{
The function returns the simplicial depth of the parameter theta for an observed process y.
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
Kustosz C., Leucht A. and Mueller Ch. H. (2016). Tests based on simplicial depth for AR(1) models with
explosion. Journal of Time Series Analysis. In press.\cr
\cr
}
\author{
Christoph Kustosz and Sebastian Szugat
}
\note{
The function depends on several internal functions.
}



\seealso{
\code{\link{resARMod_lin2}}, \code{\link{dS_lin2}},\code{\link{dS1_lin2}}, \code{\link{dS2_lin2}}, \code{\link{dS3_lin2}}, \code{\link{oner}}
}
\examples{
y <- RandomARMod_lin2(100, 0.2, 1.01, 15, 0)
theta <- c(0.2, 1.01)
 \dontrun{
dS_lin2(theta, y)
}
}

