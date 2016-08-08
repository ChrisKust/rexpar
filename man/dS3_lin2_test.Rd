\name{dS3_lin2_test}
\alias{dS3_lin2_test}
\title{Test based on dS3 for explosive AR(1) processes with intercept
}
\description{
The function evaluates the asymptotic test based on dS3 proposed in Kustosz, Mueller and Wendler (2016). It returns the test statistic and the decision. The main model is given by \deqn{Y_n = \theta_0 + \theta_1 Y_{n-1} + E_n .}
}
\usage{
dS3_lin2_test(dS3, theta, alpha, y, exact = FALSE, cpow = 1)
}
\arguments{
  \item{dS3}{
Instead of parameters to calculate depth, here the dS3 statistic can be insterted directly. Then just the
decision function is evaluated, instad of a complete depth calculation.
}
  \item{theta}{
Parameter defining the Null hypothesis \eqn{H0: \theta = \theta^0}. Thereby \eqn{\theta^0} is defined by the model. 
}

  \item{alpha}{
Value in (0,1) defining the level of the test.  
}

  \item{y}{
Observed series \eqn{y=(y_0,...,y_N)} for which the parameter test has to be executed. 
}
\item{exact}{
This switch allows the usage of an exact distribition of the test statistics using the sample size. 
}
\item{cpow}{
fixed and known power parameter for the \eqn{Y_n = \theta_1*Y_{n-1}^{cpow} + \theta_0} model
}

\item{dS3}{
Here an evaluated dS3 statistic can be inserted instead of a vector y. Then just the test statistic is
calculated and the test is evaluated directly.}
}
\details{
The theoretical details can be found in Kustosz, Mueller and Wendler (2016). The computational 
details are in Kustosz (2016).
}
\value{

\item{TS}{Returns the value of the rescaled and centred test statistic.}
\item{phi}{Retuns the test decision, phi = 1 means reject H0, and phi = 0 means do not reject H0.}
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

\seealso{
 \code{\link{dS1_lin2}}
}

\examples{
y  <- RandomARMod_lin2(1000, 0.2, 1.01, 15, "0")
dS3_lin2_test(theta = c(0.2, 1.01), alpha = 0.05, y = y)
dS3_lin2_test(theta = c(0.1, 1.01), alpha = 0.05, y = y)
}

