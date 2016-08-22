\name{dS1_nlin2_test_b}
\alias{dS1_nlin2_test_b}
\title{Test based on dS1 for explosive nonlinear AR(1) processes with intercept for Batch-Jobs usage
}
\description{
The function evaluates the asymptotic test based on \eqn{d_S^1} proposed in Kustosz, Mueller and Wendler (2014). It returns the test statistic and the decision. The main model is given by \deqn{Y_n = \theta_1 Y_{n-1}^{\theta_2} + \theta_0 + E_n} with \eqn{med(E_n)=0}.
}
\usage{
dS1_nlin2_test_b(ind, theta, alpha, y, exact = FALSE, dS1)
}
\arguments{
\item{ind}{
Indicator to select specific Null hypothesis in the matrix thetaNv.
}
\item{theta}{
Parameter defining the Null hypothesis \eqn{H0: \theta = \theta^0} . Thereby \eqn{\theta^0 = (\theta_1^0,\theta_2^0,\theta_0^0)} as defined by the model. 
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
  \item{dS1}{
Instead of parameters to calculate depth, here the dS1 statistic can be insterted directly. Then just the
decision function is evaluated, instad of a complete depth calculation.
}
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
Kustosz, Christoph
}
\seealso{
 \code{\link{dS1_lin2}}
}
\examples{
y <- RandomARMod_nlin2(nobs = 100, intercept = 0, arp = 0.2, power = 1.01, start = 15, cont = "0")
theta <- rbind(c(0.2,1.01,0),c(0.2,1.01,0.5)) # (arp, power, intercept)
dS1_nlin2_test_b(ind = 1,theta = theta, alpha = 0.05, y = y)
dS1_nlin2_test_b(ind = 2,theta = theta, alpha = 0.05, y = y)
}

