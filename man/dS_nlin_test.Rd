\name{dS_nlin_test}
\alias{dS_nlin_test}
\title{Test based on dS for explosive nonlinear AR(1) processes with intercept
}
\description{
The function evaluates the asymptotic test based on dS proposed in Kustosz, Leucht and Mueller (2014). It returns the test statistic and the decision. The main model is given by \deqn{Y_n = \theta_1 Y_{n-1}^{\theta_2} + E_n} with \eqn{med(E_n)=0}.
}
\usage{
dS_nlin_test(thetaN, alpha, y, ncores)
}
\arguments{
  \item{thetaN}{
Parameter defining the Null hypothesis \eqn{H0: \theta = \theta^0}. Thereby \eqn{\theta} is defined by the model. 
}

  \item{alpha}{
Value in (0,1) defining the level of the test.  
}

  \item{y}{
Observed series \eqn{y=(y_0,...,y_N)} for which the parameter test has to be executed. 
}
  \item{ncores}{
Defines the number of cores to use for parallel computation of the simplicial depth statistic. If no value is definied the standard value of 1 is passed and no parallel methods are applied.
}

}
\details{
The theoretical details can be found in Kustosz, Leucht and Mueller (2016) and in Kustosz, Mueller and Wendler (2016). The computational 
details are in Kustosz (2016).
}
\value{

\item{TS}{Returns the value of the rescaled and centred test statistic}
\item{phi}{Retuns the test decision, \code{phi = 1} means reject H0, and \code{phi = 0} means do not reject H0}
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
Kustosz C., Leucht A. and Mueller Ch. H. (2016). Tests based on simplicial depth for AR(1) models with
explosion. Journal of Time Series Analysis. In press.\cr
\cr
Kustosz C., Mueller Ch. H. and Wendler M. (2016). Simplified Simplicial Depth for Regression and
Autoregressive Growth Processes. Journal of Statistical Planning and Inference. In press.\cr
\cr
}
\author{
Kustosz, Christoph
}
\note{
Note, that the evaluation depends on the simulated results of the limit process, which are included in the rexpar package.
}

\seealso{
\code{\link{dS_lin2}},
}
\examples{
y <- RandomARMod_nlin1(200, 0.1, 1.01, 15, "0")
dS_nlin_test(thetaN = c(0.1, 1.01), alpha = 0.05, y = y)
dS_nlin_test(thetaN = c(0.2, 1.01), alpha = 0.05, y = y)
}

