\name{dS_lin1_test}
\alias{dS_lin1_test}
\title{Test based on dS for explosive AR(1) processes without intercept
}
\description{
The function evaluates the asymptotic test based on dS proposed in Kustosz and Mueller (2014). It returns the test statistic and the decision. The main model is given by \deqn{Y_n = \theta_1 Y_{n-1} + E_n} with \eqn{med(E_n)=0}.
}
\usage{
dS_lin1_test(thetaN, alpha, y, mod = FALSE)
}
\arguments{
  \item{thetaN}{
Parameter defining the Null hypothesis \eqn{H0: \theta = \theta^0}.
}

  \item{alpha}{
Value in (0,1) defining the level of the test.  
}

  \item{y}{
Observed series \eqn{y=(y_0,...,y_N)} for which the parameter test has to be executed. 
}
  \item{mod}{
Switch to enable full tangential depth derivative (multiplication with \eqn{y_{n-1}}) in the test statistic, if \code{mod= TRUE}.
}


}
\details{
The theoretical details can be found in Kustosz and Mueller (2014). The computational 
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
Kustosz C. and Mueller Ch. H. (2014). Analysis of crack growth with robust distribution-
free estimators and tests for nonstationary autoregressive processes. Statistical
Papers 55, 125-140.\cr
}
\author{
Kustosz, Christoph
}



\seealso{
\code{\link{dS_lin1}},
}

\examples{
y <- RandomARMod_lin2(100, 0, 1.01, 15, 0)
theta <- 1.01
dS_lin1_test(thetaN=c(0, 1.01), alpha=0.05, y=y)
}