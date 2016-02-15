\name{dS1_lin2_test}
\alias{dS1_lin2_test}
\title{Test based on dS1 for explosive AR(1) processes with intercept
}
\description{
The function evaluates the asymptotic test based on \eqn{d_S^1} proposed in Kustosz, Mueller and Wendler (2014). It returns the test statistic and the decision. The main model is given by \deqn{Y_n = \theta_0 + \theta_1 Y_{n-1} + E_n} with \eqn{med(E_n)=0}.
}
\usage{
dS1_lin2_test(thetaN,alpha,y,exact=F,cpow=1)
}
\arguments{
  \item{thetaN}{
Parameter defining the Null hypothesis H0: theta = thetaN. Thereby thetaN = (thetaN0, thetaN1) as defined by the model. 
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
Fixed and known power parameter for the \deqn{Y_n = \theta_0 + \theta_1 Y_{n-1}^{cpow} + E_n} model.
}

}
\details{
The theoretical details can be found in Kustosz, Müller and Wendler (2016). The computational 
details are in Kustosz (2016).
}
\value{

\item{TS}{Returns the value of the rescaled and centred test statistic given by \eqn{N(dS-1/4)}.}
\item{phi}{Retuns the test decision, phi = 1 means reject H0, and phi = 0 means do not reject H0}
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
Kustosz C., Müller Ch. H. and Wendler M. (2016). Simplified Simplicial Depth for Regression and
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
y<-RandomARMod_lin2(1000,0.2,1.01,15,0)
dS1_lin2_test(c(0.2,1.01),0.05,y)
dS1_lin2_test(c(0.1,1.01),0.05,y)
}

