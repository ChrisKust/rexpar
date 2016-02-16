\name{dS_lin2_test}
\alias{dS_lin2_test}
\title{Test based on dS for explosive AR(1) processes with intercept
}
\description{
The function evaluates the asymptotic test based on dS proposed in Kustosz, Leucht and M\"{u}ller (2016). It returns the test statistic and the decision. The default model is given by \deqn{Y_n = \theta_0 + \theta_1 Y_{n-1}^{cpow} + E_n} with \eqn{med(E_n)=0}.
}
\usage{
dS_lin2_test(thetaN,alpha,y,ncores=1,cpow=1,dS)
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
\item{cpow}{
Fixed and known power parameter for the \eqn{Y_n = \theta_2\cdot Y_{n-1}^{cpow} + \theta_0} model.
}

\item{dS}{
Here an evaluated depth statistic dS can be defined, instead of evaluation of data y, to calculate the test
statistic for dS directly.
}

}
\details{
The theoretical details can be found in Kustosz, Leucht and M\"{u}ller (2016). The computational 
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
Kustosz C., Leucht A. and M\"{u}ller Ch. H. (2016). Tests based on simplicial depth for AR(1) models with
explosion. Journal of Time Series Analysis. In press.\cr
\cr
}
\author{
Christoph Kustosz and Sebastian Szugat
}
\note{
Note, that the evaluation depends on the simulated results of the limit process, which are included in the rexpar package.
}

\seealso{
\code{\link{dS_lin2}},
}
\examples{
data("simLimit")
y<-RandomARMod_lin2(100,0.2,1.01,15,0)
dS_lin2_test(c(0.2,1.01),0.05,y)
dS_lin2_test(c(0.1,1.01),0.05,y)

}

