\name{ols_test}
\alias{ols_test}
\title{
OLS test for the two sided alternative for parameters of an explosive AR(1) process with intercept
}
\description{
The function calculates the test decision on for a given two sided hypothesis \eqn{H_0: \theta = \theta^0} based on an observed process y and level alpha for linear AR(1) processes with intercept and explosion.
}
\usage{
ols_test(theta0, y, alpha)
}
\arguments{
  \item{theta0}{
Parameter \eqn{\theta^0} to define the Null-Hypothesis.
}
  \item{y}{
Observed process \eqn{y=(y_0,...,y_N)}.
}
  \item{alpha}{
Level \eqn{alpha} of the test.
}
}
\details{
The theoretical details can be found in Wang and Yu (2013). Remarks on the implementation are in Kustosz (2016).
}
\value{
The function returns a boolean denoting the test decision by TRUE for reject and FALSE for not-reject.
}

\seealso{
\code{\link{ols_expl}},\code{\link{ols_ts}}
}
\references{
Wang X. and Yu, J. (2013). Limit theory for an explosive autoregressive process.
Working Paper, No 08-2013. Singapore Management University, School of Economics.\cr
\cr
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
}
\author{
Christoph Kustosz and Sebastian Szugat
}
