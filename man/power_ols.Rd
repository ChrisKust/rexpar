\name{power_ols}
\alias{power_ols}
\title{
Power of an OLS test for explosive AR processes
}
\description{
This function simulates the power of an OLS test for explosive AR processes discussed in Wang and Yu (2013).
}
\usage{
power_ols(thetas, N, R, sv, cont, theta0, alpha)
}
\arguments{
  \item{thetas}{
Parameter \eqn{\theta_k = (\theta_{0,k},\theta_{1,k})} with which processes are simulated to evaluate the power in.
}
  \item{N}{
Size of the simulated processes.
}
  \item{R}{
  Number of repetitions in the simulation study.
}
  \item{sv}{
Starting value \eqn{y_0} for the simulated processes.
}
  \item{cont}{
Contamination distribution as defined by RandomARMod_lin2.
}
  \item{theta0}{
Parameter to define the Null-Hypothesis \eqn{H0: \theta = \theta^0} of the considered two-sided test.
}
  \item{alpha}{
Level \eqn{\alpha} of the test
}
}
\details{
The theoretical details can be found in Wang and Yu (2013). Remarks on the implementation are in Kustosz (2016).
}
\value{
The output is a simulated value of the power function for the test in \eqn{H0: \theta = \theta^0} evaluated in thetas.
}

\seealso{
\code{\link{ols_expl}},\code{\link{ols_test}},\code{\link{ols_ts}}
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
Kustosz, Christoph}

