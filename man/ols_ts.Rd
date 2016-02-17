\name{ols_ts}
\alias{ols_ts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
OLS test statisitic for explosive AR(1) processes
}
\description{
The function comutes an OLS test statistic as described in Wang and Yu (2013) to derive an OLS based test for explosive AR processes.
}
\usage{
ols_ts(y, thetaT)
}
\arguments{
  \item{y}{
Observed AR(1) process\eqn{y=(y_0,...,y_N)} with intercept.
}
  \item{thetaT}{
Parameter \eqn{\theta} to calculate the OLS statistic in. 
}
}
\details{
The theoretical details can be found in Wang and Yu (2013). Remarks on the implementation are in Kustosz (2016).
}
\value{
\item{Ttheta0 }{OLS test statistic for the intercept parameter.}
\item{Ttheta1}{OLS test statistic for the AR parameter.}

}

\seealso{
\code{\link{ols_expl}},\code{\link{ols_test}}
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

