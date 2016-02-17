\name{ols_expl}
\alias{ols_expl}
\title{
OLS Estimation for explosive AR(1) processes with intercept
}
\description{
OLS Estimation for explosive AR(1) processes with intercept
}
\usage{
ols_expl(y)
}
\arguments{
  \item{y}{
Observed process \eqn{y=(y_0,...,y_N)}.
}
}
\details{
The theoretical details can be found in Wang and Yu (2013). Remarks on the implementation are in Kustosz (2016).
}
\value{
\item{theta0}{Estimated intercept.}
\item{theta1}{Estimated ar parameter.}
\item{sigma2}{Estimated variance based on the estimated residuals.}
}

\seealso{
\code{\link{ols_test}},\code{\link{ols_ts}}
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
