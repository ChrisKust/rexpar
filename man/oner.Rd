\name{oner}
\alias{oner}
\title{Main evaluation of the dS statistic
}
\description{
This function is a central part of the calculation of dS. It is necessary to calculate the matrix based inner loops. The statistic is the full simplicial depth with two paramteters, i.e. three residuals in each term.
}
\usage{
oner(resid)
}
\arguments{
  \item{resid}{A vector of residuals \eqn{(r_k,...,r_{k+m})} for which the inner loop has to be evaluated.
}
}
\details{
The details on the calculation can be found in Kustosz (2016). The theoretical idea is presented in Kustosz, Leucht and Mueller (2016).
}
\value{
The function returns the sum of all indicators, when the first residual of the input vector is fixed and the inner sums run over all remaining residuals.
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
Kustosz C., Leucht A. and Mueller Ch. H. (2016). Tests based on simplicial depth for AR(1) models with
explosion. Journal of Time Series Analysis. In press.\cr
\cr
}
\author{
Kustosz, Christoph
}
\note{
The funcion is used in the dS calculation routine.
}


\seealso{
\code{\link{find1}}, \code{\link{find2}},\code{\link{intfun2}}, \code{\link{oner}}, \code{\link{inv_tri}}
}
\examples{
res<-c(1,2,3,4)
oner(res)

y<-RandomARMod_lin2(100,0.2,1.01,15,0)
res<-resARMod_lin2(c(0.2,1.01),y)
oner(res)

}

