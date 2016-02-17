
\name{oner_sym}
\alias{oner_sym}
\title{Main evaluation of a symmetrised simplicial depth statistic dS
}
\description{
This function is a central part of the calculation of dS in the symmetrised version. It is necessary to calculate the matrix based inner loops.
}
\usage{
oner_sym(resid)
}
\arguments{
  \item{resid}{A vector of residuals \eqn{(r_k,...,r_{k+m})} for which the inner loop has to be evaluated.
}
}
\details{
The details on the calculation can be found in Kustosz (2016).
}
\value{
The function returns the sum of all indicators, when the first residual of the input vector is fixed and the inner sums run over all remaining residuals.
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
}
\author{
Kustosz, Christoph
}
\note{
The funcion is used in the symmetrised dS calculation routine. 
}


\seealso{
\code{\link{find1}}, \code{\link{find2}},\code{\link{intfun2}}, \code{\link{oner}}, \code{\link{inv_tri}}
}
\examples{
res<-c(1,2,3,4,5)
oner_sym(res)
}
