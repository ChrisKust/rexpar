\name{qvals}
\alias{qvals}
\title{
Quantiles of the limit distribution of simplicial depth for processes with two dimensional parameters
}
\description{
Approximated quantiles of the limit process for simplicial depth based on alternating residuals in case of two dimensional parameters. The statistic is proposed in Kustosz, Leucht and Mueller (2016), where the limit distribution is derived, also. 
}
\usage{
qvals
}

\details{
The function just defines a vector including quantiles from the simulated
limit distribution of dS. The quantiles are calculated at a accuracy of 3 digits. Remarks on the approximation can also be found in Kustosz (2016).
}
\value{
The resulting value is a vector including quantiles . The function is used by the dS_lin2_test() function indirectly.
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

\seealso{
\code{\link{dS_lin2_test}}
}
\examples{
qvals
}

