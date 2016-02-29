\name{SimQuants}
\alias{SimQuants}
\title{
Simulated quantiles of the limit for simplicial depth based on alternating residuals for models with two parameters
}
\description{
Approximated quantiles of the limit process for simplicial depth based on alternating residuals in case of two dimensional parameters. The statistic is proposed in Kustosz, Leucht and Mueller (2016), where the limit distribution is derived, also. 
}
\usage{
SimQuants
}

\details{
The function just defines a 2 column matrix including probabilities and quantiles from the simulated
limit distribution of dS. The quantiles are calculated at an accuracy of 3 digits . Remarks on the acutal approximation are given in Kustosz (2016).
}
\value{
The resulting value is a matrix including probabilities in the first column and respective quantiles in the second. The function is used by the \code{\link{dS_lin2_test}}.
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
SimQuants
}

