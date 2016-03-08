\name{lin1_theta}
\alias{lin1_theta}
\title{
Candidate point evaluation for simplicial depth in the linear model without intercept
}
\description{
This function precalculates parameters on which depth changes to restirict the
region on which the depht statistic has to be evaluated on as described in Kustosz and Mueller(2014).
}
\usage{
lin1_theta(dat)
}
\arguments{
  \item{dat}{
Data from an AR(1) Model without intercept
}
}


\value{
\item{t1}{Candidate values for theta1}
\item{t2}{Candidate values for theta2}
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
Kustosz C., Mueller Ch. H. and Wendler M. (2016). Simplified Simplicial Depth for Regression and
Autoregressive Growth Processes. Journal of Statistical Planning and Inference. In press.\cr
\cr
Kustosz C. and Mueller Ch. H. (2014). Analysis of crack growth with robust distribution-
free estimators and tests for nonstationary autoregressive processes. Statistical
Papers 55, 125-140.\cr
}
\author{
Kustosz, Christoph
}


\seealso{
\code{\link{dS_lin2}},\code{\link{dS1_lin2}},\code{\link{dS2_lin2}},\code{\link{dS3_lin2}}, \code{\link{lin2_CI}},\code{\link{est_lin2}},\code{\link{est_nlin1}}
}
\examples{
y <- RandomARMod_lin2(100, 1.001, 0, 15, "0")
thetas <- lin1_theta(y)
summary(thetas$t1)
}
