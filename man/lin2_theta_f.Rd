\name{lin2_theta_f}
\alias{lin2_theta_f}
\title{
Candidate point evaluation for simplicial depth in the linear model
}
\description{
This function precalculates parameters on which depth changes to restrict the
region on which the depth statistic has to be evaluated on as described in Kustosz (2016).
}
\usage{
lin2_theta_f(dat)
}
\arguments{
  \item{dat}{
Data from an AR(1) Model with intercept.
}
}
\details{
The details on the idea and implementation can be found in Kustosz (2016).}

\value{
\item{t1}{Candidate values for theta1.}
\item{t2}{Candidate values for theta2.}
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
}
\author{
Kustosz, Christoph
}

\seealso{
\code{\link{dS_lin2}},\code{\link{dS1_lin2}},\code{\link{dS2_lin2}},\code{\link{dS3_lin2}}, \code{\link{lin2_CI}},\code{\link{est_lin2}},\code{\link{est_nlin1}}
}
\examples{
y <- RandomARMod_lin2(100, 1.001, 0.2, 15, "0")
thetas <- lin2_theta_f(y)
plot(thetas$t1, thetas$t2)
}

