\name{nlin1_theta_f}
\alias{nlin1_theta_f}
\title{
Candidate point evaluation for simplicial depth in the non-linear model
}
\description{
This function precalculates parameters on which depth changes to restirict the
region on which the depht statistic has to be evaluated on.
}
\usage{
nlin1_theta_f(dat)
}
\arguments{
  \item{dat}{
Data from an non-linear AR(1) Model without intercept.
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
\code{\link{dS_lin2}},\code{\link{dS1_lin2}},\code{\link{dS2_lin2}},\code{\link{dS3_lin2}},\code{\link{est_nlin1}}
}
\examples{
y <- RandomARMod_nlin1(100, 1.001, 0.2, 15, 0)
thetas <- nlin1_theta_f(y)
}
