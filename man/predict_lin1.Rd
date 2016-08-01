\name{predict_lin1}
\alias{predict_lin1}
\title{Bootstrap prediction for linear AR(1) model without intercept
}
\description{
The function allows a prediction of a linear AR(1) model without intercept based
on simplicial depth and residual bootstrapping. The method is introduced in Kustosz (2016) and applies the simplical depth for AR(1) processes without intercept to generate estimates and confidence sets. 
}
\usage{
predict_lin1(y, CritLen, CritTime, NSim, alpha, restrict = FALSE, eps = 1e-9)
}
\arguments{
  \item{y}{
Observed series on which the parameter estimates are calculated. A prediction for future values of this series are produced. 
}
  \item{CritLen}{
A value for a critical length for which the time of arrival then can be predicted by the function. This argument is optional.
}
  \item{CritTime}{
  A value for a critical time (as index of the process), exceeding the size of y, for which the process value is predicted.
}
  \item{NSim}{
  Number of simulations used in the bootstrap simulations of the continued process.
}
  \item{alpha}{
A level which defines  the \eqn{1-\alpha} prediction intervals.
}
  \item{restrict}{
A switch, which allows to activate a restriction for parameter simulations used in the bootstrap procedure. If restrict is activatet (TRUE) 1-alpha confidence intervals are used to generate simulated paremeters based on the empirical simplicial depth. If restrict is off (FALSE), the complete candidate set is used for simulations. Thereby the probabilities of the candidates rely on the empirical simplicial depth. 
}
\item{eps}{
Parameter to shift candidates to differ from the roots of the residuals.
}
}

\value{
\item{estimation_time }{Estimated tome to arrive at the critical crack lenght.}
\item{estimation_lenght}{Estimated length at the critical time.}
\item{mean_CT}{Mean estimate of the critical lenght of arrival at CritTime.}
\item{med_CT}{Median estimate of the critical lenght of arrival at CritTime.}
\item{mean_CL}{Mean estimate of the critival time length at CritLen.}
\item{med_CL}{Median estimate of the critical time at CritLen.}
\item{confintCT}{1-alpha prediction interval for the estimate of the critical time.}
\item{confintCL}{1-alpha prediction interval for the estimate of the citical length.}
\item{alpha}{1-Level of the confidence intervals.}
\item{simulations}{A matrix including all simulated continuations of the process.}


}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
}
\author{
Kustosz, Christoph and Szugat, Sebastian
}


\seealso{
 \code{\link{draw_from_depth}}}

\examples{
y <- RandomARMod_lin2(nobs = 300, intercept = 0, arp = 1.01, start = 15, cont = "0")
plot(y, type = "l",ylim=c(0, 200), xlim = c(0, 300))
\dontrun{
p1 <- predict_lin1(y = y[1:150], CritLen = y[170], CritTime = 170, 
NSim = 1000, alpha = 0.05, restrict = FALSE)
for(i in 1:10)
{
  lines(p1$simulations[,i],col=i)
}

abline(h = c(p1$med_CT, p1$confintCT), col = c(2, 2, 2), lty = c(1, 2, 2))
abline(v = c(p1$med_CL, p1$confintCL), col = c(3, 3, 3), lty = c(1, 2, 2))
abline(h = y[170], lty = 3)
abline(v = 170, lty = 3)
lines(y, lwd = 2)
}
}
