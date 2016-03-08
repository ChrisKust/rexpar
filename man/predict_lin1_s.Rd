\name{predict_lin1_s}
\alias{predict_lin1_s}
\title{Bootstrap prediction for linear AR(1) model without intercept
}
\description{
The function allows a prediction of a linear AR(1) model without intercept based
on simplicial depth and residual bootstrapping. Thereby the jumps and depth shapes are 
adjusted by additional arguments, to allow an extrapolation of future values. The main idea is presended in Kustosz (2016). Thereby a fixed starting value of
the simulated processes can be specified.
}
\usage{
predict_lin1_s(y, CritLen, CritTime, NSim, alpha, restrict = FALSE, start, eps = 1e-9)
}
\arguments{
  \item{y}{
Observed series on which the parameter estimates are calculated. A prediction for future values of this series are produced. 
}
  \item{CritLen}{
A value for a critical lenght for which the time of arrival then can be predicted by the function.
}
  \item{CritTime}{
  A value for a critivcal time (as index of the process), exceeding the size of y, for which the process value is predicted.
}
  \item{NSim}{
  Number of simulations used in the bootstrap simulations of the continued process.
}
  \item{alpha}{
A level which defines  the \eqn{1-\alpha} prediction intervals.
}
  \item{restrict}{
A switch, which allow to activate a restriction for parameter simulations used in the bootstrap procedure. If restrict is activated \code{restrict = TRUE} 1-alpha confidence intervals are used to generate simulated paremeters based on the empirical simplicial depth. If restrict is off \code{restrict = FALSE} the complete candidate set is used for simulations. Thereby the probabilities of the candidates rely on the empirical simplicial depth. 
}
\item{start}{
Here a starting value for the simulated processes can be specified.
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
Kustosz, Christoph
}


\seealso{
 \code{\link{draw_from_depth}}}
