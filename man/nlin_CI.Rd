\name{nlin_CI}
\alias{nlin_CI}
\title{
Confidence regions for non-linar AR(1) processes with intercept based on simplicial depth
}
\description{
This function generates confidence regions for non-linear AR(1) processes without intercept and explosion based on simplicial depth for AR processes.
}
\usage{
nlin_CI(y, level, plots, notion, ncoresC, addPar, spar)
}
\arguments{
  \item{y}{
A observed series from an non-linear AR(1) process without intercept.
}
\item{level}{
A value in (0,1) defining the level of the confidence regions to evaluate.
}
\item{plots}{
A swich to turn on and off plots of the resulting region. Use \code{plots = TRUE} to turn on plots and \code{plots = FALSE} (default) to suppress plotting.
}
\item{notion}{
A parameter to select the desired depth notion to evaluate. The possible choices are "dS1", "dS2", "dS3" and "dS" defining three simplified notions as discussed in Kustosz, Mueller and Wendler (2016) and the full depth discussed in Kustosz, Leucht and Mueller (2016). The default notion is "dS1".
}
\item{ncoresC}{
If the notion "dS" is selected a multicore computation is possible. Here the number of cores is defined as described in the function \code{\link{dS_lin2}}.
}
\item{addPar}{
A swich to decide if the resulting confidence region shall be added to a existing plot (TRUE) or is generated in a new plot window (FALSE).
}
\item{spar}{
{Smoothing parameter for alpha hull construction in plots.}
}
}
\details{
The theoretical details can be found in Kustosz, Mueeller and Wendler (2016) and Kustosz, Leucht and Mueller (2016). The details on the implementation are in Kustosz (2016).}
\value{

\item{par }{A vector evaluated points to calculate the confidence region by evaluation of the depth tests.}
\item{inCI }{A binary vector indicating, if a parameter in par is in the confidence interval or not. Thereby the test decision is reported. Hence, \code{inCI = 0} donotes a parameter being inside of the confidence set and \code{inCI = 1} denotes a parameter which is not.}

}
\note{The plot functionality depends on the 'alphahull' package which is under ACM license. If this package is not avaliable, the plots are suppressed.}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
Kustosz C., Leucht A. and Mueller Ch. H. (2016). Tests based on simplicial depth for AR(1) models with
explosion. Journal of Time Series Analysis. In press.\cr
\cr
Kustosz C., Mueller Ch. H. and Wendler M. (2016). Simplified Simplicial Depth for Regression and
Autoregressive Growth Processes. Journal of Statistical Planning and Inference. In press.\cr
\cr
}
\author{
Kustosz, Christoph
}

\seealso{
\code{\link{dS1_nlin_test}},\code{\link{dS2_nlin_test}},\code{\link{dS3_nlin_test}},\code{\link{dS_nlin_test}},\code{\link{nlin1_theta_f}},\code{\link{convex_hull_plot}}
}
\examples{
y <- RandomARMod_lin2(10, 0, 1.001, 15, 0)
nlin_CI(y, 0.05)
}


