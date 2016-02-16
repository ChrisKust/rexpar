\name{lin2_CI_cl}
\alias{lin2_CI_cl}
\title{
Confidence regions for linar AR(1) processes with intercept based on simplicial depth 
}
\description{
This function generates confidence regions for linear AR(1) processes with intercept and explosion based on simplified and full simplicial depth for AR processes. This is a version which uses a predefined cluster. This allows a calculation of the confidence sets on HPC's.
}
\usage{
lin2_CI_cl(y,level,plots,notion,cluster)
}
\arguments{
  \item{y}{
A observed series from an linear AR(1) process with intercept.
}
\item{level}{
A value in (0,1) defining the level of the confidence regions to evaluate.
}
\item{plots}{
A swich to turn on and off plots of the resulting region. Use (TRUE) to turn on plots and (FALSE) (default) to suppress plotting.
}
\item{notion}{
A parameter to select the desired depth notion to evaluate. The possible choices are "dS1", "dS2", "dS3" and "dS" defining three simplified notions as discussed in Kustosz, M\"{u}eller and Wendler (2016) and the full depth discussed in Kustosz, Leucht and M\"{u}ller (2016). The standard notion is "dS1".
}
\item{cluster}{
Initialised cluster for parallel computation.
}
}
\details{
The theoretical details can be found in Kustosz, M\"{u}eller and Wendler (2016) and Kustosz, Leucht and M\"{u}ller (2016). The details on the implementation are in Kustosz (2016).}
\value{

\item{par }{A matrix with the evaluated points to calculate the confidence region by evaluation of the depth tests.}
\item{inCI }{A binary vector indicating, if a parameter in par is in the confidence interval or not.}
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
Kustosz C., Leucht A. and M\"{u}ller Ch. H. (2016). Tests based on simplicial depth for AR(1) models with
explosion. Journal of Time Series Analysis. In press.\cr
\cr
Kustosz C., M\"{u}ller Ch. H. and Wendler M. (2016). Simplified Simplicial Depth for Regression and
Autoregressive Growth Processes. Journal of Statistical Planning and Inference. In press.\cr
\cr
}
\author{
Kustosz, Christoph
}

\seealso{
\code{\link{dS1_lin2_test}},\code{\link{dS2_lin2_test}},\code{\link{dS3_lin2_test}},\code{\link{dS_lin2_test}},\code{\link{lin2_theta_f}},\code{\link{convex_hull_plot}}
}
