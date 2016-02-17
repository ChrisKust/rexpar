\name{lin2_CI}
\alias{lin2_CI}
\title{
Confidence regions for linar AR(1) processes with intercept based on simplicial depth
}
\description{
This function generates confidence regions for linear AR(1) processes with intercept and explosion based on simplified and full simplicial depth for AR processes.
}
\usage{
lin2_CI(y,level,plots=FALSE,notion="dS1",ncoresC=1,mid=FALSE)
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
\item{ncoresC}{
If the notion "dS" is selected a multicore computation is possible. Here the number of cores is defined as described in the function \code{\link{dS_lin2}}.
}
\item{mid}{
This switch allow to use central points from data generated candidates based on the paramters defining the
roots of the resuiduals.
}
}
\details{
The theoretical details can be found in Kustosz, M\"{u}eller and Wendler (2016) and Kustosz, Leucht and M\"{u}ller (2016). The details on the implementation are in Kustosz (2016).}
\value{

\item{par}{A matrix with the evaluated points to calculate the confidence region by evaluation of the depth tests.}
\item{inCI}{A binary vector indicating, if a parameter in par is in the confidence interval or not.}

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
\examples{
\dontrun{y<-RandomARMod_lin2(100,0.2,1.001,15,0)
lin2_CI(y,0.05,notion="dS1")

y1<-RandomARMod_lin2(50,0.2,1.002,15,0)
lin2_CI(y1,0.05,notion="dS1")->A1
lin2_CI(y1,0.05,notion="dS2")->A2
lin2_CI(y1,0.05,notion="dS3")->A3
lin2_CI(y1,0.05,notion="dS",ncoresC=2)->AS
lin2_CI(y1,0.05,notion="dS_pre",ncoresC=2)->ASp


par(mfrow=c(2,2))
plot(AS$par[AS$inCI==0,],col=3,pch=19,main="dS vs dSp")
points(ASp$par[ASp$inCI==0,],col=2)
plot(A1$par[A1$inCI==0,],col=2,pch=19,main="dS1 vs dS")
points(AS$par[AS$inCI==0,],col=3)
plot(A2$par[A2$inCI==0,],col=2,pch=19,main="dS2 vs dS")
points(AS$par[AS$inCI==0,],col=3)
plot(A3$par[A3$inCI==0,],col=2,pch=19,main="dS3 vs dS")
points(AS$par[AS$inCI==0,],col=3)
}

}

