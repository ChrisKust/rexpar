\name{est_nlin1}
\alias{est_nlin1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimation for non-linear AR(1) models by simplicial depth
}
\description{
This function implements an algortihm to estimate a non-linear AR(1) model and
explosion by simplicial depth or one of the proposed simplified notions in Kustosz and Mueller (2014).
}
\usage{
est_nlin1(y,maxit=25,candy=FALSE,perc=0.1,acc=0.1,plots=FALSE,
normtype=1,pv=2,wgt=c(1,0.5),unique=TRUE,notion=dS3_lin2,optim_rude=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
An observed series form an linear AR(1) process with intercept
}
\item{maxit}{
A value for the maximal number of iterations of the optimisation algorithm
}
\item{candy}{
A switch (TRUE/FALSE) deciding, if just edges of simplices defined by the residuals shall be evaluated (FALSE) or if points interior of the simplices should be considered (TRUE)
}
\item{perc}{
A value in (0,1) definig, how large the search regions around an acual candidate should be. A small value defines a very small region, while the value 1 means, that all potential candidates are considered
}
\item{acc}{
A value in (0,1) defining the search regions in the iteration steps. A value of 1 gives large regions, while a value close to 0 defines small regions, measured by the distance to the actual candidate
}
\item{plots}{
A switch (TRUE/FALSE) which allows plots of the iterations steps
}
\item{normtype}{
A parameter which defines the norm used to define distances to the actual paramter in the optimisation algorithm (see \code{\link{Ele_Norm}}) for details)
}
\item{pv}{
A parameter defining the power in the norm to calculate distances (see \code{\link{Ele_Norm}})
}
\item{wgt}{
A vector of weights used for the norm used to calculate distances (see \code{\link{Ele_Norm}})
}
\item{unique}{
A switch (TRUE/FALSE) defining, if a unique maximum shall be the result if multiple points with maximal depth exist. In this case the median point is selected.
}
\item{notion}{
Here the function which is used for depth calculation is defined. The following notionn are allowed: "dS1_lin2", "dS2_lin2", "dS3_lin2", "dS_lin2". Thereby model = nlinAR1 is used.
}
\item{optim_rude}{
This switch allows a rude optimisation by a Nelder-Mead algorithm. This may be more accurate due to the shape of the 
non-linear empirical depth function
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The function returns a list with
\item{estimate }{Parameter maximising the selected depth notion}
\item{value}{Depth at the resulting maximum}
\item{numit}{Number of iterations}
}
\references{
Kustosz, C. (2014). Implementation of simplicial depth for AR processes under non standard conditions. Working Paper, TU Dortmund.
Kustosz, Mueller and Wendler (2014). Simplified Simplicial Depth for Regression and
Autoregressive Growth Processes. Submitted.
}
\author{
Kustosz, Christoph
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{dS1_lin2}},\code{\link{dS2_lin2}},\code{\link{dS3_lin2}},\code{\link{dS_lin2}},\code{\link{Tri_Mid_n1}},\code{\link{nlin1_theta_f}},\code{\link{Ele_Norm}}
}
\examples{
y<-RandomARMod_nlin1(200,1.003,0.2,15,1)
\dontrun{
est_nlin1(y)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
