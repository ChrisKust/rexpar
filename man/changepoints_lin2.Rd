\name{changepoints_lin2}
\alias{changepoints_lin2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Changepoint detection for linear AR(1) model with intercept
}
\description{
This function detects change points for paramter changes of a linear AR(1) model with intercept based on simplicial depth
}
\usage{
changepoints_lin2(y,level,bw,sw,plots,method,ncoresCP)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
An observed AR(1) proces with intercept
}
\item{level}{
The level for which the confidence intervals for the phase change detection have to be calculated
}
\item{bw}{
A bandwidth defining the sample size for subsamples which are evaluated in the algorithm
}
\item{sw}{
A stepwidth to reduce the candidate points in which the change points are evaluated
}
\item{plots}{
A switch (TRUE and FALSE) to determine, if plots shall be produced for each tested candidate
}
\item{method}{
This parameter determines the method to detect change points. If method=1 is selected a candidate is marked as change, if the confidence intervals do not overlap. For method=2 the change point is marked, if the estimates are not within the non respective confidence intervals.
}
\item{ncoresCP}{
Number of CPU cores for parallel computing.
}
}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{candidates}{A vector of observation indecees in which the algorithm tested for change points}
\item{changepoints}{A vector indicating, if a change point was detected in the respective candidate}
%% ...
}
\references{
Kustosz, C. (2014). Implementation of simplicial depth for AR processes under non standard conditions. Working Paper, TU Dortmund.
}
\author{
Kustosz, Christoph
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{RandomARMod_lin2}}, \code{\link{lin2_CI}},   \code{\link{convex_hull_plot}},   \code{\link{convex_hull_intersect}},  \code{\link{est_lin2}},  \code{\link{dS_lin2}},  \code{\link{dS_lin2_test}},  \code{\link{dS1_lin2_test}}
}
\examples{
int1<-0.1
int2<-0.2
arp1<-1.001
arp2<-1.004


y1<-RandomARMod_lin2(50,int1,arp1,15,3)
y2<-RandomARMod_lin2(50,int2,arp2,y1[length(y1)],3)
y<-c(y1,y2)

\dontrun{
par(ask=T)

changepoints_lin2(y,0.99,25,10,plots=FALSE,method=1,ncoresCP=4)->CP

plot(y,type="l")
abline(v=CP$candidates[CP$changepoints==TRUE],col=2)
}
}

%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
