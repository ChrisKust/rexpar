\name{convex_hull_intersect}
\alias{convex_hull_intersect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Intersections of convex hulls
}
\description{
The function calculates how often the staights defining two convex hulls intersect and plots the results if desired.
}
\usage{
convex_hull_intersect(points1,points2,alpha,y1,y2,notion,plots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{points1}{
Points to calculate the first convex hull as Kx2 matrix 
}
  \item{points2}{
Points calculate the second convex hull as Kx2 matrix 
}
\item{alpha}{
Level for which the depth test was computed to get the convex hulls as result
}
\item{y1}{
Data used for the calculation of the first confidence interval or convex hull
}
\item{y2}{
Data used for the calculation of the second confidence interval or convex hull
}
\item{notion}{
Depth notion used to calculate the confidence intevals. Possible notions are "dS", "dS1", "dS2" and "dS3"
}
\item{plots}{
A switch (TRUE or FALSE) to indicate if plots shall be produced
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use p1=cbind(points1[p1,1],points1[p1,2]),p2=cbind(points2[p2,1],points2[p2,2]),sumint=sumi
\item{p1 }{Points which define the first convex hull}
\item{p2 }{Points which define the second convex hull}
\item{sumint}{Number of intersections between the straights which define the bounds of the convex hulls}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Kustosz, C. (2014). Implementation of simplicial depth for AR processes under non standard conditions. Working Paper, TU Dortmund.
}
\author{
Kustosz, Christoph
}
\note{
Note, that the value for sumint is 1, if the convex hulls are completely overlapping, to indicate an intersection in this case. 
This function is an auxiliary function to compare the confidence regions in order to construct a phase change detection based on simplicial depth. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{chull}},\code{\link{straight_intersect}},\code{\link{dS_lin2_test}},\code{\link{dS1_lin2_test}},\code{\link{dS2_lin2_test}},\code{\link{dS3_lin2_test}}
}
\examples{
po1<-rbind(c(1,1),c(2,4),c(1,2),c(2,3))
po2<-rbind(c(1,3),c(2,1.5),c(1.5,2),c(1.2,2))
convex_hull_intersect(po1,po2,plots=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
