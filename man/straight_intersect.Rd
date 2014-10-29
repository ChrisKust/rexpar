\name{straight_intersect}
\alias{straight_intersect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Intersection of two straights
}
\description{
This function calculates the intersection of two straights and gives the paramters of this value in
parametric form. Further it indicates, if the straigths intersect within the range of the four defining edges.
}
\usage{
straight_intersect(v1,v2,v3,v4,plots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v1}{
Startingpoint v1=c(v11,v22) of the first staight
}
  \item{v2}{
Endpoint v1=c(v11,v22) of the first staight
}
  \item{v3}{
Startingpoint v1=c(v11,v22) of the second staight
}
  \item{v4}{
Endpoint v1=c(v11,v22) of the second staight
}
  \item{plots}{
Switch (TRUE or FALSE) to choose, if the results should be plotted. Needs an active plot window.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use res=res,mu=mu,lambda=lambda
\item{res}{Resulting decision TRUE/FALSE which indicates, if the straghts from v1 to v2 and from v3 to v4 intercept between v1 and v2 as well as v3 and v4}
\item{lambda}{Parameter for the first straight, which coincides with the intersection}
\item{mu }{Parameter for the second straight, which coincides with the intersection}
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
 \code{\link{lin2_CI}},\code{\link{lin2_CI_cl}}
}
\examples{
straight_intersect(c(1,1),c(4,4),c(4,3),c(-1,1))->a
a

straight_intersect(c(1,1),c(2,1),c(1,2),c(2,2))->a
a
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
