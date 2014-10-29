\name{sigmaMat}
\alias{sigmaMat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of covariance matrix for the limit process on a grid
}
\description{
This function calculates the covariance matrix of the limit process in Leucht, Mueller and Kustosz (2014) for a given grid.
}
\usage{
sigmaMat(t,nclust=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{
Grid on [-2,2] on which the process later will be evaluated
}
  \item{nclust}{
Number of processors on which the calculation can be splitted. Default value is given by 1.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The function retruns the variance covariance matrix on the predefined grid, which then can be used to simulate the process. 
}
\references{
Kustosz, C. (2014). Implementation of simplicial depth for AR processes under non standard conditions. Working Paper, TU Dortmund.
Leucht, Kustosz and Mueller (2014). Tests based on simplicial depth for AR(1) models with
explosion. Submitted.
}
\author{
Kustosz, Christoph
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{simulateGP}}, \code{\link{muVec}}
}
\examples{
g<-seq(-2,2,0.1)
sigma<-sigmaMat(g)

Y<-simulateGP(g,sigma)
plot(Y$Y1)
lines(Y$Y2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
