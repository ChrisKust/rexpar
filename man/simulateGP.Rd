\name{simulateGP}
\alias{simulateGP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulation of gaussian limit process
}
\description{
This function simulates the gaussian limit process defined in Leucht, Mueller and Kustosz (2014). Thereby a grid can be definied and the covariance matrix has to be precalculated to enable a faster generation of repetitions. 
}
\usage{
simulateGP(g,sigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
One dimensional grid between on [-2,2] on which the process is simulated.
}
  \item{sigma}{
Precalculated covariance matrix based on the predefined grid. This can be done by \code{\link{sigmaMat}}
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The function retruns the bivariate process as list
 \item{Y1}{Variable component of the process}
 \item{Y2}{Second compontent of the process, which is time independent}
%% ...
}
\references{
Leucht, Kustosz and Mueller (2014). Tests based on simplicial depth for AR(1) models with
explosion. Submitted.
}
\author{
Kustosz, Christoph
}
%\note{
%The function uses the MASS package.
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sigmaMat}}, \code{\link{muVec}}, \code{\link{find1}},\code{\link{find2}},\code{\link{intfun2}}
}
\examples{
g<-seq(-2,2,0.1)
sigma<-sigmaMat(g)

Y<-simulateGP(g,sigma)
plot(g,Y$Y1)
lines(g,Y$Y2)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
