\name{LimitApprox}
\alias{LimitApprox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Approximation of the integrated limit based on a simulated bivaraite gaussian process
}
\description{
This function approximates the integrated limit based on the bivariate gaussian limit process discussed in Leucht, Mueller and Kustosz (2014).
}
\usage{
LimitApprox(g,Y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
Grid, on which the bivariate proces has been evaluated.
}
  \item{Y}{
A realisation of the bivariate Gaussian limit process
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A approximation of the integrated process as defined in Leucht, Mueller and Kustosz (2014)
}
\references{
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
g<-seq(-2,2,0.1)
sigma<-sigmaMat(g)

Y<-simulateGP(g,sigma)

R<-LimitApprox(g,Y)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
