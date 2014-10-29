\name{muVec}
\alias{muVec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean vector function for limit process
}
\description{
This function evaluates the mean vector function for the limit process derived in Leucht, Mueller and Kustosz (2014). It is just useful in the internal calculations of the Limit simulation.
}
\usage{
muVec(t,y)
}

\arguments{
  \item{t}{
Grid vector to define set, where the process has to be evaluated later on
}

  \item{y}{
Value of second marginal process, which defines the full mean function after a simulation
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns a vector with the dimensions of the grid, which defines the conditional mean function, when the second marginal process is already known.
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

\seealso{
\code{\link{simulateGP}}, \code{\link{sigmaMat}}
}
\examples{
g<-seq(-2,2,0.5)
y<-0.25
muVec(g,y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
