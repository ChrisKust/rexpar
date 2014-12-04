\name{draw_from_depth}
\alias{draw_from_depth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Random number generation based on empirical simplicial depth
}
\description{
This function transforms a given empirical simplicial depth result to a distribution function and generates random numbers from this distribution by inversion. 
}
\usage{
draw_from_depth(depthI, testvec, lower, upper)
}
\arguments{
  \item{depthI}{
Values of an empirical depth calculation
}
  \item{testvec}{
Parameters of the empirical depth vector defined by depthI.
}
  \item{lower}{
A lower candidate value to restrict the depth on an interval [lower,upper]
}
  \item{upper}{
A upper candidate value to restrict the depth on an interval [lower,upper]
}
}

\value{
The function returns a simulated parameter theta. 
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Diss
}
\author{
Kustosz, Christoph
}

\seealso{
 \code{\link{predict_lin1}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
