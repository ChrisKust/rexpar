\name{eps_ind_dist}
\alias{eps_ind_dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of a point inside of a triangle
}
\description{
This is a internal function of the optimisation algoritm using a candidate search and a precise calculation by considering points inside of the simplices defined by intersection of residual straights and an epsilon to shift the candidates into the simplex. Thereby the considered simplices are constructed by the colosest two neighbours of each candidate.
}
\usage{
eps_ind_dist(v1,Mat,eps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{v1}{
A central candidate which gives an edge of the considered simplex. The remaining edges then are selected as the candidates with minimal distance to v1.
}
  \item{Mat}{
A Matrix with parameters defining the edges of simplicies defined by intersections of residuals
}
\item{eps}{
Epsilon to shift the candidate value itside a considered simplex
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The function returns a new parameter value which lies in the selected simplex
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
\code{\link{Tri_Mid}}, \code{\link{Tri_Mid_n1}}
}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.

%## The function is currently defined as
%function (x) 
%{
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
