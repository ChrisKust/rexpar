\name{Tri_Eps}
\alias{Tri_Eps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of candidates, restricted for faster estimation by simplicial depth
}
\description{
This function calculates candidates for estimates by simplicial depth based on intersections from residual straights and epsilon variation. Thereby for large samples the candidates can be restricted by quantiles to reduce computational costs.
}
\usage{
Tri_Eps(y,perc,eps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
A series form from a linear AR(1) process with intercept for which the candidates have to be calculated
}
\item{perc}{
A value between 0 and 1 defining what range of candidates around of their median has to be considered for the optimisation. A value close to 0 gives a low precision, while a value of 1 evaluates all candidates
}
\item{eps}{
Value for epsilon to vary the corner of a considered data generated simplex
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The returned value is a matrix of candidate parameters whereby the first column is theta_1 and the second theta_0
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
 \code{\link{est_nlin1}},  \code{\link{dS_lin2_test}}
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
