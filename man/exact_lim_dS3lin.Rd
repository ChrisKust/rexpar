\name{exact_lim_dS3lin}
\alias{exact_lim_dS3lin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulation of the exact limit distribution for the dS3 test statistic
}
\description{
This function approximates the exact limit distribution of the dS3 statistic for two parameter models under H0 for fixed N. 
}
\usage{
exact_lim_dS3lin(N,Reps,plot)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
Sample size N for which the limit has to be approximated
}
  \item{Reps}{
Number of simulations used in the approximation
}
  \item{plot}{
Swich to allow plots of the limit.
}
}
\details{
The function approximates the exact limit distibution of the simplified depth notion, taking the dependency into account.
}
\value{
The function returns a sample of length Reps consisting of simulated values of the limit distribution. 
}
\references{
Kustosz, C. (2015). Depth Based Estimators and Tests for Autoregressive Processes with Application. Working Paper, TU Dortmund.
}
\author{
Kustosz, Christoph}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
exact_lim_dS3lin(10,10,plot=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
