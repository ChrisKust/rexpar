\name{ols_ts}
\alias{ols_ts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
OLS test statisitic for explosive AR(1) processes
}
\description{
The function comutes an OLS test statistic as described in Wang and Yu (2013) do derive an OLS based test for explosive AR processes.
}
\usage{
ols_ts(y, thetaT)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Observed AR(1) process with intercept.
}
  \item{thetaT}{
Parameter to calculate the OLS statistic in. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{Ttheta0 }{OLS test statistic for the intercept parameter}
\item{Ttheta1}{OLS test statistic for the AR parameter}
%% ...
}
\references{
Wang and Yu, 2013
}
\author{
Kustosz, Christoph
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
