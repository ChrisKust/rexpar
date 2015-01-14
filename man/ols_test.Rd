\name{ols_test}
\alias{ols_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
OLS test for the two sided alternative for parameters of an explosive AR(1) process with intercept
}
\description{
The function calculates the test decision on for a given two sided hypothesis H_0: theta = theta0 based on an observed process y and level alpha for linear AR(1) processes with intercept and explosion.
}
\usage{
ols_test(theta0, y, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta0}{
Parameter to define the Null-Hypothesis
}
  \item{y}{
Observed process
}
  \item{alpha}{
Level of the test
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns a boolean denoting the test decision by TRUE for reject and FALSE for not-reject.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
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
