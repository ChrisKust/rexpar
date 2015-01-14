\name{ols_expl}
\alias{ols_expl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
OLS Estimation for explosive AR(1) processes with intercept
}
\description{
OLS Estimation for explosive AR(1) processes with intercept
}
\usage{
ols_expl(y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Observed process
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{theta0}{Estimated intercept}
\item{theta1}{Estimated ar parameter}
\item{sigma2}{Estimated variance based on the estimated residuals}
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
