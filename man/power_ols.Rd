\name{power_ols}
\alias{power_ols}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Power of an OLS test for explosive AR processes
}
\description{
This function simulates the power of an OLS test for explosive AR processes discussed in Wang and Yu (2013)
}
\usage{
power_ols(thetas, N, R, sv, cont, theta0, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{thetas}{
Parameter with which processes are simulated to evaluate the power in.
}
  \item{N}{
Size of the simulated processes
}
  \item{R}{
  Number of repetitions of the simulated processes
}
  \item{sv}{
Starting value for the simulated processes
}
  \item{cont}{
Contamination distribution as defined by RandomARMod_lin2
}
  \item{theta0}{
Parameter to define the Null-Hypothesis of the considered two-sided test
}
  \item{alpha}{
Level of the test
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The output is a simulated value of the power function for the test in H0: theta = theta0 evaluated in thetas.
}
\references{
Wang and Yu, 2013}
\author{
Kustosz, Christoph}
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
