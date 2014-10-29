\name{qvals}
\alias{qvals}
\title{
Quantile Values
}
\description{
Definition of simulated quantiles.
}
\usage{
qvals
}
%\arguments{
%}
\details{
The function just defines a vector including quantiles from the simulated
limit distribution of dS. The quantiles are calculated at a accuracy of 3 digits. 
}
\value{
The resulting value is a vector including quantiles . The function is used by the dS_lin2_test() function indirectly.
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
\code{\link{dS_lin2_test}}
}
\examples{
qvals
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
