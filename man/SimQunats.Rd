\name{SimQuants}
\alias{SimQuants}
\title{
Simulated Quantiles
}
\description{
Definition of quantiles on a predefined grid. 
}
\usage{
SimQuants
}
%\arguments{
%
%}
\details{
The function just defines a 2 column matrix including probabilities and quantiles from the simulated
limit distribution of dS. The quantiles are calculated at an accuracy of 3 digits. 
}
\value{
The resulting value is a matrix including probabilities in the first column and respective quantiles in the second. The function is used by the \code{\link{dS_lin2_test}}.
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
SimQuants
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
