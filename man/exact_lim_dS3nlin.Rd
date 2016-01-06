\name{exact_lim_dS3nlin}
\alias{exact_lim_dS3nlin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Approximation of the exact distribution of dS3 for three parameters
}
\description{
This function approximates the exact distribution for dS3 in a three parameter model by 
random draws of the residual signs. 
}
\usage{
exact_lim_dS3nlin(N, Reps, plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
Size of the sample. This is necessary for the number of bins for the exact distribution
}
  \item{Reps}{
Number of simulations used to approximate the distribution
}
  \item{plot}{
Switch to allow a plot of the resulting approximation in a histogram
}
}
\details{
This function can be used to calculate an exact test in case of dS3 for three parameter models}
\value{
The function returns a simulated sample from the approximated exact
distribution
}
\references{
Kustosz, Diss...}
\author{
Kustosz, Christoph
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{dS3_lin2}},  \code{\link{exact_lim_dS3nlin}}
}
\examples{
sim<-exact_lim_dS3nlin(20,100)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
