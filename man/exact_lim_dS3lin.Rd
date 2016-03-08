\name{exact_lim_dS3lin}
\alias{exact_lim_dS3lin}
\title{
Approximation of the exact distribution of dS3 for two parameters
}
\description{
This function approximates the exact distribution for dS3 in a two parameter model by 
random draws of the residual signs. 
}
\usage{
exact_lim_dS3lin(N, Reps, plot = FALSE)
}
\arguments{
  \item{N}{
Size of the sample. This is necessary for the number of bins for the exact distribution.
}
  \item{Reps}{
Number of simulations used to approximate the distribution.
}
  \item{plot}{
Switch to allow a plot of the resulting approximation in a histogram. Select \code{plot = TRUE} for plotting and
\code{plot = FALSE} for no plots.
}
}
\details{
Details can be found in Kustosz (2016).
}
\value{
The function returns a simulated sample from the approximated exact distribution.
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
}
\author{
Kustosz, Christoph
}
\note{
The function is used, when the dS3_lin2 test is applied with exact=TRUE.}


\seealso{
 \code{\link{dS3_lin2}},  \code{\link{exact_lim_dS3nlin}}
}
\examples{
sim <- exact_lim_dS3lin(20, 100)
hist(sim)
}

