\name{follow_ups}
\alias{follow_ups}
\title{
Postprocessing for changepoint detection
}
\description{
The function collects subsequent marked candidate points in the changepoints_lin2 function to calculate final estimates of the change points. 
}
\usage{
follow_ups(dat, mincper, steps, mincp)
}
\arguments{
  \item{dat}{
Marked candidate points which could be changepoints based on the method selected in the changepoints_lin2 function.
}
  \item{mincper}{
A value for a minimal percentage of the size of the largest marked cluster to limit the size of clusters used for the final changepoints.
}
  \item{steps}{
Steps in which the candidates are evaluated. 
}
\item{mincp}{
This value defines which cluster, sorted by length, is relevant for the mincper-treshold. I.e. Just clusters with a lenght of at least 'mincper' of the 'mincp' longest cluster are considered as final estimates. 
}
}
\details{
This function is just used in the changepoint detection algorihms internally.
}
\value{
\item{clusters}{Sizes of the jump clusters.}
\item{jumps}{Detected changpoint locations based on the median values in each relevant cluster.}
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
}
\author{
Kustosz, Christoph
}


\seealso{
\code{\link{changepoints_lin2}}, \code{\link{changepoints_lin2_cl}}
}
\examples{
x <- c(1, 2, 3, 4, 7, 8, 11, 35, 40, 50, 60)
follow_ups(x, 0.75, 1)
}
