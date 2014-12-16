\name{follow_ups}
\alias{follow_ups}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Postprocessing for changepoint detection
}
\description{
The function collects subsequent marked candidate points in the changepoints_lin2 function to calculate final estimates of the change points. 
}
\usage{
follow_ups(dat, mincper, steps)
}
\arguments{
  \item{dat}{
Marked candidate points which could be changepoints based on the method selected in the changepoints_lin2 function.
}
  \item{mincper}{
A value for a minimal percentage of the size of the largest marked cluster to limit the size of clusters used for the final changepoints
}
  \item{steps}{
Steps in which the candidates are evaluated. 
}
}
\details{
This function is just used in the changepoint detection algorihms internally.
}
\value{
\item{clusters}{Sizes of the jump clusters}
\item{jumps}{Detected changpoint locations based on the median values in each relevant cluster}
%% ...
}
\references{
Diss?
}
\author{
Kustosz, Christoph
}


\seealso{
\code{\link{changepoints_lin2}}, \code{\link{changepoints_lin2_cl}}
}
\examples{
x<-c(1,2,3,4,7,8,11,35,40,50,60)
follow_ups(x,0.75,1)



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
