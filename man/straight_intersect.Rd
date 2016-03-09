\name{straight_intersect}
\alias{straight_intersect}
\title{
Intersection of two straights
}
\description{
This function calculates the intersection of two straights and gives the paramters of this value in
parametric form. Further it indicates, if the straigths intersect within the range of the four defining edges.
}
\usage{
straight_intersect(v1, v2, v3, v4, plots)
}
\arguments{
  \item{v1}{
Startingpoint \code{v1 = c(v11, v22)} of the first staight.
}
  \item{v2}{
Endpoint \code{v1 = c(v11, v22)} of the first staight.
}
  \item{v3}{
Startingpoint \code{v1 = c(v11, v22)} of the second staight.
}
  \item{v4}{
Endpoint \code{v1 = c(v11, v22)} of the second staight.
}
  \item{plots}{
Switch (TRUE or FALSE) to choose, if the results should be plotted. Needs an active plot window.
}
}
\details{This function is mainly used in the change point detection algorithms of this package. 
Details on the usage can be found in Kustosz (2016). 
}
\value{

\item{res}{Resulting decision TRUE/FALSE which indicates, if the straghts from v1 to v2 and from v3 to v4 intercept between v1 and v2 as well as v3 and v4.}
\item{lambda}{Parameter for the first straight, which coincides with the intersection.}
\item{mu }{Parameter for the second straight, which coincides with the intersection.}

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
 \code{\link{lin2_CI}},\code{\link{lin2_CI_cl}}
}
\examples{
par(mfrow = c(1, 2))
a <- straight_intersect(c(1, 1), c(4, 4), c(4, 3), c(-1, 1))
plot(0, 0, xlim = c(-1, 5), ylim =c (0, 5), type = "n")
segments(1, 1, 4, 4)
segments(4, 3, -1, 1)
a

a <- straight_intersect(c(1, 1), c(2, 1), c(1, 2), c(2, 2))
plot(0, 0, xlim = c(-1, 5), ylim =c (0, 5), type = "n")
segments(1, 1, 2, 1)
segments(1, 2, 2, 2)
a
}

