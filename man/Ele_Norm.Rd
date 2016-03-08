\name{Ele_Norm}
\alias{Ele_Norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Distance measures
}
\description{
This function calculates distances for given elements to restrict the parameters for depth estimation to smaller regions with respect to the selected norm. It is an auxiliary function for the proposed depth methods.
}
\usage{
Ele_Norm(Cvec, center, pv, nortype, wgt)
}
\arguments{
  \item{Cvec}{
  A point for which the distance to a center point has to be calculated.
  }
\item{center}{
A center point from which the distance has to be calculated.
}
\item{pv}{
A value for the power within the norm definition, e.g. for \code{ntype = 1}, \code{pv = 2} the L2-Norm is calculated.
}
\item{nortype}{
A value for the specification of the norm, \code{ntype = 1} gives a norm depending on the absolute value and a power pv, while \code{ntype = 2} gives a weighted norm with power pv.
}
\item{wgt}{
A vector with two elements defining the weights for the components of the vectors for which the norm is calculated. This allows asymmetric norms to reflect the data strucure. 
}
}

\value{
The function returns a real number measuring the distance between the vector Cvec and center.
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
The function is an auxilary function to estimate parameters of (non)linear AR processes by depth basesd methods. 
}


\seealso{
 \code{\link{est_lin2}},  \code{\link{est_nlin1}}
}
\examples{
x1 <- seq(-2, 2, 0.05)
x2 <- seq(-2, 2, 0.05)
g <- expand.grid(x1, x2)
d <- apply(g, 1, Ele_Norm, center = c(0, 0), nortype = 2, wgt = c(1, 1), pv = 2)
plot(g)
points(g[, 1][d < 1], g[, 2][d < 1], col = 2)
}