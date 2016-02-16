\name{eps_ind}
\alias{eps_ind}
\title{
Calculation of a point inside of a triangle
}
\description{
This is a internal function of the optimisation algoritm using a candidate search and a precise calculation by considering points inside of the simplices defined by intersection of residual straights and an epsilon to shift the candidates into the simplex.
}
\usage{
eps_ind(Mat,iX,eps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mat}{
A Matrix with parameters defining the edges of simplicies defined by intersections of residuals.
}
\item{iX}{
Indices for residuals from which an interior point has to be evaluated.
}
\item{eps}{
Epsilon to shift the candidate value itside a considered simplex.
}
}
\details{
The computational details are in Kustosz (2016).
}
\value{
The function returns a new parameter value which inside of the considered simplex.
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
\code{\link{Tri_Mid}}, \code{\link{Tri_Mid_n1}}
}
