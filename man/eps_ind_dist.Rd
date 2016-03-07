\name{eps_ind_dist}
\alias{eps_ind_dist}
\title{
Calculation of a point inside of a triangle
}
\description{
This is a internal function of the optimisation algoritm using a candidate search and a precise calculation by considering points inside of the simplices defined by intersection of roots of residuals and an epsilon to shift the candidates into the simplex. Thereby the considered simplices are constructed by the colosest two neighbours of each candidate.
}
\usage{
eps_ind_dist(v1, Mat, eps)
}
\arguments{
\item{v1}{
A central candidate which gives an edge of the considered simplex. The remaining edges then are selected as the candidates with minimal distance to v1.
}
  \item{Mat}{
A Matrix of parameters defining the edges of simplicies defined by intersections of residuals.
}
\item{eps}{
Epsilon to shift the candidate value itside a considered simplex.
}
}
\details{
Details can be found in Kustosz (2016).
}
\value{
The function returns a new parameter value which lies in the selected simplex.
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
