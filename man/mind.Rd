\name{mind}
\alias{mind}
\title{
Calculation of a point inside of a triangle
}
\description{
This is a internal function of the optimisation algoritm using a candidate search and a precise calculation by considering points inside of the simplices defined by intersection of straight defined by the roots of the residuals.
}
\usage{
mind(Mat, iX)
}
\arguments{
  \item{Mat}{
A Matrix with parameters defining the edges of simplicies defined by intersections of root equations from the residuals.
}
\item{iX}{
Indices for residuals from which an interior point has to be evaluated.
}
}
\details{
The details on the idea and implementation can be found in Kustosz (2016).}

\value{
The function returns a new parameter value which lies in the selected simplex
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
