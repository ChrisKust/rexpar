\name{Tri_Mid_n1}
\alias{Tri_Mid_n1}
\title{
Calculation of candidates, restricted for faster estimation by simplicial depth
}
\description{
This function calculates candidates for estimates by simplicial depth based on intersections from residual straights. Thereby for large samples the candidates can be restricted by quantiles to reduce computational costs. Further points at the centroid of the candidate simplexes are calculated and the non-linear model without intercept is considered.
}
\usage{
Tri_Mid_n1(y, perc, candy)
}
\arguments{
  \item{y}{
A series form from a non-linear AR(1) process for which the candidates have to be calculated.
}
\item{perc}{
A value between 0 and 1 defining what range of candidates around of their median has to be considered for the optimisation. A value close to 0 gives a low precision, while a value of 1 evaluates all candidates.
}
\item{candy}{
This is a switch, which decides, if the candidates shall be restricted to the edges of the respective simplices \code{candy = FALSE} or if in addition interior points shall be evaluated \code{candy = TRUE}.
}
}
\details{
The main idea for the candidate construction is presented in Kustosz (2016).
}
\value{
The returned value is a matrix of candidate parameters whereby the first column is \eqn{\theta_1} and the second \eqn{\theta_2}.
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
 \code{\link{est_nlin1}},  \code{\link{dS_lin2_test}}
}
