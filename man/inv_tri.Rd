\name{inv_tri}
\alias{inv_tri}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inverted triagonal matrix
}
\description{
This function is necessary to calculate simplicial depth for explosive autoregressive processes
in a more efficient way, than in simple loops. It generates a matrix, which is zero below the
counterdiagonal. 
}
\usage{
inv_tri(Matrix)
}
\arguments{
  \item{Matrix}{
Matrix is a matrix from witch the elements below the counterdiagonals have to be identified
}
}
%\details{
%}
\value{
the output is a matrix with boolean values of size of Matrix, which determine, if the elements are 
below the counterdiagonal or not.
}
\references{
Kustosz, C. (2014). Implementation of simplicial depth for AR processes under non standard conditions. Working Paper, TU Dortmund.
}
\author{
Kustosz, Christoph
}
\note{
The function is neccecary for internal usage in the calculation of dS.
}

\seealso{
\code{\link{dS_lin2}}

}
\examples{
M<-matrix(seq(1,9,1),ncol=3,byrow=TRUE)
inv_tri(M)
M[inv_tri(M)==FALSE]
M[inv_tri(M)==TRUE]<-0
M

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
