\name{inv_tri}
\alias{inv_tri}
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
Matrix is a matrix from witch the elements below the counterdiagonals have to be identified.
}
}
\details{
Details on the usage in the calculation be found in Kustosz (2016).
}
\value{
The output is a matrix with boolean values of size of Matrix, which determine, if the elements are 
below the counterdiagonal or not.
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

