\name{Ele_Norm}
\alias{Ele_Norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Distance measures
}
\description{
This function calculates distances for given elements to restrict the parameters for depth estimation to smaller regions with respect to the selected norm.
}
\usage{
Ele_Norm(Cvec,center,pv,nortype,wgt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Cvec}{
  A point for which the distance to a center point has to be calculated
  }
\item{center}{
A center point from which the distance has to be calculated
}
\item{pv}{
A value for the power within the norm definition, e.g. for ntype = 1, pv = 2 the L2-Norm is calculated
}
\item{nortype}{
A value for the specification of the norm, ntype=1 gives a norm depending on the absolute value and a power pv, while ntype=2 gives a weighted norm with power pv.
}
\item{wgt}{
A vector with two elements defining the weights for the components of the vectors for which the norm is calculated. This allows asymmetric norms to reflect the data strucure. 
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The function returns a real value measuring the distance between the vector Cvec and center
}
\references{
Kustosz, C. (2014). Implementation of simplicial depth for AR processes under non standard conditions. Working Paper, TU Dortmund.
}
\author{
Kustosz, Christoph
}
\note{
The function is an auxilary function to estimate parameters of (non)linear AR processes by depth basesd methods. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{est_lin2}},  \code{\link{est_nlin1}}
}
\examples{
x1<-seq(-2,2,0.05)
x2<-seq(-2,2,0.05)
expand.grid(x1,x2)->g
d<-apply(g,1,Ele_Norm,center=c(0,0),nortype=2,wgt=c(1,1),pv=2)
plot(g)
points(g[,1][d<1],g[,2][d<1],col=2)
%## The function is currently defined as
%function (x) 
%{
%  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
