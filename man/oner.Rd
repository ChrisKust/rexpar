\name{oner}
\alias{oner}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Main evaluation of dS
}
\description{
This function is a central part of the calculation of dS. It is necessary to calculate the matrix based inner loops
}
\usage{
oner(resid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resid}{A vector of residuals \eqn{(r_k,...,r_{k+m})} for which the inner loop has to be evaluated
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The function returns the sum of all indicators, when the first residual of the input vector is fixed and the inner sums run over all remaining residuals
}
\references{
Kustosz, C. (2014). Implementation of simplicial depth for AR processes under non standard conditions. Working Paper, TU Dortmund.
Leucht, Kustosz and Mueller (2014). Tests based on simplicial depth for AR(1) models with
explosion. Submitted.
}
\author{
Kustosz, Christoph
}
\note{
The funcion is used in the dS calculation routine.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{find1}}, \code{\link{find2}},\code{\link{intfun2}}, \code{\link{oner}}, \code{\link{inv_tri}}
}
\examples{
res<-c(1,2,3,4)
oner(res)

y<-RandomARMod_lin2(100,0.2,1.01,15,0)
res<-resARMod_lin2(c(0.2,1.01),y)
oner(res)

## The function is currently defined as
function(resid)
{
#M1<-matrix(rep(resid[1],(length(resid)-2)^2),ncol=length(resid)-2)
M2<-matrix(rep(resid[2:(length(resid)-1)],each=length(resid)-2),ncol=length(resid)-2,byrow=T)
M2[inv_tri(M2)==T]<-0
M3<-hankel.matrix(length(resid)-2,resid[3:length(resid)])
M3[inv_tri(M3)==T]<-0
MT<-(resid[1]>0)*(M2<0)*(M3>0)+(resid[1]<0)*(M2>0)*(M3<0)
return(sum(MT))
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
