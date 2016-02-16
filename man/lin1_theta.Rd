\name{lin1_theta}
\alias{lin1_theta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Candidate point evaluation for simplicial depth in the linear model without intercept
}
\description{
This function precalculates parameters on which depth changes to restirict the
region on which the depht statistic has to be evaluated on as described in Kustosz and Mueller(2014).
}
\usage{
lin1_theta(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Data from an AR(1) Model without intercept
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
\item{t1}{Candidate values for theta1}
\item{t2}{Candidate values for theta2}
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
Kustosz C., M\"{u}ller Ch. H. and Wendler M. (2016). Simplified Simplicial Depth for Regression and
Autoregressive Growth Processes. Journal of Statistical Planning and Inference. In press.\cr
\cr
Kustosz C. and M\"{u}ller Ch. H. (2014). Analysis of crack growth with robust distribution-
free estimators and tests for nonstationary autoregressive processes. Statistical
Papers 55, 125-140.\cr
}
\author{
Kustosz, Christoph
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dS_lin2}},\code{\link{dS1_lin2}},\code{\link{dS2_lin2}},\code{\link{dS3_lin2}}, \code{\link{lin2_CI}},\code{\link{est_lin2}},\code{\link{est_nlin1}}
}
\examples{
y<-RandomARMod_lin2(100,1.001,0,15,0)
thetas<-lin1_theta(y)
}
%## The function is currently defined as
%function (x) 
%{
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
