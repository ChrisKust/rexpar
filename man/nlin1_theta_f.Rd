\name{nlin1_theta_f}
\alias{nlin1_theta_f}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Candidate point evaluation for simplicial depth in the non-linear model
}
\description{
This function precalculates parameters on which depth changes to restirict the
region on which the depht statistic has to be evaluated on
}
\usage{
nlin1_theta_f(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Data from an non linear AR(1) Model without intercept
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
Kustosz, C. (2014). Implementation of simplicial depth for AR processes under non standard conditions. Working Paper, TU Dortmund.
}
\author{
Kustosz, Christoph
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dS_lin2}},\code{\link{dS1_lin2}},\code{\link{dS2_lin2}},\code{\link{dS3_lin2}},\code{\link{est_nlin1}}
}
\examples{
y<-RandomARMod_nlin1(100,1.001,0.2,15,0)
thetas<-nlin1_theta_f(y)
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
