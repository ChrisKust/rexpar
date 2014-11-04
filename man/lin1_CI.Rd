\name{lin1_CI}
\alias{lin1_CI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Confidence regions for linar AR(1) processes with intercept based on simplicial depth
}
\description{
This function generates confidence intervals for linear AR(1) processes without intercept and explosion based on simplicial depth for AR processes.
}
\usage{
lin1_CI(y,level,plots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
A observed series from an linear AR(1) process with intercept
}
\item{level}{
A value in (0,1) defining the level of the confidence regions to evaluate
}
\item{plots}{
A swich to turn on and off plots of the resulting region. Use (TRUE) to turn on plots and (FALSE) (default) to suppress plotting
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{par }{A vector evaluated points to calculate the confidence region by evaluation of the depth tests}
\item{inCI }{A binary vector indicating, if a parameter in par is in the confidence interval or not}
%% ...
}
\references{
Kustosz and Mueller (2014). ...Stat. Pap. 
}
\author{
Kustosz, Christoph
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dS1_lin2_test}},\code{\link{dS2_lin2_test}},\code{\link{dS3_lin2_test}},\code{\link{dS_lin2_test}},\code{\link{lin2_theta_f}},\code{\link{convex_hull_plot}}
}
\examples{
y<-RandomARMod_lin2(100,0,1.001,15,0)
lin1_CI(y,0.05)
}


%## The function is currently defined as
%function (x) 
%{
%  }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
