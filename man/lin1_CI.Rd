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
lin1_CI(y,level,plots=FALSE,notion="dS",eps=NULL)
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
\item{notion}{
Here the applied test statistic for the calculation of confidence intervals is specified. Available are\cr
"dS" = full simplicial depth\cr
"dS1" = reduced notion without overlapping of residuals\cr
"dS2" = reduced notion based on large distance residuals\cr
"dS3" = reduced notion with overlapping residuals\cr
}
\item{eps}{
This allows to define a shifting of the candidate paramters by a constant eps to avoid evaluations on candidates defined by roots of the residuals. 
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
\code{\link{dS1_lin1_test}},\code{\link{dS2_lin1_test}},\code{\link{dS3_lin1_test}},\code{\link{dS_lin1_test}},\code{\link{lin1_theta}}}

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
