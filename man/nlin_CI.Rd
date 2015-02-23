\name{nlin_CI}
\alias{nlin_CI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Confidence regions for nonlinar AR(1) processes with intercept based on simplicial depth
}
\description{
This function generates confidence intervals for nonlinear AR(1) processes without intercept and explosion based on simplicial depth for AR processes.
}
\usage{
nlin_CI(y,level,plots,notion,ncoresC,addPar)
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
A parameter to select the desired depth notion to evaluate. The possible choices are "dS1", "dS2", "dS3" and "dS" defining three simplified notions as discussed in Kustosz, Mueller and Wendler (2014) and the full depth discussed in Leucht, Mueller and Kustosz (2014). The standard notion is "dS1".
}
\item{ncoresC}{
If the notion "dS" is selected a multicore computation is possible. Here the number of cores is defined as described in the function \code{\link{dS_lin2}}
}
\item{addPar}{
A swich to decide if the resulting confidence region shall be added to a existing plot (TRUE) or is generated in a new plot window (FALSE)
}
\item{spar}{
{Smoothing parameter for alpha hull construction in plots}
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
Leucht, Kustosz and Mueller (2014). Tests based on simplicial depth for AR(1) models with
explosion. Submitted.
Kustosz, C. (2014). Implementation of simplicial depth for AR processes under non standard conditions. Working Paper, TU Dortmund.
Kustosz, Mueller and Wendler (2014). Simplified Simplicial Depth for Regression and
Autoregressive Growth Processes. Submitted.
}
\author{
Kustosz, Christoph
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dS1_nlin_test}},\code{\link{dS2_nlin_test}},\code{\link{dS3_nlin_test}},\code{\link{dS_nlin_test}},\code{\link{nlin1_theta_f}},\code{\link{convex_hull_plot}}
}
\examples{
y<-RandomARMod_lin2(10,0,1.001,15,0)
nlin_CI(y,0.05)
}


%## The function is currently defined as
%function (x) 
%{
%  }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
