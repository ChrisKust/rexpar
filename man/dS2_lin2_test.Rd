\name{dS2_lin2_test}
\alias{dS2_lin2_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Test based on dS2 for explosive AR(1) processes with intercept
}
\description{
The function evaluates the asymptotic test based on dS2 proposed in Kustosz, Mueller and Wendler (2014). It returns the test statistic and the decision. The main model is given by \deqn{Y_n = \theta_0 + \theta_1 Y_{n-1} + E_n} with \eqn{med(E_n)=0}.
}
\usage{
dS2_lin2_test(thetaN,alpha,y,exact=F,cpow=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{thetaN}{
Parameter defining the Null hypothesis H0: theta = thetaN. Thereby thetaN = (thetaN0, thetaN1) as defined by the model. 
}

  \item{alpha}{
Value in (0,1) defining the level of the test.  
}

  \item{y}{
Observed series y=(y0,...,yN) for which the parameter test has to be executed. 
}
\item{exact}{
This switch allows the usage of an exact distribition of the test statistics using the sample size. 
}
\item{cpow}{
fixed and known power parameter for the y(n) = theta2*y(n)^cpow + theta1 model
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{

\item{TS}{Returns the value of the rescaled and centred test statistic}
\item{phi}{Retuns the test decision, phi = 1 means reject H0, and phi = 0 means do not reject H0}
}
\references{
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
\code{\link{dS2_lin2}}
}
\examples{
y<-RandomARMod_nlin1(1000,0.2,1.01,15,0)
dS2_lin2_test(c(0.2,1.01),0.05,y)
dS2_lin2_test(c(0.1,1.01),0.05,y)

## The function is currently defined as
function(thetaN,alpha,y)
{
  dS2<-dS2_lin2(thetaN,y)
  NdS2<-NdS2<-sqrt(floor(length(y)/2)-1)*(dS2-1/4)/sqrt(3/16)
  deci<-(NdS2<qnorm(alpha))
  list(TS=NdS2,phi=deci)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
