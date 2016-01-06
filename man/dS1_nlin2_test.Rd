\name{dS1_nlin2_test}
\alias{dS1_nlin2_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Test based on dS1 for explosive nonlinear AR(1) processes with intercept
}
\description{
The function evaluates the asymptotic test based on \eqn{d_S^1} proposed in Kustosz, Mueller and Wendler (2014). It returns the test statistic and the decision. The main model is given by \deqn{Y_n = \theta_1 Y_{n-1}^{\theta_2} + \theta_0 + E_n} with \eqn{med(E_n)=0}.
}
\usage{
dS1_nlin2_test(thetaN,alpha,y,exact=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{thetaN}{
Parameter defining the Null hypothesis H0: theta = thetaN. Thereby thetaN = (thetaN1,thetaN2) as defined by the model. 
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

}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{

\item{TS}{Returns the value of the rescaled and centred test statistic}
\item{phi}{Retuns the test decision, phi = 1 means reject H0, and phi = 0 means do not reject H0}
}
\references{
Kustosz, C. Diss?
}
\author{
Kustosz, Christoph
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{dS1_lin2}}
}
\examples{
y<-RandomARMod_nlin2(10,0,0.2,1.01,15,0)
dS1_nlin2_test(c(0,0.2,1.01),0.05,y)
dS1_nlin2_test(c(0,0.1,1.01),0.05,y)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
