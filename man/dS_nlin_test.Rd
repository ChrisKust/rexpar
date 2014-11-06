\name{dS_nlin_test}
\alias{dS_nlin_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Test based on dS for explosive nonlinear AR(1) processes with intercept
}
\description{
The function evaluates the asymptotic test based on dS proposed in Leucht, Mueller and Kustosz (2014). It returns the test statistic and the decision. The main model is given by \deqn{Y_n = \theta_1 Y_{n-1}^{\theta_2} + E_n} with \eqn{med(E_n)=0}.
}
\usage{
dS_nlin_test(thetaN,alpha,y,ncores)
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
  \item{ncores}{
Defines the number of cores to use for parallel computation of the simplicial depth statistic. If no value is definied the standard value of 1 is passed and no parallel methods are applied.
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
Kustosz and Mueller (2014). Tests based on simplicial depth for AR(1) models with
explosion. Submitted.
}
\author{
Kustosz, Christoph
}
\note{
Note, that the evaluation depends on the simulated results of the limit process, which are included in the rexpar package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dS_lin2}},
}
\examples{
data("simLimit")
y<-RandomARMod_nlin1(10,0.2,1.01,15,0)
dS_nlin_test(c(0.2,1.01),0.05,y)
dS_nlin_test(c(0.1,1.01),0.05,y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
