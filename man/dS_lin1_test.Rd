\name{dS_lin1_test}
\alias{dS_lin1_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Test based on dS for explosive AR(1) processes without intercept
}
\description{
The function evaluates the asymptotic test based on dS proposed in Leucht, Mueller and Kustosz (2014). It returns the test statistic and the decision. The main model is given by \deqn{Y_n = \theta_1 Y_{n-1} + E_n} with \eqn{med(E_n)=0}.
}
\usage{
dS_lin2_test(thetaN,alpha,y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{thetaN}{
Parameter defining the Null hypothesis H0: theta = thetaN.
}

  \item{alpha}{
Value in (0,1) defining the level of the test.  
}

  \item{y}{
Observed series y=(y0,...,yN) for which the parameter test has to be executed. 
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
Kustosz and Mueller (2014). ???
}
\author{
Kustosz, Christoph
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dS_lin2}},
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
