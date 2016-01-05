\name{convex_hull_plot}
\alias{convex_hull_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convex hull plot for two dimensional data
}
\description{
The function generates a plot of the convex hull from two dimensional scatter data
}
\usage{
convex_hull_plot(x,y,col=1,lwd=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The x-axis coordinates of the scatter data
}
  \item{y}{
  The y-axis coordinates of the scatter data
  }
  \item{col}{
  The color for the lines defining the convex hull in the plot
  }
    \item{lwd}{
  The line width of the convex hull
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The function does not return any values
}
\references{
http://stats.stackexchange.com/questions/11919/convex-hull-in-r
}
\author{
Kustosz, Christoph
}
\note{
This in an auxiliary function to generate plots of the confidence regions based on simplicial depth for two paramteter models. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{lin2_CI}},\code{\link{convex_hull_intersect}}
}
\examples{
x<-rnorm(10)
y<-rnorm(10)
plot(x,y)
convex_hull_plot(x,y,col=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
