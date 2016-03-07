\name{convex_hull_plot}
\alias{convex_hull_plot}
\title{
Convex hull plot for two dimensional data
}
\description{
The function generates a plot of the convex hull from two dimensional scatter data.
}
\usage{
convex_hull_plot(x, y, col=1, lwd = 1)
}
\arguments{
  \item{x}{
The x coordinates of the scatter data.
}
  \item{y}{
  The y coordinates of the scatter data.
  }
  \item{col}{
  The color for the lines defining the convex hull in the plot.
  }
    \item{lwd}{
  The line width of the convex hull.
  }
}
\value{
The function does not return any values.
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


\seealso{
\code{\link{lin2_CI}},\code{\link{convex_hull_intersect}}
}
\examples{
x <- rnorm(10)
y <- rnorm(10)
plot(x, y)
convex_hull_plot(x, y, col=2)
}

