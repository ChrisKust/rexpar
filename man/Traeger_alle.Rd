\name{Traeger_alle}
\alias{Traeger_alle}
\docType{data}
\title{
Observations for a crack growth process
}
\description{
The dataset contains results experiment TR05 from Maurer and Heeke (2010). 
}
\usage{data(Traeger_alle)}
\format{
A data frame indluding Time, Loadcycles, Mean values and a recenterd process from 5 experiments
  \describe{
    \item{\code{TR01}}{Experiment TR01}
     \item{\code{TR02}}{Experiment TR02}
      \item{\code{TR03}}{Experiment TR03}
       \item{\code{TR04}}{Experiment TR04}
        \item{\code{TR05}}{Experiment TR05}
  }
}

\source{
Data generated in Maurer and Heeke (2010)
}
\references{
MH
}
\examples{
data(Traeger_alle)
plot(TR01$Mittel)
}
\keyword{datasets}
