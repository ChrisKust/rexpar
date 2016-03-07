\name{est_lin1}
\alias{est_lin1}
\title{
  Estimation for linear AR(1) models without intercept by simplicial depth
}
\description{
  This function implements an algortihm to estimate a linear AR(1) model without intercept and
  explosion by simplicial depth or one of the proposed simplified notions in Kustosz, Mueeller and Wendler (2016).
}
\usage{
  est_lin1(y, maxit, candy, acc, plots, eps, unique, notion)
}
\arguments{
  \item{y}{
    An observed series form an linear AR(1) process without intercept.
  }
  \item{maxit}{
    A value for the maximal number of iterations of the optimisation algorithm.
  }
  \item{candy}{
    A switch deciding, if just edges of simplices defined by the residuals shall be evaluated \code{candy = FALSE} or if points interior of the simplices should be considered \code{candy = TRUE}.
  }
  \item{acc}{
    A value in (0,1) definig the size of the search regions in each iteration step.
  }
  \item{plots}{
    A switch (TRUE/FALSE) which enables plots of the iterations steps.
  }
  \item{eps}{
    An epsilon defining the range around candidate parameters to inflate the candidate set. See \code{\link{lin1_theta}}
  }
\item{unique}{
  A switch (TRUE/FALSE) defining, if a unique maximum shall be the result if multiple points with maximal depth exist. In this case the median point is selected.
}
\item{notion}{
  Here the function which is used for depth calculation is defined. The following notions are allowed: "dS1", "dS2", "dS3", "dS".
}
}
\details{
The theoretical details can be found in Kustosz, Mueller and Wendler (2016) and Kustosz and Mueller (2014). The computational details are in Kustosz (2016).
}
\value{
  The function returns a list with
  \item{estimate }{Parameter maximising the selected depth notion.}
  \item{value}{Depth at the resulting maximum.}
  \item{numit}{Number of iterations.}
}
\references{
Kustosz, C. (2016). Depth based estimators and tests for
autoregressive processes with application. Ph. D. thesis. TU Dortmund.\cr 
\cr
Kustosz C., Mueller Ch. H. and Wendler M. (2016). Simplified Simplicial Depth for Regression and
Autoregressive Growth Processes. Journal of Statistical Planning and Inference. In press.\cr
\cr
Kustosz C. and Mueller Ch. H. (2014). Analysis of crack growth with robust distribution-
free estimators and tests for nonstationary autoregressive processes. Statistical
Papers 55, 125-140.\cr
}
\author{
  Kustosz, Christoph
}

  
  \seealso{
    \code{\link{dS1_lin2}},\code{\link{dS2_lin2}},\code{\link{dS3_lin2}},\code{\link{dS_lin1}},\code{\link{Tri_Mid}},\code{\link{lin1_theta}},\code{\link{Ele_Norm}}
  }
\examples{
  y <- RandomARMod_lin2(250, 0, 1.003, 15, 1)
  \dontrun{
    est_lin1(y)
  }
}

