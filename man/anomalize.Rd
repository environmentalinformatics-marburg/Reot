\name{anomalize}
\alias{anomalize}
\title{Create an anomaly RasterStack}
\usage{
anomalize(x, reference = NULL, ...)
}
\arguments{
  \item{x}{a raster stack}

  \item{reference}{an optional raszer layer to be used as
  the reference}

  \item{...}{additional arguments passed to
  \code{\link{calc}} which is used under the hood}
}
\description{
The function creates an anomaly raster stack either based
on the overall mean of the original stack, or a supplied
reference raster. For the creation of seasonal anomalies
use \code{\link{deseason}}.
}
\examples{
data(australiaGPCP)

aus.anom <- anomalize(australiaGPCP)

opar <- par(mfrow = c(1,2))
plot(australiaGPCP[[1]], main = "original")
plot(aus.anom[[1]], main = "anomalized")
par(opar)
}

