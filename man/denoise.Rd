\name{denoise}
\alias{denoise}
\title{Noise filtering through principal components.}
\usage{
denoise(data, k = NULL, expl.var = 0.95, weighted = TRUE, ...)
}
\arguments{
  \item{data}{raster stack to be filtered}

  \item{k}{number of components to be kept for
  reconstruction (set this to NULL if you supply
  \code{expl.var})}

  \item{expl.var}{minimum amount of variance to be kept
  after reconstruction (not used if \code{k} is supplied)}

  \item{weighted}{logical. If \code{TRUE} the covariance
  matrix will be geographically weighted using the cosine
  of latitude during decomposition (only important for
  lat/lon data)}

  \item{...}{additional arguments passed to
  \code{\link{princomp}}}
}
\value{
the denoised raster stack object
}
\description{
Filter noise from a raster stack by decomposing into
principal components and subsequent reconstruction using
only a subset of components
}
\examples{
data("australiaGPCP")
aus.dns <- denoise(australiaGPCP, expl.var = 0.8)

opar <- par(mfrow = c(1,2))
plot(australiaGPCP[[1]], main = "original")
plot(aus.dns[[1]], main = "denoised")
par(opar)
}

