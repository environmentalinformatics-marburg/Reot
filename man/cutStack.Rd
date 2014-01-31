\name{cutStack}
\alias{cutStack}
\title{Shorten a raster stack}
\usage{
cutStack(data, tail = TRUE, lag = NULL, ...)
}
\arguments{
  \item{data}{a raster stack}

  \item{tail}{logical. If \code{TRUE} the layers will be
  taken away from the end of the stack. If \code{FALSE}
  layers will be taken away from the beginning.}

  \item{lag}{the number of layers to take away.}
}
\description{
The function cuts a specified number of layers off a raster
stack in order to create lagged raster stacks.
}
\examples{
data(australiaGPCP)

# 6 layers from the beginning
cutStack(australiaGPCP, tail = FALSE, lag = 6)
# 8 layers from the end
cutStack(australiaGPCP, tail = TRUE, lag = 8)
}

