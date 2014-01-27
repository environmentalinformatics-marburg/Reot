\name{plotLocations}
\alias{plotLocations}
\title{Simple plotting routine to visualise the location of all identified base
points colour coded according to eot mode (1 to n).}
\usage{
plotLocations(eot.obj, ...)
}
\arguments{
  \item{eot.obj}{an EOT object as returned by
  \code{\link{eot}}}
}
\description{
Simple plotting routine to visualise the location of all
identified base points colour coded according to eot mode
(1 to n).
}
\examples{
data(vdendool)

# claculate 4 leading modes
modes <- eot(pred = vdendool, resp = NULL, n = 4, reduce.both = FALSE,
             standardised = FALSE, print.console = TRUE)

plotLocations(modes)
}

