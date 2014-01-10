\name{eot}
\alias{eot}
\title{The core function of the package, calculates EOT based on user-supplied
predictor and (optional) response RasterStack.}
\usage{
eot(pred, resp = NULL, n = 1, standardised = TRUE, write.out = FALSE,
  path.out = ".", names.out = NULL, cycle.window = NULL,
  reduce.both = TRUE, type = c("rsq", "ioa"), print.console = TRUE, ...)
}
\description{
The core function of the package, calculates EOT based on
user-supplied predictor and (optional) response
RasterStack.
}

