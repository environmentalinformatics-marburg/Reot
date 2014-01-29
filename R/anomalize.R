#' Create an anomaly RasterStack 
#' 
#' @description
#' The function creates an anomaly raster stack either based on the
#' overall mean of the original stack, or a supplied reference raster.
#' For the creation of seasonal anomalies use \code{\link{deseason}}.
#' 
#' @param x a raster stack
#' @param reference an optional raszer layer to be used as the reference 
#' @param ... additional arguments passed to \code{\link{calc}} 
#' which is used under the hood
#' 
#' @export anomalise
#' 
#' @examples
#' data(australiaGPCP)
#' 
#' aus.anom <- anomalize(australiaGPCP)
#' 
#' opar <- par(mfrow = c(1,2))
#' plot(australiaGPCP[[1]], main = "original")
#' plot(aus.anom[[1]], main = "anomalized")
#' par(opar)
anomalize <- function(x, 
                      reference = NULL, 
                      ...) {
  
  if (is.null(reference)) {
    mn <- calc(x, fun = mean, ...)
  } else {
    mn <- reference
  }
  
  return(x - mn)
  
}