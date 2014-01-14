#' Create an anomaly time series from a RasterStack, either based on the
#' overall mean of the stack, or a supplied reference raster.
#' 
#' @export anomalise
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