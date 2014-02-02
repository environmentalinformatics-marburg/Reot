#' Create seasonal anomalies
#' 
#' @description
#' The function calculates anomalies of a RasterStack by supplying a 
#' suitable seasonal window. E. g. to create monthly anomalies of a 
#' raster stack of 12 layers per year, use \code{cycle.window = 12}.
#' 
#' @param data a RasterStack
#' @param cycle.window the window for the creation of the anomalies
#' @param ... currently not used
#' 
#' @return a deseasoned RasterStack
#' 
#' @seealso
#' \code{\link{anomalize}}, \code{\link{denoise}}
#' 
#' @export deseason
#' 
#' @examples 
#' data("australiaGPCP")
#' 
#' aus.dsn <- deseason(australiaGPCP, 12)
#' 
#' opar <- par(mfrow = c(1,2))
#' plot(australiaGPCP[[1]], main = "original")
#' plot(aus.dsn[[1]], main = "deseasoned")
#' par(opar)
deseason <- function(data, 
                     cycle.window = 12,
                     ...) {
  
  # Calculate layer averages based on supplied seasonal window
  data.mv <- stack(rep(lapply(1:cycle.window, function(i) {
    calc(data[[seq(i, nlayers(data), cycle.window)]], fun = mean)
  }), nlayers(data) / cycle.window))
  
  # Subtract monthly averages from actually measured values
  data.dsn <- data - data.mv
    
  # Return output
  return(data.dsn)
}
