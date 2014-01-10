#' Create seasonal anomalies of a RasterStack by supplying a suitable seasonal
#' window.
#' 
#' @export deseason
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
