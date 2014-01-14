#' Create a lagged stack from predictor or response RasterStack.
#' 
#' @export lagalizeOne
lagalizeOne <- function(data, 
                        is.predictor = TRUE,
                        lag = NULL, 
                        ...) {
  
  # Return unmodified RasterStack if lag == NULL
  if (is.null(lag)) {
    return(data)
  } else {
    # Supplied RasterStack is predictor:
    if (is.predictor) {
      return(data[[1:(nlayers(data)-lag)]])
    # Supplied RasterStack is response:  
    } else {
      return(data[[(lag+1):nlayers(data)]])
    }
  }
}