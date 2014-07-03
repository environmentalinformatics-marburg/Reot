#' Shorten a RasterStack
#' 
#' @description
#' The function cuts a specified number of layers off a RrasterStack in 
#' order to create lagged RasterStacks.
#' 
#' @param data a RasterStack
#' @param tail logical. If \code{TRUE} the layers will be taken away from
#' the end of the stack. If \code{FALSE} layers will be taken away from 
#' the beginning.
#' @param lag the number of layers to take away.
#' 
#' @return a RasterStack shortened by \code{lag} either from the 
#' beginning or the end, depending on the specification of \code{tail}
#' 
#' @examples
#' data(australiaGPCP)
#' 
#' # 6 layers from the beginning
#' cutStack(australiaGPCP, tail = FALSE, lag = 6)
#' # 8 layers from the end
#' cutStack(australiaGPCP, tail = TRUE, lag = 8)
#' 
#' @export cutStack
cutStack <- function(data, 
                     tail = TRUE,
                     lag = NULL) {
  
  # Return unmodified RasterStack if lag == NULL
  if (is.null(lag)) {
    return(data)
  } else {
    # Supplied RasterStack is predictor:
    if (tail) {
      return(data[[1:(nlayers(data)-lag)]])
    # Supplied RasterStack is response:  
    } else {
      return(data[[(lag+1):nlayers(data)]])
    }
  }
}