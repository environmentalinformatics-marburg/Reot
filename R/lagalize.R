#' Create a lagged list of stacks from predictor and response RasterStacks.
#' 
#' @export lagalize
lagalize <- function(data.pred, 
                     data.resp, 
                     lag = NULL, 
                     ...) {
  
  # Return list of unmodified RasterStacks if lag == NULL
  if (is.null(lag)) {
    return(list(data.pred, data.resp))
  } else {
    # Lagalize predictor stack
    data.pred.lag <- lagalizeOne(data = data.pred, is.predictor = TRUE, 
                                 lag = lag)
    # Lagalize response stack
    data.resp.lag <- lagalizeOne(data = data.resp, is.predictor = FALSE, 
                                 lag = lag)
    # Return list of lagalized stacks
    return(list(data.pred.lag, data.resp.lag))
  }
}