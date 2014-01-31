#' Create a weighted covariance matrix
#' 
#' @param y a matrix (e.g. as returned by \code{\link{getValues}})
#' @param weights a numeric vector of weights. For lat/lon data this 
#' can be produced with \code{\link{getWeights}}
#' 
#' @export covWeight
covWeight <- function(y, weights, ...) {
  
  cov.wt(na.exclude(y), weights, cor = TRUE, ...)
  
}