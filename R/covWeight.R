#' Create a weighted covariance matrix
#' 
#' @param y a matrix (e.g. as returned by \code{\link{getValues}})
#' @param weights a numeric vector of weights. For lat/lon data this 
#' can be produced with \code{\link{getWeights}}
#' @param ... additional arguments passed to \code{\link{cov.wt}}
#' 
#' @return
#' see \code{\link{cov.wt}}
#' 
#' @seealso
#' \code{\link{cov.wt}}
#' 
#' @export covWeight
covWeight <- function(y, weights, ...) {
  
  cov.wt(na.exclude(y), weights, cor = TRUE, ...)
  
}