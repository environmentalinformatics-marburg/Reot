#' (Enter brief description for Reot function 'covWeight()')
#' 
#' @export covWeight
covWeight <- function(y, weights, ...) {
  
  cov.wt(na.exclude(y), weights, cor = TRUE, ...)
  
}