covWeight <- function(y, weights, ...) {
  
  cov.wt(na.exclude(y), weights, cor = TRUE, ...)
  
}