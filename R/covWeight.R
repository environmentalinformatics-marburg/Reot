covWeight <- function(y, weights) {
  
  cm <- cov.wt(y, weights, cor = TRUE)
  return(cm)
  
}