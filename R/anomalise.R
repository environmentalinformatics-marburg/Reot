anomalise <- function(x, reference = NULL) {
  
  if (is.null(reference)) {
    mn <- calc(x, fun = mean)
  } else mn <- reference
  
  x - mn
  
}