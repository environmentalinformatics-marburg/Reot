#' (Enter brief description for Reot function 'getWeights()')
#' 
#' @export getWeights
getWeights <- function(x) {
  
  cos(deg2rad(coordinates(x)[, 2][!is.na(x[[1]][])]))
  
}
