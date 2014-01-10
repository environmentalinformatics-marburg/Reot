#' Function to perform geographic weighting of user-supplied RasterStacks.
#' 
#' @export geoWeight
geoWeight <- function(x) {
  x.vals <- x[]
  rads <- deg2rad(coordinates(x)[, 2])
  x.weightd <- x.vals * cos(rads)
  x[] <- x.weightd
  return(x)
}