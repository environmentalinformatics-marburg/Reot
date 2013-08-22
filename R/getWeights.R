getWeights <- function(x) {
  rads <- deg2rad(coordinates(x)[, 2])
  cos(rads)
}