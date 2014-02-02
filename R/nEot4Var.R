#' Number of EOTs needed for variance explanation
#' 
#' @description 
#' The function identifies the number of modes needed to explain a certain amount of
#' variance within the response series.
#' 
#' @param eot.obj the object returned by \code{\link{eot}}
#' @param var the minimum amount of variance explained by the modes
#' 
#' @note This is a post-hoc function like \code{\link{plotEot}} 
#' and \code{\link{plotLocations}}. This means that it needs an object 
#' which was created with \code{\link{eot}}. Depending on the potency
#' of the identified EOTs, it may be necessary to compute a high number of 
#' modes in order to be able to explain a large enough part of the variance
#' 
#' @return an integer denoting the number of EOTs needed to explain \code{var}
#' 
#' @export nEot4Var
#' 
#' @examples
#' data(vdendool)
#' 
#' modes <- eot(pred = vdendool, resp = NULL, n = 8, reduce.both = FALSE,
#'              standardised = FALSE, print.console = TRUE)
#'              
#' nEot4Var(modes, 0.65)
nEot4Var <- function(eot.obj, var = 0.9) {
  expl.var <- sapply(seq(eot.obj), function(i) {
    eot.obj[[i]]$exp.var
  })

  min(which(var - expl.var <= 0), na.rm = TRUE)
  
}