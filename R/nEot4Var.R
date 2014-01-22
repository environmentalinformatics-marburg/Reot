#' Function to identify the number of modes needed to explain a certain amount of
#' variance within the response series.
#' 
#' @export nEot4Var
nEot4Var <- function(eot.obj, var = 0.9) {
  expl.var <- sapply(seq(eot.obj), function(i) {
    eot.obj[[i]]$exp.var
  })

  min(which(var - expl.var <= 0), na.rm = TRUE)
  
}