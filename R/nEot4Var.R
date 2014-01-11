#' Function to identify the number of modes needed to explain a certain amount of
#' variance within the response series.
#' 
#' @export nEot4Var
nEot4Var <- function(eot.list, var = 0.9) {
  expl.var <- sapply(seq(eot.list[[1]]), function(i) {
    eot.list[[1]][[i]]$exp.var
  })

  max(which(var - expl.var >= 0), na.rm = TRUE)
  
}