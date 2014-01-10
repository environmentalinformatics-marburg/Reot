#' Function to identify the number of modes needed to explain a certain amount of
#' variance within the response series.
#' 
#' @export nEot4Var
nEot4Var <- function(eot.list, var = 1) {
  expl.var <- sapply(seq(eot.list[[1]]), function(i) {
    eot.list[[1]][[i]]$exp.var
  })
  
  result <- Reduce("+", expl.var, accumulate = TRUE)
  max(which(var - result >= 0), na.rm = TRUE)
  
}