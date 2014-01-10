nEot4Var <- function(eot.list, var = 1) {
  expl.var <- sapply(seq(eot.list[[1]]), function(i) {
    eot.list[[1]][[i]]$exp.var
  })
  
  result <- Reduce("+", expl.var, accumulate = TRUE)
  max(which(var - result >= 0), na.rm = TRUE)
  
}