deseason <- function(data, 
                     cycle.window = 12,
                     #n.cores = NULL,
                     ...) {
  
  ### Environmental settings
  
  # Required packages
  lib <- c("raster", "parallel")
  sapply(lib, function(...) stopifnot(require(..., character.only = T)))
  
  # Parallelization
#   if (is.null(n.cores)) 
#     n.cores <- detectCores()
#   
#   clstr <- makePSOCKcluster(n.cores)
#   clusterEvalQ(clstr, c(library(raster), library(rgdal)))
#   
  
  ### Deseasoning
  
  # Calculate long-term monthly averages
#   clusterExport(clstr, c("data", "cycle.window"), envir = environment())
  
#   data.mv <- do.call("stack", rep(parLapply(clstr, 1:cycle.window, function(i) {
#     calc(data[[seq(i, nlayers(data), cycle.window)]], fun = mean)
#   }), nlayers(data) / cycle.window))
  
  data.mv <- do.call("stack", rep(lapply(1:cycle.window, function(i) {
    calc(data[[seq(i, nlayers(data), cycle.window)]], fun = mean)
  }), nlayers(data) / cycle.window))
  
  # Subtract monthly averages from actually measured values
  data.dsn <- data - data.mv
    
  # Deregister parallel backend
#   stopCluster(clstr)
  
  # Return output
  return(data.dsn)
}
