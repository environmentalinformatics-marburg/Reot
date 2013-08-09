EotControl <- function(pred, 
                       resp = NULL, 
                       n = 1, 
                       write.out = F,
                       path.out = ".", 
                       names.out = NULL,
                       cycle.window = NULL, 
                       ...) {
  
  
  ### Environmental settings
  
  # Required functions
  # source("src/Eot.R")
  
  # Duplicate predictor set in case predictor and response are identical
  if (is.null(resp)) {
    resp <- pred  
    resp.eq.pred = T
  } else {
    resp.eq.pred = F
  }
  
  # Cycle window if not provided
  if (is.null(cycle.window))
    cycle.window <- nlayers(pred)
  
  
  ### EOT
  
  # Loop through RasterStacks by specified cycle.window (e.g. 12 for one year)
  pred.eot <- lapply(seq(1, nlayers(pred), cycle.window), function(i) {
    
    # User-defined iterations
    for (z in seq(n)) {
      # Use initial response data set in case of first iteration
      if (z == 1) {
        pred.eot <- Eot(pred = pred[[i:(i+cycle.window-1)]], 
                             resp = resp[[i:(i+cycle.window-1)]],
                             resp.eq.pred = resp.eq.pred,
                             n = z, 
                             write.out = write.out,
                             path.out = path.out, 
                             names.out = if (!is.null(names.out) | write.out) {
                               names.out[ceiling(i/cycle.window)]
                             } else {
                               NULL
                             })
                
        # Use last entry of slot 'residuals' otherwise  
      } else if (z > 1) {
        tmp.pred.eot <- Eot(pred = pred[[i:(i+cycle.window-1)]], 
                            resp = if (z == 2) {pred.eot$resid.response 
                            } else {pred.eot[[z-1]]$resid.response}, 
                            resp.eq.pred = resp.eq.pred,
                            n = z, 
                            write.out = write.out,
                            path.out = path.out, 
                            names.out = if (!is.null(names.out) | write.out) {
                              names.out[ceiling(i/cycle.window)]
                            } else {
                              NULL
                            })
        
        if (z == 2) {
          pred.eot <- list(pred.eot, tmp.pred.eot)
          names(pred.eot) <- c("EOT_1", paste("EOT", z, sep = "_"))
        } else {
          tmp.names <- names(pred.eot)
          pred.eot <- append(pred.eot, list(tmp.pred.eot))
          names(pred.eot) <- c(tmp.names, paste("EOT", z, sep = "_"))
        }
      }
    }
    
    return(pred.eot)
    
  })
  
  # Return output list
  return(pred.eot)
  
}
