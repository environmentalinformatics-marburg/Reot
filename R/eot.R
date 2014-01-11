#' The core function of the package, calculates EOT based on user-supplied
#' predictor and (optional) response RasterStack.
#' 
#' @export eot
eot <- function(pred, 
                resp = NULL, 
                n = 1, 
                standardised = TRUE, 
                write.out = FALSE,
                path.out = ".", 
                names.out = NULL,
                cycle.window = NULL,
                reduce.both = FALSE, 
                type = c("rsq", "ioa"),
                print.console = TRUE,
                ...) {
  
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
  
  if (!standardised) {
    t <- mean(apply(getValues(resp), 1, var, na.rm = TRUE), na.rm = TRUE)
    s <- mean(apply(getValues(resp), 2, var, na.rm = TRUE), na.rm = TRUE)
    orig.var <- t + s
  } else {
    orig.var <- var(as.vector(getValues(resp)), na.rm = TRUE)
  }
  
  
  ### EOT
  
  # Loop through RasterStacks by specified cycle.window (e.g. 12 for one year)
  #pred.eot <- 
  lapply(seq(1, nlayers(pred), cycle.window), function(i) {
    
    # User-defined iterations
    for (z in seq(n)) {
      # Use initial response data set in case of first iteration
      if (z == 1) {
        pred.eot <- EotCycle(pred = pred[[i:(i+cycle.window-1)]], 
                             resp = resp[[i:(i+cycle.window-1)]],
                             resp.eq.pred = resp.eq.pred,
                             n = z, 
                             type = type,
                             standardised = standardised, 
                             orig.var = orig.var,
                             write.out = write.out,
                             path.out = path.out, 
                             print.console = print.console,
                             names.out = if (!is.null(names.out) | write.out) {
                               names.out[ceiling(i/cycle.window)]
                             } else {
                               NULL
                             })
                
        # Use last entry of slot 'residuals' otherwise  
      } else if (z > 1) {
        tmp.pred.eot <- EotCycle(
          pred = if (!reduce.both) {
            pred[[i:(i+cycle.window-1)]]
          } else {
            if (z == 2) {
              pred.eot$resid.predictor
            } else {
              pred.eot[[z-1]]$resid.predictor
            }
          }, 
          resp = if (z == 2) {
            pred.eot$resid.response 
          } else {
            pred.eot[[z-1]]$resid.response
          }, 
          resp.eq.pred = resp.eq.pred,
          n = z, 
          type = type,
          standardised = standardised, 
          orig.var = orig.var,
          write.out = write.out,
          path.out = path.out,  
          print.console = print.console,
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
  
}
