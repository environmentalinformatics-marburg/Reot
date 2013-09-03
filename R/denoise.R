denoise <- function(data,
                    k = NULL,
                    expl.var = 0.95,
                    #n.cores = NULL,
                    weighted = TRUE,
                    ...) {

  # Required packages
  stopifnot(require(doParallel))
  
  x <- data[]
  
  # PCA
  if (isTRUE(weighted)) { 
    #w <- getWeights(data)
    #cm <- covWeight(x, getWeights(data))
    pca <- princomp(x, covmat = covWeight(x, getWeights(data)), 
                    scores = TRUE)
  } else {
    pca <- princomp(x, scores = TRUE)
  }
  
  if (is.null(k)) 
    k <- which(cumsum(pca$sdev^2 / sum(pca$sdev^2)) >= expl.var)[1]
  
  if (!is.null(k)) 
    expl.var <- cumsum(pca$sdev^2 / sum(pca$sdev^2))[k]
  
  #eivecs <- as.matrix(pca$loadings[, 1:k])
  #pvals <- pca$scores[, 1:k]
  #cent <- pca$center
  
  cat("\n",
      "using the first ",
      k,
      " components (of ",
      nlayers(data),
      ") to reconstruct series...\n",
      " these account for ",
      expl.var,
      " of variance in orig. series\n\n", 
      sep = "")
  
  # Reconstruction
  recons <- lapply(seq(nlayers(data)), function(i) {
    rowSums(t(as.matrix(pca$loadings[, 1:k])[i, ] * 
                t(pca$scores[, 1:k]))) + pca$center[i]
  })
  
  # Parallelization
#   if (is.null(n.cores)) 
#     n.cores <- detectCores()
  
#   registerDoParallel(cl <- makeCluster(n.cores))

  # Insert reconstructed values in original data set 
  data.tmp <- do.call("brick", 
                      foreach(i = seq(recons)) #, .packages = "raster") 
                      %do% {
                        tmp.data <- data[[i]]
                        tmp.data[] <- recons[[i]]
                        return(tmp.data)
                      })

  # Deregister parallel backend
#   stopCluster(cl)

  # Return denoised data set
  return(data.tmp)
}
