denoise <- function(data,
                    k = NULL,
                    expl.var = 0.95,
                    n.cores = NULL, 
                    ...) {

  # Required packages
  stopifnot(require(doParallel))
  
  # PCA
  data.vals <- data[]
  data.vals_pca <- prcomp(data.vals)
  
  if (is.null(k)) 
    k <- which(cumsum(data.vals_pca$sdev^2 / sum(data.vals_pca$sdev^2)) 
               >= expl.var)[1]
  
  if (!is.null(k)) 
    expl.var <- cumsum(data.vals_pca$sdev^2 / sum(data.vals_pca$sdev^2))[k]
  
  eivecs <- data.vals_pca$rotation[, 1:k]
  pvals <- data.vals_pca$x[, 1:k]
  cent <- data.vals_pca$center
  
  cat("\n",
      "using the first ",
      k,
      " components (of ",
      nlayers(data),
      ") to reconstruct series.\n",
      " these account for ",
      expl.var,
      " of variance in orig. series\n\n", 
      sep = "")
  
  
  # Reconstruction
  recons <- lapply(seq(nrow(eivecs)), function(i) {
    rowSums(t(eivecs[i, ] * t(pvals))) + cent[i]
  })
  
  # Parallelization
  if (is.null(n.cores)) 
    n.cores <- detectCores()
  
  clstr <- makeCluster(n.cores)
  clusterEvalQ(clstr, library(raster))

  # Insert reconstructed values in original data set
  clusterExport(clstr, c("data", "recons"), envir = environment())
  data.tmp <- do.call("brick", parLapply(clstr, seq(recons), function(i) {
    tmp.data <- data[[i]]
    tmp.recons <- recons[[i]]

    tmp.data[] <- tmp.recons
    return(tmp.data)
  }))

  # Check whether input is lat/lon and if then apply geographic weighting
  if (isLonLat(data)) data.tmp <- geoWeight(data.tmp)
  
  # Deregister parallel backend
  stopCluster(clstr)

  # Return denoised data set
  return(data.tmp)
}
