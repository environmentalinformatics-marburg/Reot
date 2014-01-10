#' Noise filtering through principal components. The user can either specify
#' how many components to keep or can specify a minimum value for the variance that
#' should be kept.
#' 
#' @export denoise
denoise <- function(data,
                    k = NULL,
                    expl.var = 0.95,
                    weighted = TRUE,
                    ...) {

  x <- data[]
  #x[is.na(x)] <- 0
  
  # PCA
  if (weighted) { 
    pca <- princomp(~ x, covmat = covWeight(x, getWeights(data)), 
                    scores = TRUE, na.action = na.exclude)
  } else {
    pca <- princomp(~ x, scores = TRUE, na.action = na.exclude)
  }
  
  if (is.null(k)) {
    k <- which(cumsum(pca$sdev^2 / sum(pca$sdev^2)) >= expl.var)[1]
  } else {
    expl.var <- cumsum(pca$sdev^2 / sum(pca$sdev^2))[k]
  }
  
  #eivecs <- as.matrix(pca$loadings[, 1:k])
  #pvals <- pca$scores[, 1:k]
  #cent <- pca$center
  
  cat("\n",
      "Using the first ",
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
  
  # Insert reconstructed values in original data set 
  data.tmp <- brick(lapply(seq(recons), function(i) {
    tmp.data <- data[[i]]
    tmp.data[] <- recons[[i]]
    return(tmp.data)
  }))

  # Return denoised data set
  return(data.tmp)
}
