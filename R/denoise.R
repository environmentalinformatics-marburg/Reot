#' Noise filtering through principal components. 
#' 
#' Filter noise from a raster stack by decomposing into principal components 
#' and subsequent reconstruction using only a subset of components
#' 
#' @param data raster stack to be filtered
#' @param k number of components to be kept for reconstruction 
#' (set this to NULL if you supply \code{expl.var})
#' @param expl.var  minimum amount of variance to be kept after reconstruction
#' (not used if \code{k} is supplied)
#' @param weighted logical. If \code{TRUE} the covariance matrix will be 
#' geographically weighted using the cosine of latitude during decomposition 
#' (only important for lat/lon data)
#' @param ... additional arguments passed to \code{\link{princomp}}
#' @return the denoised raster stack object
#' 
#' @export denoise
#' 
#' @examples
#' data("australiaGPCP")
#' aus.dns <- denoise(australiaGPCP, expl.var = 0.8)
#' 
#' opar <- par(mfrow = c(1,2))
#' plot(australiaGPCP[[1]], main = "original")
#' plot(aus.dns[[1]], main = "denoised")
#' par(opar)
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
                    scores = TRUE, na.action = na.exclude, ...)
  } else {
    pca <- princomp(~ x, scores = TRUE, na.action = na.exclude, ...)
  }
  
  # declare reconstruction characteristics according to supplied values
  if (is.null(k)) {
    k <- which(cumsum(pca$sdev^2 / sum(pca$sdev^2)) >= expl.var)[1]
  } else {
    expl.var <- cumsum(pca$sdev^2 / sum(pca$sdev^2))[k]
  }

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
