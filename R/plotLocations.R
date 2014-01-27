#' Simple plotting routine to visualise the location of all identified base
#' points colour coded according to eot mode (1 to n).
#' 
#' @param eot.obj an EOT object as returned by \code{\link{eot}}
#' 
#' @export plotLocations
#' 
#' @examples
#' data(vdendool)
#' 
#' # claculate 4 leading modes
#' modes <- eot(pred = vdendool, resp = NULL, n = 4, reduce.both = FALSE,
#'              standardised = FALSE, print.console = TRUE)
#'
#' plotLocations(modes)
plotLocations <- function(eot.obj, ...) {

  ### some helper functions especially for the grid layout
  ### resizingTextGrob from 
  ### http://ryouready.wordpress.com/2012/08/01/
  resizingTextGrob <- function(..., scale.fact = 1) {
    
    grob(tg = textGrob(...), cl = "resizingTextGrob", 
         scale.fact = scale.fact)
    
  }
  
  drawDetails.resizingTextGrob <- function(x, scale.fact, recording = TRUE) {
    
    grid.draw(x$tg)
    
  }
  
  preDrawDetails.resizingTextGrob <- function(x, ...) {
    
    library(scales)
    
    h <- convertHeight(unit(1, "npc"), "mm", valueOnly = TRUE)
    fs <- rescale(h, to = c(80, 15), from = c(120, 20)) * x$scale.fact
    pushViewport(viewport(gp = gpar(fontsize = fs)))
    
  }
  
  postDrawDetails.resizingTextGrob <- function(x) popViewport()
  
  
  ### plot function
  loc.df <- as.data.frame(do.call("rbind", 
                                  lapply(seq(eot.obj), function(i) {
    xyFromCell(eot.obj[[i]]$rsq.predictor, 
               cell = eot.obj[[i]]$max.xy)
  })))
  
  loc.df$eot <- paste("EOT", sprintf("%02.f", seq(eot.obj)), 
                      sep = "_")
  
  mm <- map("world", plot = FALSE, fill = TRUE)
  px.pred <- ncell(eot.obj$EOT_1$r.predictor)
  
  pred.p <- spplot(eot.obj$EOT_1$rsq.predictor, 
                   mm = mm, maxpixels = px.pred,
                   colorkey = FALSE, 
                   col.regions = "grey50", panel = function(..., mm) {
                     panel.levelplot(...)
                     panel.polygon(mm$x, mm$y, lwd = 0.5, 
                                   border = "grey20", col = "grey70")
                   }) 
  
  clrs.hcl <- function(n) {
    hcl(h = seq(270, 0, length.out = n), 
        c = 70, l = 50, fixup = TRUE)
  }
  
  clrs <- clrs.hcl(length(eot.obj))
  
  points.p <- xyplot(y ~ x, data = loc.df, col = "black", 
                     fill = clrs, pch = 21,
                     cex = 2)
  
  out <- pred.p + as.layer(points.p)
  
  n <- length(eot.obj)
  
  grid.newpage()
  
  map.vp <- viewport(x = 0, y = 0, 
                     height = 1, width = 0.85,
                     just = c("left", "bottom"))
  
  pushViewport(map.vp)
  
  print(out, newpage = FALSE)
    
  downViewport(trellis.vpname(name = "figure"))
  
  leg.vp <- viewport(x = 1, y = 0.5, 
                     height = n / 10, width = 0.15,
                     just = c("left", "centre"))
  
  pushViewport(leg.vp)  
  
  if(n == 1) ypos <- 0.5 else ypos <- seq(0.95, 0.05, length.out = n + 2)
  if(n == 1) ypos <- ypos else ypos <- ypos[-c(1, length(ypos))]
  xpos.pts <- unit(0.15, "npc")
  size.pts <- 1 / n
  
  for (i in 1:n) {
    
    vp <- viewport(x = xpos.pts, y = ypos[i], 
                   height = size.pts, width = 0.1,
                   just = c("left", "centre"))
    
    pushViewport(vp)
    
    grid.circle(gp = gpar(fill = clrs[i], 
                          col = "black", ...))
    
    upViewport()
    
  }
  
    xpos.txt <- unit(0.25, "npc")
    width.txt <- 0.7
    
    for (i in 1:n) {
      
      vp <- viewport(x = xpos.txt, y = ypos[i], 
                     height = size.pts, width = width.txt,
                     just = c("left", "centre"))
      
      pushViewport(vp)
      
      txt <- resizingTextGrob(x = 0.2, sort(names(eot.obj))[i],
                              just = "left")
      
      grid.draw(txt)
      
      popViewport()
      
    }
  
  upViewport(0)
  
    
  #return(eot.map)
 
}
