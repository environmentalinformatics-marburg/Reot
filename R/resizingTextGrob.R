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