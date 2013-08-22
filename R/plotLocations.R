plotLocations <- function(eot.obj) {

  library(ggplot2)
  
  wrld <- map_data("world")
  
  loc.df <- as.data.frame(do.call("rbind", 
                                  lapply(seq(eot.obj[[1]]), function(i) {
    xyFromCell(eot.obj[[1]][[i]]$rsq.predictor, 
               cell = eot.obj[[1]][[i]]$max.xy)
  })))
  
  loc.df$eot <- paste("EOT", sprintf("%02.f", seq(eot.obj[[1]])), 
                      sep = "_")
  
  p <- ggplot() + coord_fixed()
  
  base.world <- p + geom_polygon(data = wrld,
                                 aes(x = long,
                                     y = lat,
                                     group = group),
                                 fill = "grey70") +
    theme_bw()
  
  eot.map <- base.world +
    geom_point(data = loc.df, aes(x = x,  y = y,  
                                  colour = eot, size = 5)) +
    guides(size = FALSE)
  
  return(eot.map)
 
}
