plotLocations <- function(eot.obj) {

  loc.df <- as.data.frame(do.call("rbind", 
                                  lapply(seq(eot.obj[[1]]), function(i) {
    xyFromCell(eot.obj[[1]][[i]]$rsq.predictor, 
               cell = eot.obj[[1]][[i]]$max.xy)
  })))
  
  loc.df$eot <- paste("EOT", sprintf("%02.f", seq(eot.obj[[1]])), 
                      sep = "_")
  
  mm <- map("world", plot = FALSE, fill = TRUE)
  px.pred <- ncell(eot.obj[[1]]$EOT_1$r.predictor)
  
  pred.p <- spplot(eot.obj[[1]]$EOT_1$rsq.predictor, 
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
  
  clrs <- clrs.hcl(length(eot.obj[[1]]))
  
  points.p <- xyplot(y ~ x, data = loc.df, col = "black", 
                     fill = clrs, pch = 21,
                     cex = 2)
  
  out <- pred.p + as.layer(points.p)
  
  n <- length(eot.obj[[1]])
  
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
                          col = "black"))
    
    upViewport()
    
  }
  
    xpos.txt <- unit(0.25, "npc")
    width.txt <- 0.7
    
    for (i in 1:n) {
      
      vp <- viewport(x = xpos.txt, y = ypos[i], 
                     height = size.pts, width = width.txt,
                     just = c("left", "centre"))
      
      pushViewport(vp)
      
      txt <- resizingTextGrob(x = 0.2, sort(names(eot.obj[[1]]))[i],
                              just = "left")
      
      grid.draw(txt)
      
      popViewport()
      
    }
  
  upViewport(0)
  
    
  #return(eot.map)
 
}
