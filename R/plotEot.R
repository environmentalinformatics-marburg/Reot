plotEot <- function(eot.outlist,
                    eot = 1,
                    pred.prm = "rsq.predictor",
                    resp.prm = "rsq.response",
                    add.map = TRUE,
                    clr = colorRampPalette(
                      rev(brewer.pal(9, "Spectral")))(1000))
{
  
  xy <- xyFromCell(eot.outlist[[1]]$rsq.predictor[[eot]], 
                   cell = eot.outlist[[1]]$max.xy[eot])
  
  eot.location.p <- xyplot(xy[1, 2] ~ xy[1, 1], cex = 2,
                           pch = 21, fill = "grey80", col = "black")
  
  if (isTRUE(add.map)) {
    mm <- map("world", plot = F, fill = T, col = "grey70")
  }
  
  pred.p <- spplot(eot.outlist[[1]][[pred.prm]][[eot]], mm = mm,
                   colorkey = list(space = "top",
                                   width = 0.7, height = 0.8), 
                   main = pred.prm, 
                   col.regions = clr, panel = function(..., mm) {
                     panel.levelplot(...)
                     if (isTRUE(add.map)) {
                     panel.polygon(mm$x, mm$y, lwd = 0.5, 
                                   border = "grey20")
                     }
                     }) +
    as.layer(eot.location.p)
  
  resp.p <- spplot(eot.outlist[[1]][[resp.prm]][[eot]], mm = mm,
                   colorkey = list(space = "top",
                                   width = 0.7, height = 0.8), 
                   main = resp.prm, 
                   col.regions = clr, panel = function(..., mm) {
                     panel.levelplot(...)
                     if (isTRUE(add.map)) {
                       panel.polygon(mm$x, mm$y, lwd = 0.5, 
                                     border = "grey20")
                     }
                   }) +
    as.layer(eot.location.p)
  
  ### clear plot area
  grid.newpage()
  
  ### define first plotting region (viewport)
  vp1 <- viewport(x = 0, y = 0, 
                  height = 1, width = 0.5,
                  just = c("left", "bottom"),
                  name = "left")
  
  ### enter vp1 
  pushViewport(vp1)
  
  ### plot a plot - needs to be printed (and newpage set to FALSE)!!!
  print(pred.p, newpage = FALSE)
  
  ### leave vp1 - up one level (into root vieport)
  upViewport(1)
  
  ### define second plot area
  vp2 <- viewport(x = 1, y = 0, 
                  height = 1, width = 0.5,
                  just = c("right", "bottom"),
                  name = "right")
  
  ### enter vp2
  pushViewport(vp2)
  
  ### plot another plot
  print(resp.p, newpage = FALSE)
  
  ### leave vp2
  upViewport(0)
  
}
