plotEot <- function(eot.outlist,
                    eot = 1,
                    pred.prm = "rsq.predictor",
                    resp.prm = "rsq.response",
                    show.eot.loc = FALSE,
                    add.map = TRUE,
                    times.vec = NULL,
                    clr = colorRampPalette(
                      rev(brewer.pal(9, "Spectral")))(1000))
{
  
  if (is.null(times.vec)) 
    times.vec <- seq(nlayers(eot.outlist[[1]]$resid.response[[1]]))
  
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
                   main = paste(pred.prm, "EOT", eot, sep = " "), 
                   col.regions = clr, panel = function(..., mm) {
                     panel.levelplot(...)
                     if (isTRUE(add.map)) {
                     panel.polygon(mm$x, mm$y, lwd = 0.5, 
                                   border = "grey20")
                     }
                     }) 
  
  resp.p <- spplot(eot.outlist[[1]][[resp.prm]][[eot]], mm = mm,
                   colorkey = list(space = "top",
                                   width = 0.7, height = 0.8), 
                   main = paste(resp.prm, "EOT", eot, sep = " "), 
                   col.regions = clr, panel = function(..., mm) {
                     panel.levelplot(...)
                     if (isTRUE(add.map)) {
                       panel.polygon(mm$x, mm$y, lwd = 0.5, 
                                     border = "grey20")
                     }
                   }) 
  
  eot.ts <- xyplot(eot.outlist[[1]]$eot.series[[eot]][1, ] ~ times.vec,
                   type = "b", pch = 20, col = "black", 
                   ylab = "", xlab = "",
                   scales = list(tck = c(0.5, 0), x = list(axs = "i")), 
                   main = paste("time series EOT", eot, sep = " ")) 
  
  ### clear plot area
  grid.newpage()
  
  ### define first plotting region (viewport)
  vp1 <- viewport(x = 0, y = 1, 
                  height = 0.7, width = 0.5,
                  just = c("left", "top"),
                  name = "left")
  
  ### enter vp1 
  pushViewport(vp1)
  
  ### plot a plot - needs to be printed (and newpage set to FALSE)!!!
  if (!show.eot.loc) print(pred.p, newpage = FALSE) else
    print(pred.p + 
            as.layer(eot.location.p), 
          newpage = FALSE)
  
  ### leave vp1 - up one level (into root vieport)
  upViewport(1)
  
  ### define second plot area
  vp2 <- viewport(x = 1, y = 1, 
                  height = 0.7, width = 0.5,
                  just = c("right", "top"),
                  name = "right")
  
  ### enter vp2
  pushViewport(vp2)
  
  ### plot another plot
  if (!show.eot.loc) print(resp.p, newpage = FALSE) else
    print(resp.p + 
            as.layer(eot.location.p), 
          newpage = FALSE)
  
  ### leave vp2
  upViewport(1)
  
  ### define second plot area
  vp3 <- viewport(x = 0.05, y = 0, 
                  height = 0.3, width = 0.9,
                  just = c("left", "bottom"),
                  name = "botoom")
  
  ### enter vp2
  pushViewport(vp3)
  
  ### plot another plot
  print(eot.ts, newpage = FALSE)
  
  upViewport(0)
}
