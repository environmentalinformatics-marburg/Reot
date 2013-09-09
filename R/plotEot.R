plotEot <- function(eot.obj,
                    eot = 1,
                    pred.prm = "rsq.predictor",
                    resp.prm = "rsq.response",
                    show.eot.loc = FALSE,
                    anomalies = TRUE,
                    add.map = TRUE,
                    times.vec = NULL,
                    arrange = c("wide", "long"),
                    clr = colorRampPalette(
                      rev(brewer.pal(9, "Spectral")))(1000),
                    ...)
{
  
  if (is.null(times.vec)) 
    times.vec <- seq(nlayers(eot.obj[[1]][[1]]$resid.response))
  
  xy <- xyFromCell(eot.obj[[1]][[eot]]$rsq.predictor, 
                   cell = eot.obj[[1]][[eot]]$max.xy)
  
  eot.location.p <- xyplot(xy[1, 2] ~ xy[1, 1], cex = 2,
                           pch = 21, fill = "grey80", col = "black")
  
  if (isTRUE(add.map)) {
    mm <- map("world", plot = F, fill = T, col = "grey70")
  }
  
  px.pred <- ncell(eot.obj[[1]][[eot]]$r.predictor)
  px.resp <- ncell(eot.obj[[1]][[eot]]$r.response)
  
  pred.p <- spplot(eot.obj[[1]][[eot]][[pred.prm]], 
                   mm = mm, maxpixels = px.pred,
                   colorkey = list(space = "top",
                                   width = 0.7, height = 0.8), 
                   main = paste(pred.prm, "EOT", eot, sep = " "), 
                   col.regions = clr, panel = function(..., mm) {
                     panel.levelplot(...)
                     if (isTRUE(add.map)) {
                     panel.polygon(mm$x, mm$y, lwd = 0.5, 
                                   border = "grey20")
                     }
                     }, ...) 
  
  if (show.eot.loc) pred.p <- pred.p + as.layer(eot.location.p)
  
  resp.p <- spplot(eot.obj[[1]][[eot]][[resp.prm]], 
                   mm = mm, maxpixels = px.resp,
                   colorkey = list(space = "top",
                                   width = 0.7, height = 0.8), 
                   main = paste(resp.prm, "EOT", eot, sep = " "), 
                   col.regions = clr, panel = function(..., mm) {
                     panel.levelplot(...)
                     if (isTRUE(add.map)) {
                       panel.polygon(mm$x, mm$y, lwd = 0.5, 
                                     border = "grey20")
                     }
                   }, ...) 
  
  if (show.eot.loc) resp.p <- resp.p + as.layer(eot.location.p)
  
  eot.ts <- xyplot(eot.obj[[1]][[eot]]$eot.series[1, ] ~ times.vec,
                   type = "b", pch = 20, col = "black", 
                   ylab = "", xlab = "",
                   scales = list(tck = c(0.5, 0), x = list(axs = "i")), 
                   main = paste("time series EOT", eot, sep = " ")) 
  
  if (anomalies) {
    eot.ts <- eot.ts + layer(panel.abline(h = 0, col = "grey40", lty = 3), 
                             under = TRUE)
  }

  ### set layout to wide or long
  arrange <- arrange[1]
  if (arrange == "wide") ncls <- 2 else ncls <- 1
  
  ### amalgamate pred.p and resp.p according to layout
  c.pred.resp <- arrangeGrob(pred.p, resp.p, ncol = ncls)
  
  ### clear plot area
  grid.newpage()
  
  ### combine c.pred.resp and eot time series and plot
  grid.arrange(c.pred.resp, eot.ts, heights = c(1, 0.5), ncol = 1)
  
}
