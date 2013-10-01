\name{Reot}
\alias{Reot}
\alias{Reot}
\docType{package}
\title{
Empirical Orthogonal Teleconnections in R
}
\description{
Empirical Orthogonal Teleconnections in R
}
\details{
\tabular{ll}{
Package: \tab Reot\cr
Type: \tab Package\cr
Version: \tab 0.1.1\cr
Date: \tab 2013-08-15\cr
License: \tab GNU GPL (>=3)\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Tim Appelhans, Florian Detsch

Maintainer: Tim Appelhans <tim.appelhans@gmail.com>
}
\references{
Empirical Orthogonal Teleconnections\cr
H. M. van den Dool, S. Saha, Å. Johansson (2000)\cr
Journal of Climate, Volume 13, Issue 8 (April 2000) pp. 1421-1435\cr

Empirical methods in short-term climate prediction\cr
H. M. van den Dool (2007)\cr
Oxford University Press, Oxford, New York (2007)\cr

}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
~~ documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
# recreate figure 6 from van den Dool et al. (2000)
library(raster)
library(rworldmap)
library(rgdal)
library(latticeExtra)
library(rgeos)

data(vdendool)
data(coastsCoarse)

modes <- eot(pred = vdendool, resp = NULL, n = 4,
             standardised = FALSE) 

ster <- CRS("+proj=stere +lat_0=90 +lon_0=-45")

xmin <- -180
xmax <- 180
ymin <- 20
ymax <- 90     # Coordinates for bounding box
bb <- cbind(x = c(xmin, xmin, xmax, xmax, xmin), 
            y = c(ymin, ymax, ymax, ymin, ymin))    #Create bounding box
SP <- SpatialPolygons(list(Polygons(list(Polygon(bb)), "1")), 
                      proj4string = CRS(proj4string(coastsCoarse)))

gI <- gIntersects(coastsCoarse, SP, byid = TRUE) 
out <- vector(mode = "list", length = length(which(gI))) 
ii <- 1

for (i in seq(along = gI)) if (gI[i]) {
  out[[ii]] <- gIntersection(coastsCoarse[i, ], SP)
  row.names(out[[ii]]) <- row.names(coastsCoarse)[i]
  ii <- ii + 1
}

nhem.coasts <- do.call("rbind", out)
nhem.coasts.ster <- spTransform(nhem.coasts, ster) 

lout <- list("sp.lines", nhem.coasts.ster, 
             col = "grey30", grid = TRUE)

clrs <- colorRampPalette(rev(brewer.pal(9, "RdBu")))

mode1 <- projectRaster(modes[[1]]$EOT_1$r.predictor, crs = ster)
mode2 <- projectRaster(modes[[1]]$EOT_2$r.predictor, crs = ster)
mode3 <- projectRaster(modes[[1]]$EOT_3$r.predictor, crs = ster)
mode4 <- projectRaster(modes[[1]]$EOT_4$r.predictor, crs = ster)

p1 <- spplot(mode1, sp.layout = lout, main = "Mode 1",
             col.regions = clrs(1000), at = seq(-1, 1, 0.02))
p2 <- spplot(mode2, sp.layout = lout, main = "Mode 2",
             col.regions = clrs(1000), at = seq(-1, 1, 0.02))
p3 <- spplot(mode3, sp.layout = lout, main = "Mode 3",
             col.regions = clrs(1000), at = seq(-1, 1, 0.02))
p4 <- spplot(mode4, sp.layout = lout, main = "Mode 4",
             col.regions = clrs(1000), at = seq(-1, 1, 0.02))

grid.arrange(p1, p2, p3, p4,
             heights = 1, ncol = 2)
}
