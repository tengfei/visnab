library(visnab)
data(grv8)
grd <- visplot(grv8)
visenv$zoomobj <- grd
visZoom(grd)
setHover(grd,"sector")

