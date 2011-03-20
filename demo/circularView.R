require(visnab)
data(grv8)
grd <- print(grv8)
visenv$zoomobj <- grd
visZoom(grd)
setHover(grd,"sector")

