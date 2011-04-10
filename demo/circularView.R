require(visnab)
data(grv8)
grv8@tracks
grd <- print(grv8)
visenv$zoomobj <- grd
visZoom(grd)
setHover(grd,"sector")

