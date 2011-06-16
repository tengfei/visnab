######################################################################
##  Class "Painter"
######################################################################
VisnabPainter.gen <- setRefClass("VisnabPainter",
                                 fields = list(zoomLevel = "numeric"))
## accesors
## addLayer
## Should have the ability to
## 1. Transform automatically when the layer is added(limits)
##    compute limits itself, then compare limits to it's parent,
##    basically most time, it's object's rootLayer.
##    So the assumption here is that:
##    1) we transform them on the same scale
##    2) 
## 2. How to add events?
## assume it has extra column to store the color
