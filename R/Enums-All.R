##-----------------------------------------------------------------##
##                Class GeomEnum
##-----------------------------------------------------------------##
##  lazily define Geoms here
## setEnum return class name not generator function
setEnum("IntervalViewGeom", levels = c("full", "dense"))
setEnum("TxdbViewGeom", levels = c("full", "dense"))
setEnum("CoverageViewGeom", levels = c("full", "dense"))
setEnum("AlignmentViewGeom", levels = c("full", "dense"))
setEnum("ScaleViewGeom", levels = c("full", "dense"))
setEnum("SingleChromViewGeom", levels = c("full", "dense"))
## setEnum("CircularViewGeoms", levels = c("full", "dense"), contains = "Geoms")

## sapply(names(getOption("BioC")$visnab), function(nm){
##   setEnum(paste(nm, "Geom", sep = ""),
##           levels = getOption("BioC")$visnab[[nm]]$geom)
## })


##-----------------------------------------------------------------##
##                Class CPalEnum
##-----------------------------------------------------------------##
CPalEnum <- setEnum("CPal", levels = c("area", "gradient", "grey",
                              "identity", "manual", "rescales"))
##-----------------------------------------------------------------##
##                Class DPalEnum
##-----------------------------------------------------------------##
DPalEnum <- setEnum("DPal", levels = c("brewer", "dichromat", "hue",
                              "identity", "manual"))

##-----------------------------------------------------------------##
##                Class RescaleEnum
##-----------------------------------------------------------------##
RescaleEnum <- setEnum("Rescale", levels = c("geometry", "transform", "none"))