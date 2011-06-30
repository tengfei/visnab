##-----------------------------------------------------------------##
##                Class GeomEnum
##-----------------------------------------------------------------##
##  lazily define Geoms here
## setEnum return class name not generator function
setEnum("VisnabViewGeom", levels = character())
setEnum("IntervalViewGeom", levels = c("full", "dense"))
setEnum("TxdbViewGeom", levels = c("full", "dense", "slice"))
setEnum("CoverageViewGeom", levels = c("total","mismatch","pairend","elength"))
setEnum("AlignmentViewGeom", levels = c("full", "dense"))
setEnum("ScaleViewGeom", levels = c("twoside"))
setEnum("SingleChromViewGeom", levels = c("full", "dense"))
setEnum("SeqViewGeom", levels = c("default"))

## automatica constructor
.Geoms <- function(view = "VisnabView"){
  geom <- switch(view,
                 VisnabView = new("VisnabViewGeomEnum"),
                 IntervalView = new("IntervalViewGeomEnum", "full"),
                 TxdbView = new("TxdbViewGeomEnum", "full"),
                 CoverageView = new("CoverageViewGeomEnum", "total"),
                 AlignmentView = new("AlignmentViewGeomEnum", "full"),
                 ScaleViewGeom = new("ScaleViewGeomEnum", "twoside"),
                 SingleChromView = new("SingleChromViewGeomEnum", "full"),
                 SeqViewGeom = new("SeqViewGeomEnum", "default")
                 ) 
}

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


##-----------------------------------------------------------------##
##                Class ImodeEnum
##-----------------------------------------------------------------##
## TODO


##-----------------------------------------------------------------##
##                Class ThemeEnum
##-----------------------------------------------------------------##
## TODO




