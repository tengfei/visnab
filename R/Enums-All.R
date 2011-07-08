##-----------------------------------------------------------------##
##                Class GeomSingleEnum
##-----------------------------------------------------------------##
##  lazily define Geoms here
## setEnum return class name not generator function
.IntervalViewGeom <- c("full", "reduce", "point", "length",
                       "barchart", "heatmap", "segment")
setSingleEnum("VisnabViewGeom", levels = character())
setSingleEnum("IntervalViewGeom", levels = .IntervalViewGeom)
setSingleEnum("TxdbViewGeom", levels = c("full", "dense", "slice"))
setSingleEnum("CoverageViewGeom", levels = c("total","mismatch","pairend","elength"))
setSingleEnum("AlignmentViewGeom", levels = c("full", "dense"))
setSingleEnum("ScaleViewGeom", levels = c("twoside"))
setSingleEnum("SingleChromViewGeom", levels = c("full", "dense"))
setSingleEnum("SeqViewGeom", levels = c("default"))

## automatica constructor
.Geom <- function(view = "VisnabView"){
  geom <- switch(view,
                 VisnabView = new("VisnabViewGeomSingleEnum"),
                 IntervalView = new("IntervalViewGeomSingleEnum", "full"),
                 TxdbView = new("TxdbViewGeomSingleEnum", "full"),
                 CoverageView = new("CoverageViewGeomSingleEnum", "total"),
                 AlignmentView = new("AlignmentViewGeomSingleEnum", "full"),
                 ScaleViewGeom = new("ScaleViewGeomSingleEnum", "twoside"),
                 SingleChromView = new("SingleChromViewGeomSingleEnum", "full"),
                 SeqViewGeom = new("SeqViewGeomSingleEnum", "default"))

}

.GeomName <- function(view = "VisnabView"){
  geom <- switch(view,
                 VisnabView = "VisnabViewGeomSingleEnum",
                 IntervalView = "IntervalViewGeomSingleEnum",
                 TxdbView = "TxdbViewGeomSingleEnum",
                 CoverageView = "CoverageViewGeomSingleEnum", 
                 AlignmentView = "AlignmentViewGeomSingleEnum",
                 ScaleView = "ScaleViewGeomSingleEnum", 
                 SingleChromView = "SingleChromViewGeomSingleEnum",
                 SeqView = "SeqViewGeomSingleEnum") 
}

## setEnum("CircularViewGeoms", levels = c("full", "dense"), contains = "Geoms")

## sapply(names(getOption("BioC")$visnab), function(nm){
##   setEnum(paste(nm, "Geom", sep = ""),
##           levels = getOption("BioC")$visnab[[nm]]$geom)
## })


##-----------------------------------------------------------------##
##                Class CPalEnum
##-----------------------------------------------------------------##
CPalSingleEnum <- setSingleEnum("CPal", levels = c("area", "gradient", "grey",
                              "identity", "manual", "rescales"))
##-----------------------------------------------------------------##
##                Class DPalEnum
##-----------------------------------------------------------------##
DPalSingleEnum <- setSingleEnum("DPal", levels = c("brewer", "dichromat", "hue",
                              "identity", "manual"))

##-----------------------------------------------------------------##
##                Class RescaleSingleEnum
##-----------------------------------------------------------------##
RescaleSingleEnum <- setSingleEnum("Rescale",
                                   levels = c("geometry", "transform", "none"))


##-----------------------------------------------------------------##
##                Class ImodeEnum
##-----------------------------------------------------------------##
## TODO


##-----------------------------------------------------------------##
##                Class ThemeEnum
##-----------------------------------------------------------------##
## TODO




