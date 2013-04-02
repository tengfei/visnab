##-----------------------------------------------------------------##
##                Class GeomSingleEnum
##-----------------------------------------------------------------##
##  lazily define Geoms here
## setEnum return class name not generator function
.IntervalViewGeom <- c("full", "reduce", "point", "length",
                       "barchart", "heatmap", "segment", "line",
                       "polygon", "mismatch", "histogram")
setSingleEnum("VisnabViewGeom", levels = "default")
setMultipleEnum("IntervalViewGeom", levels = .IntervalViewGeom)
setSingleEnum("TxdbViewGeom", levels = c("full", "dense", "slice"))
setSingleEnum("CoverageViewGeom", levels = c("total","mismatch","pairend","elength"))
setSingleEnum("BamViewGeom", levels = c("CoverageAndAlignment",
                               "Coverage","Alignment"))
setSingleEnum("AlignmentViewGeom", levels = c("oneside","twoside","pairend"))
setSingleEnum("ScaleViewGeom", levels = c("twoside"))
setSingleEnum("SingleChromViewGeom", levels = c("cytoband"))
setSingleEnum("SeqViewGeom", levels = c("default"))
setSingleEnum("TracksViewGeom", levels = c("default"))
setSingleEnum("CircularViewGeom", levels = c("default"))


## automatically default geom constructor
.Geom <- function(view = "VisnabView"){
  geom <- switch(view,
                 VisnabView = new("VisnabViewGeomSingleEnum"),
                 IntervalView = new("IntervalViewGeomMultipleEnum", "full"),
                 TxdbView = new("TxdbViewGeomSingleEnum", "full"),
                 CoverageView = new("CoverageViewGeomSingleEnum", "total"),
                 BamView = new("BamViewGeomSingleEnum", "CoverageAndAlignment"),
                 AlignmentView = new("AlignmentViewGeomSingleEnum", "oneside"),
                 ScaleView = new("ScaleViewGeomSingleEnum", "twoside"),
                 SingleChromView = new("SingleChromViewGeomSingleEnum", "cytoband"),
                 SeqView = new("SeqViewGeomSingleEnum", "default"),
                 TracksView = new("TracksViewGeomSingleEnum", "default"),
                 CircularView = new("CircularViewGeomSingleEnum", "default"))

}

.GeomName <- function(view = "VisnabView"){
  geom <- switch(view,
                 VisnabView = "VisnabViewGeomSingleEnum",
                 IntervalView = "IntervalViewGeomMultipleEnum",
                 TxdbView = "TxdbViewGeomSingleEnum",
                 CoverageView = "CoverageViewGeomSingleEnum",
                 BamView = "BamViewGeomSingleEnum", 
                 AlignmentView = "AlignmentViewGeomSingleEnum",
                 ScaleView = "ScaleViewGeomSingleEnum", 
                 SingleChromView = "SingleChromViewGeomSingleEnum",
                 SeqView = "SeqViewGeomSingleEnum",
                 TracksView = "TracksViewGeomSingleEnum",
                 CircularView = "CircularViewGeomSingleEnum") 
}





## setClass("GlyphEnum", contains = "VIRTUAL")
## setSingleEnum("PointSize", levels = c("1", "2", "5", "10"), contains = "GlyphEnum")

## setGeneric("icons", function(obj, ...) standardGeneric("icons"))

## setMethod("icons", "PointSizeSingleEnum", function(obj, pix.size = c(30, 30),
##                                                   pixbg.col = "white",
##                                                   point.fill = "black",
##                                                   point.stroke = NA){
##   lst <- lapply(levels(obj), function(i){
##     scene <- qscene()
##     layer <- qlayer(scene, function(layer, painter){
##       qdrawCircle(painter, 20, 10, r = as.numeric(i),
##                   fill = point.fill, stroke = point.stroke)
##     },limits = qrect(0, 0, 40, 20))
##     qpixmap <- Qt$QPixmap(pix.size[1], pix.size[2])
##     ## cols <- col2qcol("white", 0.1)
##     qpixmap$fill(Qt$QColor("white"))
##     pt <- Qt$QPainter(qpixmap)
##     scene$render(pt)
##     icon <- Qt$QIcon(qpixmap)
##   })
##   names(lst) <- levels(obj)
##   lst
## })

