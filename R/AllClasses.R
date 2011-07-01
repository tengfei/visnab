setClassUnion("AsIsORcharacter", c("AsIs","character"))

setOldClass("QWidget")
setOldClass("QMainWindow")
setClassUnion("QWidgetORNULL",c("QWidget","NULL"))
setClassUnion("QMainWindowORNULL",c("QMainWindow","NULL"))





## qsetRefClass(Qt$QColor)
setOldClass("QColor")
setMethod("values", "QColor", function(x, ...){
  x$names()
})
## suppose values only accepted characters
setReplaceMethod("values", "QColor", function(x, value){
  if(is(value, "QColor"))
    x <- value
  if(is.character(value)){
    x <- col2qcol(value)
  }else{
    stop("Values need to be a character or QColor object.")
  }
  x
})

## setClass("CColor", contains = c("character"))
## setReplaceMethod("values", "CColor", function(x, value){
##   if(!is.character(value))
##     stop("Values need to be a character")
##   x@.Data <- value
##   x
## })
## setMethod("values", "CColor", function(x, ...){
##   x$names()
## })

## setClassUnion("Color", c("QColor", "CColor"))

setClassUnion("BSgenomeORNULL",c("BSgenome","NULL"))

setOldClass("mutalist")
setOldClass("QGraphicsScene")
setOldClass("QGraphicsView")
setOldClass("Qanviz::RLayer")
setOldClass("Qanviz::PlotView")

setClassUnion("QGraphicsSceneORNULL", c("QGraphicsScene","NULL"))
setClassUnion("Qanviz::RLayerORNULL", c("Qanviz::RLayer","NULL"))
setClassUnion("Qanviz::PlotViewORNULL", c("Qanviz::PlotView","NULL"))


setClassUnion("numericORNULL", c("numeric","NULL"))
setClassUnion("characterORNULL", c("character", "NULL"))
setClassUnion("functionORNULL", c("function","NULL"))
setClassUnion("logicalORNULL", c("logical","NULL"))
setClassUnion("SingleEnumORMultipleEnum", c("SingleEnum","MultipleEnum"))
