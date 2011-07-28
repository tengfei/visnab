setClassUnion("AsIsORcharacter", c("AsIs","character"))

setOldClass("QIcon")

setOldClass("QWidget")
setOldClass("QDockWidget")
setOldClass("QStackedWidget")
setOldClass("QMainWindow")
setClassUnion("QWidgetORNULL",c("QWidget","NULL"))
setClassUnion("QMainWindowORNULL",c("QMainWindow","NULL"))





## qsetRefClass(Qt$QColor)
## setOldClass("QColor")

## setMethod("values", "QColor", function(x, ...){
##   x$names()
## })
## ## suppose values only accepted characters
## setReplaceMethod("values", "QColor", function(x, value){
##   if(is(value, "QColor"))
##     x <- value
##   if(is.character(value)){
##     x <- col2qcol(value)
##   }else{
##     stop("Values need to be a character or QColor object.")
##   }
##   x
## })



setClassUnion("BSgenomeORNULL",c("BSgenome","NULL"))

setOldClass("mutalist")
setOldClass("QGraphicsScene")
setOldClass("QGraphicsView")
setOldClass("Qanviz::RLayer")
setOldClass("Qanviz::PlotView")

setClassUnion("QGraphicsSceneORNULL", c("QGraphicsScene","NULL"))
setClassUnion("Qanviz::RLayerORNULL", c("Qanviz::RLayer","NULL"))
setClassUnion("Qanviz::PlotViewORNULL", c("Qanviz::PlotView","NULL"))

setClassUnion("numericORcharacter", c("numeric", "character"))
setClassUnion("numericORNULL", c("numeric","NULL"))
setClassUnion("characterORNULL", c("character", "NULL"))
setClassUnion("functionORNULL", c("function","NULL"))
setClassUnion("logicalORNULL", c("logical","NULL"))

setClass("Color", contains = "character")

setOldClass("mutaframe")
setAs("MutableGRanges", "mutaframe", function(from) {
  makeValueBindingFun <- function(cn) {
    function(val) {
      if (missing(val))
        values(from)[[cn]]
      else values(from)[[cn]] <- val
    }
  }
  cn <- colnames(values(from))
  closures <- sapply(, makeValueBindingFun)
  makeBindingFun <- function(accessor) {
    function(val) {
      if (missing(val))
        accessor(from)
      else accessor(from) <- val
    }
  }
  closures <- c(seqnames = makeBindingFun(seqnames),
                start = makeBindingFun(start), end = makeBindingFun(end),
                width = makeBindingFun(width), strand = makeBindingFun(strand),
                closures)
  mf <- do.call(mutaframe, closures)
  from$elementMetadataChanged$connect(function(i, j) {
    if (!is.null(j)) {
      j <- j + 5 # take into account the location columns
      notify_listeners(mf, i, j)
    } else { ## Assumption: columns can be added/removed but not rows
      newcn <- colnames(values(from))
      newCols <- setdiff(newcn, cn)
      for (newCol in newCols)
        mf[[newCol]] <- values(from)[[newCol]]
      remCols <- setdiff(cn, newcn)
      for (remCol in remCols)
        mf[[remCol]] <- NULL
      cn <<- newcn
    }
  })
  from$locationsChanged$connect(function(i) notify_listeners(mf, i, 1:5))
  mf
})

