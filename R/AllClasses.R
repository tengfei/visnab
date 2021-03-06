setClassUnion("BSgenomeORNULL",c("BSgenome","NULL"))

setOldClass("QIcon")
setOldClass("QWidget")
setOldClass("QDockWidget")
setOldClass("QStackedWidget")
setOldClass("QMainWindow")
setOldClass("mutalist")
setOldClass("QGraphicsScene")
setOldClass("QGraphicsView")
setOldClass("Qanviz::RLayer")
setOldClass("Qanviz::PlotView")
setOldClass("mutaframe")

setClassUnion("QWidgetORNULL",c("QWidget","NULL"))
setClassUnion("QMainWindowORNULL",c("QMainWindow","NULL"))
setClassUnion("QGraphicsSceneORNULL", c("QGraphicsScene","NULL"))
setClassUnion("Qanviz::RLayerORNULL", c("Qanviz::RLayer","NULL"))
setClassUnion("Qanviz::PlotViewORNULL", c("Qanviz::PlotView","NULL"))
setClassUnion("numericORcharacter", c("numeric", "character"))
setClassUnion("numericORNULL", c("numeric","NULL"))
setClassUnion("logicalORNULL", c("logical","NULL"))


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

