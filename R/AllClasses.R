setClassUnion("MutableGRangesORGRanges",c("MutableGRanges","GRanges"))

setOldClass("QWidget")
setClassUnion("QWidgetORNULL",c("QWidget","NULL"))

setClassUnion("BSgenomeORNULL",c("BSgenome","NULL"))

setOldClass("QGraphicsScene")
setOldClass("QGraphicsView")
setOldClass("Qanviz::RLayer")
setOldClass("Qanviz::PlotView")

setClassUnion("QGraphicsSceneORNULL", c("QGraphicsScene","NULL"))
setClassUnion("Qanviz::RLayerORNULL", c("Qanviz::RLayer","NULL"))
setClassUnion("Qanviz::PlotViewORNULL", c("Qanviz::PlotView","NULL"))

setClassUnion("numericORNULL", c("numeric","NULL"))
setClassUnion("characterORNULL", c("character", "NULL"))
setClassUnion("vectorORNULL", c("vector","NULL"))
setClassUnion("functionORNULL", c("function","NULL"))

