setClassUnion("AsIsORcharacter", c("AsIs","character"))

setOldClass("QWidget")
setOldClass("QMainWindow")
setClassUnion("QWidgetORNULL",c("QWidget","NULL"))
setClassUnion("QMainWindowORNULL",c("QMainWindow","NULL"))

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
