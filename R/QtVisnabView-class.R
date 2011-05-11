## This virtual class should have slots which could hold qt object like
## scene,view,rootLayer
## Following defination doesn't work
setOldClass("QGraphicsScene")
setOldClass("QGraphicsView")
setOldClass("Qanviz::RLayer")
setOldClass("Qanviz::PlotView")

setClassUnion("QGraphicsSceneORNULL", c("QGraphicsScene","NULL"))
setClassUnion("Qanviz::RLayerORNULL", c("Qanviz::RLayer","NULL"))
setClassUnion("Qanviz::PlotViewORNULL", c("Qanviz::PlotView","NULL"))


setRefClass("QtVisnabView",contains=c("VisnabView","VIRTUAL"),
            fields=list(
                        scene = "QGraphicsSceneORNULL",
                        view = "Qanviz::PlotViewORNULL",
                        rootLayer = "Qanviz::RLayerORNULL",
                        ## thisLayer = "Qanviz::RLayerORNULL",
                        row = "integer",
                        col = "integer",
                        rowSpan = "integer",
                        colSpan = "integer")
              )

