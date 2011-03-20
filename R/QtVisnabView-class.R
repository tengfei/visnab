## This virtual class should have slots which could hold qt object like
## scene,view,rootLayer
## Following defination doesn't work
setOldClass("QGraphicsScene")
setOldClass("QGraphicsView")
setOldClass("Qanviz::RLayer")
setClassUnion("QGraphicsSceneOrNULL",c("QGraphicsScene",NULL))
setClassUnion("QGraphicsViewOrNULL",c("QGraphicsView",NULL))
setClassUnion("Qanviz::RLayerOrNULL",c("Qanviz::RLayer",NULL))

setClass("QtVisnabView",
         representation("VIRTUAL",
                        scene="QGraphicsSceneOrNULL",
                        view="QGraphicsViewOrNULL",
                        rootLayer="Qanviz::RLayerOrNULL"),
         contains="VisnabView")

