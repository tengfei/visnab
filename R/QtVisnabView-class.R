##----------------------------------------------------------##
##             For class "QtVisnabView"
##----------------------------------------------------------##
QtVisnabView.gen <- setRefClass("QtVisnabView",contains=c("VisnabView", "VIRTUAL"),
                                fields=c(
                                  scene = "QGraphicsSceneORNULL",
                                  view = "Qanviz::PlotViewORNULL",
                                  rootLayer = "Qanviz::RLayerORNULL",
                                  thisLayer = "Qanviz::RLayerORNULL",
                                  row = "integer",
                                  col = "integer",
                                  rowSpan = "integer",
                                  colSpan = "integer",
                                  signalingField("focusin","logicalORNULL")))

