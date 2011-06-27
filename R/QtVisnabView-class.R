##----------------------------------------------------------##
##             For class "QtVisnabView"
##----------------------------------------------------------##
QtVisnabView.gen <- setRefClass("QtVisnabView",contains=c("VisnabView", "VIRTUAL"),
                                fields=c(
                                  scene = "QGraphicsScene",
                                  view = "Qanviz::PlotView",
                                  rootLayer = "Qanviz::RLayer",
                                  rescale = "RescaleEnum",
                                  signalingField("outputRange", "numeric"),
                                  signalingField("selfSignal", "logical")))

