##----------------------------------------------------------##
##             For class "QtVisnabView"
##----------------------------------------------------------##
QtVisnabView.gen <- setRefClass("QtVisnabView",contains=c("VisnabView", "VIRTUAL"),
                                fields=list(
                                  scene = "QGraphicsSceneORNULL",
                                  view = "Qanviz::PlotViewORNULL",
                                  rootLayer = "Qanviz::RLayerORNULL",
                                  rescale = "RescaleEnum"))

## general utils used in createView function
QtVisnabView.gen$methods(
                         setDislayWidgets = function(dragMode = TRUE){
                           if(is.null(scene)){
                             scene <<- qscene()
                             view <<- qplotView(scene,rescale = rescale)
                             if(dragMode)
                               view$setDragMode(Qt$QGraphicsView$ScrollHandDrag)
                           }
                           if(is.null(rootLayer))
                             rootLayer <<- qlayer(scene,
                                                     geometry=qrect(0,0,800,600))
                         },
                         setBgColor = function(bgcol = NULL){
                           if(is.null(bgcol))
                             bgcol <- pars$bgColor
                           bgalpha <- pars$alpha
                           qcol <- col2qcol(bgcol,bgalpha)
                           scene$setBackgroundBrush(qbrush(qcol))
                         }
                         )


