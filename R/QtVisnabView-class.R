##----------------------------------------------------------##
##             For class "QtVisnabView"
##----------------------------------------------------------##
setRefClass("QtVisnabView",contains=c("VisnabView", "VIRTUAL"),
            fields=list(
              scene = "QGraphicsSceneORNULL",
              view = "Qanviz::PlotViewORNULL",
              rootLayer = "Qanviz::RLayerORNULL",
              gridLayer = "Qanviz::RLayerORNULL",
              tooltipLayer = "Qanviz::RLayerORNULL",
              rescale = "RescaleSingleEnum"),
            methods = list(
              setDragMode = function(value = c("NoDrag",
                                       "ScrollHandDrag",
                                       "RubberBandDrag")){
                value <- match.arg(value)
                mode$items$scaleMode$pars$dragMode <<- value
                vals <- getQtEnum(mode$items$scaleMode$pars$dragMode)
                view$setDragMode(vals)
              },
              
              setDislayWidgets = function(dragMode = TRUE){
                if(is.null(scene)){
                  scene <<- qscene()
                  view <<- qplotView(scene,rescale = rescale)
                  vals <- getQtEnum(mode$items$scaleMode$pars$dragMode)
                  view$setDragMode(vals)
                }
                if(is.null(rootLayer))
                  rootLayer <<- qlayer(scene,
                                       geometry=qrect(0,0,800,600),
                                       cache = FALSE)
              },
              setBgColor = function(bgcol = NULL){
                if(is.null(bgcol))
                  bgcol <- pars$bgColor
                bgalpha <- pars$alpha
                qcol <- col2qcol(bgcol,bgalpha)
                scene$setBackgroundBrush(qbrush(qcol))
              },
              ## painter level
              ## addLayer = function(layer, parent, i = 0L, j = 0L,...){
              ##   if(missing(parent))
              ##     parent <- rootLayer
              ##   ## qlayer(parent, ...)
              ##   parent[i, j] <<- layer
              ## },
              setTitle = function(text){
                setLabel(text, side = 3)
              },
              closeTitle = function(){
                closeLabel(side = 3)
              },
              setXLab = function(text){
                setLabel(text, side = 1)
              },
              closeXLab = function(){
                closeLabel(side = 1)
              },
              setYLab = function(text){
                setLabel(text, side = 2)
              },
              closeYLab = function(){
                closeLabel(side = 2)
              },
              setMargin = function(mar = c(10, 10, 20, 20)){
                stopifnot(length(mar) == 4)
                sapply(seq_along(mar), function(s){
                  switch(s,{
                    i <- 6
                    j <- 2
                  },{
                    i <- 3
                    j <- 0
                  },{
                    i <- 0
                    j <- 2
                  },{
                    i <- 3
                    j <- 5
                  })
                  arr <- function(s){
                    layout <- rootLayer$gridLayout()
                    if(s %in% c(1, 3)){
                      layout$setRowPreferredHeight(i, mar[s])
                      layout$setRowStretchFactor(i, 0)
                    }
                    if(s %in% c(2, 4)){
                      layout$setColumnPreferredWidth(j, mar[s])
                      layout$setColumnStretchFactor(j, 0)

                    }
                  }
                  arr(s)
                })
              },
              setLabel = function(text, side = c(1, 2, 3, 4), ...){
                if(missing(text))
                  stop("Please specify the label")
                if(!all(side %in% c(1, 2, 3, 4)))
                  stop("side must be one or more of 1, 2, 3, 4")
                if(length(text) != length(side))
                  stop("length of text must be of the same length of side")
                ## suppose mainLayer is always's in
                sapply(seq_along(side), function(k){
                  s <- side[k]
                switch(s,{
                  i <- 5
                  j <- 2
                  halign <- "center"
                  valign <- "top"
                  rot <- 0
                  data <- pars$xlim
                },{
                  i <- 3
                  j <- 0
                  halign <- "right"
                  valign <- "center"
                  rot <- 90
                  data <- pars$ylim
                },{
                  i <- 1
                  j <- 2
                  halign <- "center"
                  valign <- "bottom"
                  rot <- 0
                  data <- pars$xlim
                },{
                  i <- 3
                  j <- 4
                  halign <- "left"
                  valign <- "center"
                  rot <- 270
                  data <- pars$ylim
                })
                arr <- function(s, text){
                rootLayer[i, j] <<- qlayer(scene,
                                           function(layer, painter){
                           qdrawText(painter, text, 5, 5, halign, valign,
                            rot = rot, color = pars$textColor)
                                           },
                                           limits = qrect(0, 0, 10, 10))
                layout <- rootLayer$gridLayout()
                if(s %in% c(1, 3)){
                  layout$setRowPreferredHeight(i, 30)
                  layout$setRowStretchFactor(i, 0)
                }
                if(s %in% c(2, 4)){
                  layout$setColumnPreferredWidth(j, 30)
                  layout$setColumnStretchFactor(j, 0)
                }}
                arr(s, text[k])
              })
              },

              showLabel = function(side = c(1, 2, 3, 4)){
                if(!all(side %in% c(1, 2, 3, 4)))
                  stop("side must be one or more of 1, 2, 3, 4")
                switch(side,{
                  i <- 5
                  j <- 2
                },{
                  i <- 3
                  j <- 0
                },{
                  i <- 1
                  j <- 2
                },{
                  i <- 3
                  j <- 4
                })
                rootLayer[i, j]$show()
              },

              hideLabel = function(side = c(1, 2, 3, 4)){
                if(!all(side %in% c(1, 2, 3, 4)))
                  stop("side must be one or more of 1, 2, 3, 4")
                for(s in side){
                switch(s,{
                  i <- 5
                  j <- 2
                },{
                  i <- 3
                  j <- 0
                },{
                  i <- 1
                  j <- 2
                },{
                  i <- 3
                  j <- 4
                })
                rootLayer[i, j]$hide()
              }
              },

              closeLabel = function(side = c(1, 2, 3, 4)){
                if(!all(side %in% c(1, 2, 3, 4)))
                  stop("side must be one or more of 1, 2, 3, 4")
                switch(side,{
                  i <- 5
                  j <- 2
                },{
                  i <- 3
                  j <- 0
                },{
                  i <- 1
                  j <- 2
                },{
                  i <- 3
                  j <- 4
                })
                rootLayer[i, j]$close()
                layout <- rootLayer$gridLayout()
                if(side %in% c(1, 3)){
                  layout$setRowPreferredHeight(i, 0)
                }
                if(side %in% c(2, 4)){
                  layout$setColumnPreferredWidth(j, 0)
                }
              },

              drawAxis = function(side = c(1, 2, 3, 4), ...){
                if(!all(side %in% c(1, 2, 3, 4)))
                  stop("side must be one or more of 1, 2, 3, 4")
                ## suppose mainLayer is always's in 
                for(s in side){
                switch(s,{
                  i <- 4
                  j <- 2
                },{
                  i <- 3
                  j <- 1
                },{
                  i <- 2
                  j <- 2
                },{
                  i <- 3
                  j <- 3
                })
                ## a little hack to put it into closure
                arr <- function(s){
                  rootLayer[i, j] <<- layer <- qlayer(scene,
                                                      axisPainter(s))
                  layout <- rootLayer$gridLayout()
                  if(s %in% c(1, 3)){
                    layout$setRowPreferredHeight(i, 30)
                    layout$setRowStretchFactor(i, 0)
                    layer$setLimits(qrect(pars$xlim, c(-1,1)))
                    pars$xlimChanged$connect(function(){
                      layer$setLimits(qrect(pars$xlim, c(-1, 1)))
                    })
                  }
                  if(s %in% c(2, 4)){
                    layout$setColumnPreferredWidth(j, 30)
                    layout$setColumnStretchFactor(j, 0)
                    layer$setLimits(qrect(c(-1, 1), pars$ylim))
                    pars$ylimChanged$connect(function(){
                      layer$setLimits(qrect(c(-1, 1), pars$ylim))
                    })
                  }
                }
                arr(s)
              }
              },
              
              showAxis = function(side = c(1, 2, 3, 4)){
                if(!all(side %in% c(1, 2, 3, 4)))
                  stop("side must be one or more of 1, 2, 3, 4")
                for(s in side){
                switch(s,{
                  i <- 4
                  j <- 2
                },{
                  i <- 3
                  j <- 1
                },{
                  i <- 2
                  j <- 2
                },{
                  i <- 3
                  j <- 3
                })
                rootLayer[i, j]$show()
              }
              },

              hideAxis = function(side = c(1, 2, 3, 4)){
                if(!all(side %in% c(1, 2, 3, 4)))
                  stop("side must be one or more of 1, 2, 3, 4")
                for(s in side){
                switch(s,{
                  i <- 4
                  j <- 2
                },{
                  i <- 3
                  j <- 1
                },{
                  i <- 2
                  j <- 2
                },{
                  i <- 3
                  j <- 3
                })
                rootLayer[i, j]$hide()
              }
              },

              closeAxis = function(side = c(1, 2, 3, 4)){
                if(!all(side %in% c(1, 2, 3, 4)))
                  stop("side must be one or more of 1, 2, 3, 4")
                for(s in side){
                switch(s,{
                  i <- 4
                  j <- 2
                },{
                  i <- 3
                  j <- 1
                },{
                  i <- 2
                  j <- 2
                },{
                  i <- 3
                  j <- 3
                })
                arr <- function(s){
                rootLayer[i, j]$close()
                layout <- rootLayer$gridLayout()
                if(s %in% c(1, 3)){
                  layout$setRowPreferredHeight(i, 0)
                }
                if(s %in% c(2, 4)){
                  layout$setColumnPreferredWidth(j, 0)
                }
              }
                arr(s)
              }
              },

              drawGrid = function(color) {
                if(!missing(color))
                  pars$gridBgColor <<- color
                rootLayer[3, 2] <<- gridLayer <<- qlayer(scene, paintFun = gridPainter())
                pars$xlimChanged$connect(function(){
                  gridLayer$setLimits(qrect(pars$xlim[1],
                                        pars$ylim[1],
                                        pars$xlim[2],
                                        pars$ylim[2]))
                })
                pars$ylimChanged$connect(function(){
                  gridLayer$setLimits(qrect(pars$xlim[1],
                                        pars$ylim[1],
                                        pars$xlim[2],
                                        pars$ylim[2]))
                })
              },
              showGrid = function(){
                gridLayer$show()
              },
              hideGrid = function(){
                gridLayer$hide()
              },
              ## abline
              abline = function(a, b, h, v, stroke, width = 2, parent){
                if(!missing(a))
                  stop("not implemented 'a' mode")
                if(!missing(b))
                  stop("not implemented 'b' mode")
                if(missing(parent))
                  parent <- rootLayer[3, 2]
                if(missing(stroke))
                    stroke <- pars$stroke

                arr <- function(fun){
                  rootLayer[3, 2] <<- layer <- qlayer(scene, fun)
                  fixLimits(layer)
                  syncLimits(layer)
                }
                if(!missing(h)){
                  fun <- function(layer, painter){
                    qlineWidth(painter) <- width
                    qdrawSegment(painter, pars$xlim[1], h,
                                 pars$xlim[2], h, stroke = stroke)
                  }
                  arr(fun)
                }
                if(!missing(v)){
                  fun <- function(layer, painter){
                    qlineWidth(painter) <- width
                    qdrawSegment(painter, v, pars$ylim[1],
                                 v, pars$ylim[2], stroke = stroke)
                  }
                  arr(fun)
                }
              },
              ## draw tooltip
              ## setIdentifyMode = function(){
                
              ## },
              ## setBrushMode = function(){
                
              ## },
              ## setScaleMode = function(){
                
              ## },
              fixLimits = function(layer){
                layer$setLimits(qrect(pars$xlim[1],
                                          pars$ylim[1],
                                          pars$xlim[2],
                                          pars$ylim[2]))
                  },
              syncLimits = function(layer){
                'signal level
                '
                pars$xlimChanged$connect(fixLimits(layer))
                pars$ylimChanged$connect(fixLimits(layer))
              },
              drawTooltip = function(text){
                layer <- qlayer(scene, tooltipPainter(text))
                fixLimits(layer)
                rootLayer[3, 2] <<- tooltipLayer <<- layer
                syncLimits(tooltipLayer)
              },
              showTooltip = function(){
                tooltipLayer$show()
              },
              hideTooltip = function(){
                tooltipLayer$hide()
              },
              closeTooltip = function(){
                tooltipLayer$close()
              },
              update = function(){
                qupdate(scene)
              },
              
              show = function(){
                view$show()
              },
              
              close = function(){
                view$close()
              },
              
              hide = function(){
                view$hide()
              },
              
              setWindowTitle = function(title){
                view$setWindowTitle(title)
              },
              
              ## ----------------------------------------
              ##   GUI level
              ## ----------------------------------------
                            GUI = function(show = TRUE){
                ## for temproary
                ## FIXME: need to check the genome
                if(TRUE)
                  data(genesymbol)
                sv <- SimpleViewer(view = view, gr = genesymbol)
                qconnect(sv, "rangeChanged", function(){
                  vgr <- sv$getSearchRange()
                  if(length(vgr))
                    range(.self) <- vgr
                })
                if(show)
                  sv$show()
                else
                  sv$hide()
              },
              regSignal = function(){
                pars$ThemeChanged$connect(function(){
                  qupdate(scene)
                })
              },

              showMessage = function(){
                
              },

              clearMessage = function(){
                
              },
              setValue = function(){
                
              },
              cp = function(...){
                pars$cp(...)
              },
              ## ----------------------------------------
              ##   painter
              ## ----------------------------------------
              axisPainter = function(side, breaks = pretty_breaks(),
                labels = scientific_format()){
                xalign <- yalign <- "center"
                xshift1 <- yshift1 <- xshift2 <- yshift2 <- 0
                function(layer, painter){
                  if(side %in% c(1, 3)){
                    data <- pars$xlim
                  }
                  if(side %in% c(2, 4)){
                    data <- pars$ylim
                  }
                  cbs <- cbreaks(range(data), breaks = breaks, labels = labels)
                  xat <- yat <- cbs$breaks
                  switch(side, {
                    yat <- 0.9
                    yalign <- "top"
                    yshift1 <- 0.01
                    yshift2 <- 0.1
                  }, {
                    xat <- 0.9
                    xalign <- "right"
                    xshift1 <- 0.01
                    xshift2 <- 0.1
                  }, {
                    yat <- 0.1
                    yalign <- "bottom"
                    yshift1 <- -0.01
                    yshift2 <- -0.1
                  }, {
                    xat <- 0.1
                    xalign <- "left"
                    xshift1 <- -0.01
                    xshift2 <- -0.1
                  })
                  qdrawText(painter, cbs$labels, x = xat,
                            y = yat, halign = xalign, valign = yalign)
                  qdrawSegment(painter,  xat+ xshift1,
                               yat + yshift1, xat + xshift2, yat + yshift2)
                }
              },
              gridPainter = function(breaks = pretty_breaks(),
                labels = scientific_format()){
                function(layer, painter) {
                  xat <- cbreaks(range(pars$xlim),
                                 breaks = breaks, labels = labels)$breaks
                  yat <- cbreaks(range(pars$ylim),
                                 breaks = breaks, labels = labels)$breaks
                  qdrawRect(painter, pars$xlim[1], pars$ylim[1],
                            pars$xlim[2], pars$ylim[2], stroke = "gray",
                            fill = "gray")
                  qlineWidth(painter) <- 2
                  qdrawSegment(painter, xat, pars$ylim[1], xat,
                               pars$ylim[2], stroke = "white")
                  qdrawSegment(painter, pars$xlim[1], yat,
                               pars$xlim[2], yat, stroke = "white")
                  qlineWidth(painter) <- 1
                }},
              ## for general painter
              tooltipPainter = function(text){
                'Generate tooltip painter function
                '
                function(layer, painter){
                  ## mode
                  md <- mode$idenitfyMode$tooltipMode
                  pos <- switch(mode$identifyMode$tooltipPos,
                                TopLeft = c(0, 0),
                                TopRight = NULL , #TODO
                                BottomLeft = NULL, #TODO
                                BottomRight = NULL, #TODO
                                Float = eventTrace$hoverPos)
                  ## info
                  text <- switch(md,
                                 Off = chracter(),
                                 Identify = hoverId,
                                 Metainfo =
                                 getTooltipInfo(mr,eventTrace$hoverId),
                                 Text = text
                                 )
                  ## draw background
                  bgwidth <- qstrWidth(painter, text)
                  bgheight <- qstrHeight(painter, text)
                  qdrawRect(painter, pos[1], pos[2],
                            pos[1]+bgwidth, pos[2]+bgheight,
                            fill = "yellow", stroke = "yellow")
                  qdrawText(painter, text, pos[1], pos[2], "left", "top")
                }
              },
              ## ## ----------------------------------------
              ## ##  Event-generator method
              ## ## ----------------------------------------
              keyPressEventZoom = function(sx = 1.5, sy = 1.5,
                browser = TRUE){
                function(layer, event){
                  zoommode <- mode$items$scaleMode$pars$zoomMode
                  if(zoommode == "Vertical")
                    sx <- 1
                  if(zoommode == "Horizontal")
                    sy <- 1
                  if(zoommode == "Off")
                    sx <- sy <- 1
                  eventTrace$focusin <<- TRUE
                  if(event$modifiers() == Qt$Qt$ControlModifier){
                    if(event$key() == Qt$Qt$Key_Equal)
                      view$scale(sx, sy)
                    if(event$key() == Qt$Qt$Key_Minus)
                      view$scale(1/sx, 1/sy)
                    if(event$key() == Qt$Qt$Key_0)
                      view$resetTransform()
                  }
                }
              },
              wheelEventZoom = function(sx = 1.5, sy = 1.5){
                function(layer, event){
                  zoommode <- mode$items$scaleMode$pars$zoomMode
                  if(zoommode == "Vertical")
                    sx <- 1
                  if(zoommode == "Horizontal")
                    sy <- 1
                  if(zoommode == "Off")
                    sx <- sy <- 1
                  pos.s <- as.numeric(event$scenePos())
                  pos <- as.numeric(event$pos())
                  if (event$delta() < 0)
                    sx <- 1/sx
                  mid <- c(mean(pars$xlimZoom),mean(pars$ylim))
                  mid.cur <- pos[1]- (pos[1]-mid[1])*(1/sx)
                  centerOn <- as.numeric(rootLayer[3, 2]$mapToScene(mid.cur, mid[2]))
                  view$scale(sx, sy)
                  ## mid.s.old <- rootLayer[3, 2]$mapToScene(mid[1], mid[2])
                  ## mid.s.cur <- pos.s-(pos.s-mid.s.old)*sx
                  view$centerOn(centerOn[1], centerOn[2])
                  }
                },

              ## hoverMoveEvent = function(obj, mr){
              ##   function(layer,event){
              ##     rect <- qrect(0,0,1,1)
              ##     mat <- layer$deviceTransform(event)$inverted()
              ##     rect <- mat$mapRect(rect)
              ##     pos <- event$pos()
              ##     rect$moveCenter(pos)
              ##     hits <- layer$locate(rect)+1
              ##     if(length(hits)>=1){
              ##       posS <- event$screenPos()
              ##       hits <- hits[1]
              ##       values(obj$track)$.color[hits] <<- obj$pars$hoverColor
              ##       Qt$QToolTip$showText(posS,getTooltipInfo(mr,hits))
              ##       obj$flag <<- TRUE
              ##     }else{
              ##       if(obj$flag){
              ##         values(obj$track)$.color <<- obj$pars$fill
              ##         obj$flag <<- FALSE
              ##       }
              ##     }
              ##   }
              ## }

              save = function(file){
                if(FALSE){
                  library(qtbase)
                  library(qtpaint)
                  scene <<- qscene()
                  layer <- qlayer(scene, function(layer, painter){
                    qdrawCircle(painter, 5, 5, r = 5, fill = "black")
                  }, limits = qrect(0, 0, 10, 10))
                  view <<- qplotView(scene)
                  view$show()
                  view$setWindowTitle("tengfei")

                  qpixmap <- Qt$QPixmap(400, 300)
                  pt <- Qt$QPainter(qpixmap)
                  scene$render(pt)
                  qpixmap$save("~/Desktop/point.jpeg")
                  img <- qpixmap$toImage()
                  img$save("~/Desktop/point.gif")
                }
              }

              ))


setMethod("print","QtVisnabView",function(x){
  x$show()
})
