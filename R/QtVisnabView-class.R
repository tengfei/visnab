##----------------------------------------------------------##
##             For class "QtVisnabView"
##----------------------------------------------------------##
setRefClass("QtVisnabView",contains=c("VisnabView", "VIRTUAL"),
            fields=list(
              scene = "QGraphicsSceneORNULL",
              view = "Qanviz::PlotViewORNULL",
              rootLayer = "Qanviz::RLayerORNULL",
              gridLayer = "Qanviz::RLayerORNULL",
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
              ##   rootLayer[i, j] <<- layer
              ## },
              

              setLabel = function(...){
                
              },


              drawAxis = function(side = c(1, 2, 3, 4), ...){
                if(!all(side %in% c(1, 2, 3, 4)))
                  stop("side must be one or more of 1, 2, 3, 4")
                ## suppose mainLayer is always's in 
                s <- side
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
                if(s %in% c(1, 3)){
                  data <- pars$xlim
                }
                if(s %in% c(2, 4)){
                  data <- pars$ylim
                }
                rootLayer[i, j] <<- qlayer(scene,
                                           axisPainter(s, data = data))
                layout <- rootLayer$gridLayout()
                if(s %in% c(1, 3)){
                  layout$setRowPreferredHeight(i, 30)
                  layout$setRowStretchFactor(i, 0)
                  rootLayer[i, j]$setLimits(qrect(pars$xlim, c(-1,1)))
                  pars$xlimChanged$connect(function(){
                    rootLayer[i, j]$setLimits(qrect(pars$xlim, c(-1, 1)))
                  })
                }
                if(s %in% c(2, 4)){
                  layout$setColumnPreferredWidth(j, 30)
                  layout$setColumnStretchFactor(j, 0)
                  rootLayer[i, j]$setLimits(qrect(c(-1, 1), pars$ylim))
                  pars$ylimChanged$connect(function(){
                    rootLayer[i, j]$setLimits(qrect(c(-1, 1), pars$ylim))
                  })
                }
              },
              
              showAxis = function(side = c(1, 2, 3, 4)){
                if(!all(side %in% c(1, 2, 3, 4)))
                  stop("side must be one or more of 1, 2, 3, 4")
                switch(side,{
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
              },

              hideAxis = function(side = c(1, 2, 3, 4)){
                if(!all(side %in% c(1, 2, 3, 4)))
                  stop("side must be one or more of 1, 2, 3, 4")
                switch(side,{
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
              },

              closeAxis = function(side = c(1, 2, 3, 4)){
                if(!all(side %in% c(1, 2, 3, 4)))
                  stop("side must be one or more of 1, 2, 3, 4")
                switch(side,{
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
                rootLayer[i, j]$close()
                layout <- rootLayer$gridLayout()
                if(side %in% c(1, 3)){
                  layout$setRowPreferredHeight(i, 0)
                }
                if(side %in% c(2, 4)){
                  layout$setColumnPreferredWidth(j, 0)
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
                
              },
              ## ----------------------------------------
              ##   GUI level
              ## ----------------------------------------
              showMessage = function(){
                
              },

              clearMessage = function(){
                
              },
              setValue = function(){
                
              },
              ## ----------------------------------------
              ##   painter
              ## ----------------------------------------
              axisPainter = function(side, data, breaks = pretty_breaks(),
                labels = scientific_format()){
                xalign <- yalign <- "center"
                xshift1 <- yshift1 <- xshift2 <- yshift2 <- 0
                cbs <- cbreaks(range(data), breaks = breaks, labels = labels)
                xat <- yat <- cbs$breaks
                function(layer, painter){
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
                  ## lims <- range(data)
                  ## qdrawSegment(painter, )
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
              ## tooltipPainter = function(text){
              ##   'Generate tooltip painter function
              ##   '
              ##   function(layer, painter){
              ##     ## mode
              ##     pos <- switch(mode$identifyMode$tooltipPos,
              ##                   TopLeft = c(0, 0),
              ##                   TopRight = NULL ,
              ##                   BottomLeft = NULL,
              ##                   BottomRight = NULL,
              ##                   Float = eventTrace$hoverPos)
              ##     ## draw background
              ##     bgwidth <- qstrWidth(painter, text)
              ##     bgheight <- qstrHeight(painter, text)
              ##     qdrawRect(painter, pos[1], pos[2],
              ##               pos[1]+bgwidth, pos[2]+bgheight,
              ##               fill = "yellow", stroke = "yellow")
              ##     qdrawText(painter, text, pos[1], pos[2], "left", "top")
              ##   }
              ## },
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
                  if(zoommode == "NoDrag")
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
              ## wheelEventZoom = function(sx = 2, sy = 1, mid, layer){
              ##   function(layer, event){
              ##     pos.s <- as.numeric(event$scenePos())
              ##     pos <- as.numeric(event$pos())
              ##     if (event$delta() < 0)
              ##       sx <- 1/sx
              ##     view$scale(sx, sy)
              ##     if(!missing(layer)){
              ##       centerOn <-
              ##         as.numeric(layer$mapToScene((pos[1]-
              ##                                      (pos[1]-mid[1])*(1/sx)), pos[2]))
              ##       view$centerOn(centerOn)
              ##     }
              ##   }
              ## },
              
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


##' s = qscene()
##' r = qlayer(s)
##' m = qlayer(paintFun = function(layer, painter) {
##'     qdrawCircle(painter, runif(1000), runif(1000), r = 2)
##'     qdrawRect(painter, 0, 0, 1, 1)
##' }, limits = qrect(matrix(c(0, 1, 0, 1), 2))) # main layer
##' g = qgrid(xat = seq(0, 1, .2), yat = seq(0, 1, .5), sister = m)
##' r[1, 1] = g  # must add the grid layer FIRST, then the plot layer
##' r[1, 1] = m
##' print(qplotView(scene = s))
##'

qmtext = function(parent = NULL, data = NULL, side = 1, text = '', x = NULL, y = NULL,
  cex = 1, sister = NULL, ...) {
  if (!is.null(sister)) {
    lims = as.matrix(sister$limits())
    at = colMeans(lims)
    x = at[1]; y = at[2]
    lims = qrect(if (side%%2) cbind(lims[, 1], 0:1) else cbind(0:1, lims[, 2]))
  }
  draw_text = function(layer, painter) {
    if (!is.null(data)) {
      at = colMeans(data$limits)
      x = at[1]; y = at[2]
    }
    if (side%%2) y <- 0.5 else x <- 0.5
    qdrawText(painter, text, x, y, rot = c(0, 90, 0, 90)[side], cex = cex)
  }
  if (!('limits' %in% names(list(...))) && !is.null(sister))
    qlayer(parent, paintFun = draw_text, limits = lims, ...) else
  qlayer(parent, paintFun = draw_text, ...)
}



## s = qscene()
## r = qlayer(s)
## r[1, 1] = qlayer(paintFun = function(layer, painter) {
## qdrawCircle(painter, runif(1000), runif(1000), r = 2)
## qdrawRect(painter, 0, 0, 1, 1)
## }, limits = qrect(matrix(c(0, 1, 0, 1), 2))) # main layer

## r[2, 1] = qaxis(side = 1, at = c(0, .1, .3, .7, .8), sister = r[1, 1]) # x-axis
## r[1, 0] = qaxis(side = 2, at = c(0.2, .5, .6, .7, .9), sister = r[1, 1]) # y-axis
## r[0, 1] = qaxis(side = 3, data = list(xat = c(.1, .3, .7), xlabels = c('a', 'b', 'c')),
## sister = r[1, 1]) # top x-axis
## print(qplotView(scene = s)) # default layout is ugly; tune in r$gridLayout()




setMethod("print","QtVisnabView",function(x){
  x$show()
})
