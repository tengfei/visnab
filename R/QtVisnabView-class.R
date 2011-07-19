##----------------------------------------------------------##
##             For class "QtVisnabView"
##----------------------------------------------------------##
setRefClass("QtVisnabView",contains=c("VisnabView", "VIRTUAL"),
            fields=list(
              scene = "QGraphicsSceneORNULL",
              view = "Qanviz::PlotViewORNULL",
              rootLayer = "Qanviz::RLayerORNULL",
              rescale = "RescaleSingleEnum"),
            methods = list(
              setDislayWidgets = function(dragMode = TRUE){
                if(is.null(scene)){
                  scene <<- qscene()
                  view <<- qplotView(scene,rescale = rescale)
                  if(dragMode)
                    view$setDragMode(Qt$QGraphicsView$ScrollHandDrag)
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
              addLayer = function(layer, parent, i = 0L, j = 0L,...){
                if(missing(parent))
                  parent <- rootLayer
                ## qlayer(parent, ...)
                rootLayer[i, j] <<- layer
              },
              
              setLimits = function(i = 3, j = 2, ...){
                rootLayer[i, j]$setLimits(...)
              },

              setXLab = function(...){
                stop("not implemented yet")
              },

              setYLab = function(...){
                stop("not implemented yet")
              },

              showAxis = function(side = c(1, 2), ...){
                if(!all(side %in% c(1, 2, 3, 4)))
                  stop("side must be one or more of 1, 2, 3, 4")
                ## suppose mainLayer is always's in 
                for(s in side){
                  switch(s,{
                    i <- 4
                    j <- 2
                    data <- pars$xlim
                  },{
                    i <- 3
                    j <- 1
                    data <- pars$ylim
                  },{
                    i <- 2
                    j <- 2
                    data <- pars$xlim
                  },{
                    i <- 3
                    j <- 3
                    data <- pars$ylim
                  })
                  rootLayer[i, j] <<- qlayer(scene,
                                            axisPainter(side, data = data))
                }
              },

              setGrid = function(...){
                stop("not implemented yet")
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
                
              }
              ))

## TODO
## save
## save = function(file){
##   library(qtbase)
##   library(qtpaint)
##   scene <- qscene()
##   layer <- qlayer(scene, function(layer, painter){
##     qdrawCircle(painter, 5, 5, r = 5, fill = "black")
##   }, limits = qrect(0, 0, 10, 10))
## view <- qplotView(scene)
## view$show()
## view$setWindowTitle("tengfei")

##   qpixmap <- Qt$QPixmap(400, 300)
##   pt <- Qt$QPainter(qpixmap)
##   scene$render(pt)
##   qpixmap$save("~/Desktop/point.jpeg")
##   img <- qpixmap$toImage()
##   img$save("~/Desktop/point.gif")
## }

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
qgrid = function(parent = NULL, data = NULL, xat, yat, xlim, ylim, minor = 'xy',
                 sister = NULL, ...) {
    .bgcolor = "grey90"  # background color
    minor_at = function(at, lim) {
        n = length(at)
        if (n <= 1) return(NULL)
        l = at[1] - at[2]; r = at[n] - at[n - 1]
        at = (at[-1] + at[-n])/2
        n = n - 1
        at = sort(c(seq(at[1], lim[1], l), at[-c(1, n)], seq(at[n], lim[2], r)))
        at[at < lim[2] & at > lim[1]]
    }
    draw_grid = function(layer, painter) {
        if (!is.null(sister)) {
            lims = as.matrix(sister$limits())
            xlim = lims[, 1]
            ylim = lims[, 2]
        }
        qdrawRect(painter, xlim[1], ylim[1], xlim[2], ylim[2], stroke = .bgcolor,
            fill = .bgcolor)
        qlineWidth(painter) = 2
        if (!is.null(data)) {
            xat = data$xat
            yat = data$yat
        }
        qdrawSegment(painter, xat, ylim[1], xat, ylim[2], stroke = "white")
        qdrawSegment(painter, xlim[1], yat, xlim[2], yat, stroke = "white")
        ## minor grid
        qlineWidth(painter) = 1
        if (minor %in% c('x', 'xy')) {
            xat = minor_at(xat, xlim)
            if (length(xat))
                qdrawSegment(painter, xat, ylim[1], xat, ylim[2], stroke = "white")
        }
        if (minor %in% c('y', 'xy')) {
            yat = minor_at(yat, ylim)
            if (length(yat))
                qdrawSegment(painter, xlim[1], yat, xlim[2], yat, stroke = "white")
        }
    }
    if (!('limits' %in% names(list(...))) && !is.null(sister))
        qlayer(parent, paintFun = draw_grid, limits = sister$limits(), ...) else
    qlayer(parent, paintFun = draw_grid, ...)
}

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


axisPainter <- function(side, data, breaks = pretty_breaks(),
                     labels = scientific_format()){
  cbs <- cbreaks(range(data), breaks = breaks, labels = labels)
  xat <- yat <- cbs$breaks
  switch(side, {
    yat = 0.9
    yalign = "top"
    yshift1 = 0.01
    yshift2 = 0.1
  }, {
    xat = 0.9
    xalign = "right"
    xshift1 = 0.01
    xshift2 = 0.1
  }, {
    yat = 0.1
    yalign = "bottom"
    yshift1 = -0.01
    yshift2 = -0.1
  }, {
    xat = 0.1
    xalign = "left"
    xshift1 = -0.01
    xshift2 = -0.1
  })
  function(layer, painter){
    qdrawText(painter, cbs$labels, x = xat, y = yat, halign = xalign, valign = yalign)
    qdrawSegment(painter,  xat+ xshift1, yat + yshift1, xat + xshift2, yat + yshift2)
  }
}


setMethod("print","QtVisnabView",function(x){
  x$show()
})
