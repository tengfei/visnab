##----------------------------------------------------------------------------##
##             For class "ScaleView"
##----------------------------------------------------------------------------##

ScaleView.gen <- setRefClass("ScaleView",
                             contains = c("QtVisnabView", "LinearView"),
                             fields=list(track = "SimpleMutableGRanges"))

##----------------------------------------------------------------------------##
##             "IntervalView" constructor
##----------------------------------------------------------------------------##
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title \code{ScaleView} object constructor
##' @param track \code{GRanges} object 
##' @param seqname 
##' @param geom 
##' @param rescale 
##' @param viewname 
##' @param ...
##' @return 
##' @author tengfei
ScaleView <- function(track,
                      seqname,
                      geom = c("twoside"),
                      rescale = c("geometry", "transform", "none"),
                      viewname = "Scales",
                      ...){

  if(is(track,"GRanges"))
    track <- as(track,"MutableGRanges")
  if(missing(seqname)){
    seqname <- as.character(unique(as.character(seqnames(track)))[1])
  }
  tooltips <- "not implemented yet"
  
  start <- 0
  end <- max(end(ranges(track[seqnames(track) == seqname])))

  geom <- match.arg(geom)
  geom <- new("ScaleViewGeomSingleEnum", geom)
  
  rescale <- match.arg(rescale)
  rescale <- new("RescaleSingleEnum", rescale)

  xlimZoom <- c(start,end)

  viewrange <- MutableGRanges(seqname, IRanges(start, end))
  seqlengths(viewrange) <- end

  pars <- GraphicPars(xlimZoom = xlimZoom, geom = geom, view = "ScaleView")
  gp <- GraphicPars(view = "TxdbView")
  gp$geom <- "dense"

  mode <- IModeGroup(scaleMode = ScaleMode(zoomMode = "Off"))
  obj <- ScaleView.gen$new(track = track, pars = pars,
                           eventTrace = new("EventTrace"),
                           viewrange = viewrange,  mode = mode,
                           rescale = rescale, tooltipinfo = tooltips)
  obj$createView()
  obj$regSignal()
  obj
}


ScaleView.gen$methods(createView = function(){

  seqname <- as.character(seqnames(viewrange))
  .self$setDislayWidgets()
  .self$setBgColor()

  ## set zoomLevels, this is not exposed to users
  zoomLevels <- c(500,50)
  h <- 10
  
  start <- 0
  end <- max(end(ranges(track[seqnames(track)==seqname])))

  ## lengths <- end
  ## pars$seqlength <<- end

  pfunScale <- function(layer,painter,exposed){
    if(pars$geom == "twoside"){
      pars$xlimZoomChanged$block()
      pars$xlimZoom <<- as.matrix(exposed)[,1]
          if(!eventTrace$selfSignal){
      viewrange$rangesChanged$unblock()
      viewrange$ranges <<- IRanges(pars$xlimZoom[1], pars$xlimZoom[2]) 
    }
    if(eventTrace$selfSignal){
      viewrange$rangesChanged$block()
      viewrange$ranges <<- IRanges(pars$xlimZoom[1], pars$xlimZoom[2]) 
    }

      pars$xlimZoomChanged$unblock()
      st <- pars$xlimZoom[1]
      ed <- pars$xlimZoom[2]
      scaleUnit <- as.integer(diff(pars$xlimZoom))/5L
      xscale <- as.integer(pars$xlimZoom[1]+scaleUnit*(0:5))
      N <- length(xscale)
      ## draw width
      wids <- diff(pars$xlimZoom)
      cen <- mean(pars$xlimZoom)
      qdrawSegment(painter, pars$xlimZoom[1],-h/2+h/9+h/2,
                   pars$xlimZoom[1]+2*wids/5,-h/2+h/9+h/2, stroke = "black")
      qdrawSegment(painter, pars$xlimZoom[2],-h/2+h/9+h/2,
                   pars$xlimZoom[2]-2*wids/5,-h/2+h/9+h/2, stroke = "black")
      
      txts <- paste("Width:",as.integer(wids),"bp")
      qdrawText(painter, txts, cen, -h/2+h/9+h/2, color = "black",
                "center", "center")
      arrow.left <- qglyphArrow(direction="left")
      qdrawGlyph(painter, arrow.left, pars$xlimZoom[1],-h/2+h/9+h/2,
                 stroke = "black")
      arrow.left <- qglyphArrow(direction="right")
      qdrawGlyph(painter, arrow.left, pars$xlimZoom[2],-h/2+h/9+h/2,
                 stroke = "black")

      ## draw scales
      qdrawSegment(painter,st,-h/2,
                   ed,-h/2,stroke=pars$stroke)
      qdrawSegment(painter,xscale,-h/2-h/9,xscale,
                  -h/2+h/9,stroke=pars$stroke)
      idx <- (1L:as.integer(N/2))*2L
      qdrawText(painter,xscale[idx],
                xscale[idx],
                -h/2-h/9,"center","top",color=pars$textColor)
      qdrawText(painter,xscale[idx-1],xscale[idx-1],-h/2+h/9,"center","bottom",
                color=pars$textColor)
    }
  }

  keyOutFun <- function(layer, event){
  eventTrace$focusin <<- FALSE
}
hoverEnterFun <- function(layer, event){
  eventTrace$focusin <<- TRUE
}
hoverLeaveFun <- function(layer, event){
  eventTrace$focusin <<- FALSE
}
  rootLayer[0,0] <<- qlayer(scene,paintFun=pfunScale,
                  ## limits=qrect(pars$xlimZoom[1],h/2-h/9-3*h,
                  ##   pars$xlimZoom[2],h/2+h/9+h),
                  wheelFun=wheelEventZoom(),
                  keyPressFun=keyPressEventZoom(),
                  row=row, col=col, rowSpan=rowSpan, colSpan=colSpan,
                       hoverEnterFun = hoverEnterFun,
                       focusOutFun = keyOutFun, hoverLeaveFun = hoverLeaveFun)
 
  rootLayer[0,0]$setLimits(qrect(0, -h/2-h/9-h/2,
                                 as.numeric(seqlengths(viewrange)), -h/2+h/9+h))
  pars$ylim <<- c(-h/2-h/9-h/2, -h/2+h/9+h)
  ## not define selectedDataModel in this one, do we need it?
  ## layer$setGeometry(0,0,600,100)
})

ScaleView.gen$methods(show = function(){
  view$show()
})

setMethod("print","ScaleView",function(x,..){
  x$show()
})


ScaleView.gen$methods(regSignal = function(){
  pars$geomChanged$connect(function(){
    qupdate(scene)
  })

  viewrange$seqnamesChanged$connect(function(){
    viewrange$seqnamesChanged$block()
    seqlengths(viewrange) <<- max(end(track[seqnames(track)==viewrange$seqnames]))
    viewrange$seqnamesChanged$unblock()
    rootLayer[0,0]$close()
    view$resetTransform()  
    createView()
    regSignal()
  })

  pars$xlimZoomChanged$connect(function(){
    zoom_factor <- diff(pars$xlimZoom)/as.numeric(seqlengths(viewrange))
    ## then scale view
    view$resetTransform()
    view$scale(1/zoom_factor, 1)
    ## then center viewr
    pos.x <- mean(pars$xlimZoom)
    pos.y <- mean(pars$ylim)
    pos.scene <- as.numeric(rootLayer[0,0]$mapToScene(pos.x, pos.y))
    view$centerOn(pos.scene[1], pos.scene[2])
  })

  pars$bgColorChanged$connect(function(){
    bgcol <- pars$bgColor
    bgalpha <- pars$alpha
    qcol <- col2qcol(bgcol,bgalpha)
    scene$setBackgroundBrush(qbrush(qcol))
  })
})
