##----------------------------------------------------------------------------##
##             For class "ScaleView"
##----------------------------------------------------------------------------##

ScaleView.gen <- setRefClass("ScaleView",contains = "QtVisnabView",
                             fields=list(track = "MutableGRanges"))

##----------------------------------------------------------------------------##
##             "IntervalView" constructor
##----------------------------------------------------------------------------##

ScaleView <- function(track,
                      seqname = NULL,
                      scene = NULL,
                      view = NULL,
                      rootLayer = NULL,
                      thisLayer = NULL,
                      selectedRangesModel = NULL,
                      selectedRangesModelColor = "red", 
                      row = 0L,
                      col = 0L,
                      rowSpan = 1L,
                      colSpan = 1L,
                      geom = c("twoside"),
                      rescale = "none",
                      ...){
  if(is(track,"GRanges"))
    track <- as(track,"MutableGRanges")
  if(is.null(seqname)){
    seqname <- as.character(unique(as.character(seqnames(track)))[1])
  }
  start <- 0
  end <- max(end(ranges(track[seqnames(track)==seqname])))
  if(is.null(selectedRangesModel))
    selectedRangesModel <- MutableGRanges()
  if(is(selectedRangesModel,"GRanges"))
    selectedRangesModel <- as(selectedRangesModel,"MutableGRanges")
  xlimZoom <- c(start,end)
  seqlength <- end
  pars <- GraphicPars(xlimZoom = xlimZoom, seqname = seqname,
                      seqlength = seqlength, geom = geom, 
                      view = "ScaleView")
  obj <- ScaleView.gen$new(track=track,pars=pars, selfSignal = FALSE,
                           selectedRangesModel = selectedRangesModel,
                           selectedRangesModelColor = selectedRangesModelColor,
                           row=row,col=col, rowSpan = rowSpan, colSpan = colSpan,
                           scene=scene,view=view,rootLayer=rootLayer,
                           outputRange = xlimZoom, focusin = FALSE,
                           thisLayer = thisLayer)
  obj$createView(rescale = rescale)
  obj$regSignal()
  obj
}


ScaleView.gen$methods(createView = function(seqname=NULL, rescale = "geometry"){
  if(is.null(scene)){
    scene <<- qscene()
    ## view <<- qplotView(scene,rescale="none")
    view <<- qplotView(scene, rescale = rescale)
    view$setDragMode(Qt$QGraphicsView$ScrollHandDrag)
    rootLayer <<- qlayer(scene,geometry=qrect(0,0,800,600))
  }
  if(!is.null(seqname))
    pars$seqname <<- seqname
  
  seqname <- pars$seqname
  bgcol <- pars$bgColor
  bgalpha <- pars$alpha
  qcol <- col2qcol(bgcol,bgalpha)
  scene$setBackgroundBrush(qbrush(qcol))
  ## set zoomLevels, this is not exposed to users
  zoomLevels <- c(500,50)
  h <- 10
  
  start <- 0
  end <- max(end(ranges(track[seqnames(track)==seqname])))

  lengths <- end
  pars$seqlength <<- end

  pfunScale <- function(layer,painter,exposed){
    if(pars$geom == "twoside"){
      pars$xlimZoomChanged$block()
      pars$xlimZoom <<- as.matrix(exposed)[,1]
          if(!selfSignal){
      outputRangeChanged$unblock()
      outputRange <<- pars$xlimZoom 
    }
    if(selfSignal){
      outputRangeChanged$block()
      outputRange <<- pars$xlimZoom 
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
  focusin <<- FALSE
}
hoverEnterFun <- function(layer, event){
  focusin <<- TRUE
}
hoverLeaveFun <- function(layer, event){
  focusin <<- FALSE
}
  thisLayer <<- qlayer(rootLayer,paintFun=pfunScale,
                  ## limits=qrect(pars$xlimZoom[1],h/2-h/9-3*h,
                  ##   pars$xlimZoom[2],h/2+h/9+h),
                  wheelFun=wheelEventZoom(view),
                  keyPressFun=keyPressEventZoom(track, view, sy = 1, focusin = focusin),
                  row=row, col=col, rowSpan=rowSpan, colSpan=colSpan,
                       hoverEnterFun = hoverEnterFun,
                       focusOutFun = keyOutFun, hoverLeaveFun = hoverLeaveFun)
 
  thisLayer$setLimits(qrect(0, -h/2-h/9-h/2, pars$seqlength, -h/2+h/9+h))
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

  pars$seqnameChanged$connect(function(){
    start <- 0
    end <- max(end(track[seqnames(track)==pars$seqname]))
    ## pars$seqlength <<- end-start
    ## pars$xlimZoomChanged$block()
    ## pars$xlimZoom <<- c(0, end)
    ## pars$xlimZoomChanged$unblock()
    thisLayer$close()
    view$resetTransform()               #this already fix xlim
    .self$createView()
    ## .self$regSignal()
  })

  pars$xlimZoomChanged$connect(function(){
    zoom_factor <- diff(pars$xlimZoom)/pars$seqlength
    ## then scale view
    view$resetTransform()
    view$scale(1/zoom_factor, 1)
    ## then center viewr
    pos.x <- mean(pars$xlimZoom)
    pos.y <- mean(pars$ylim)
    pos.scene <- as.numeric(thisLayer$mapToScene(pos.x, pos.y))
    view$centerOn(pos.scene[1], pos.scene[2])
  })

  pars$bgColorChanged$connect(function(){
    bgcol <- pars$bgColor
    bgalpha <- pars$alpha
    qcol <- col2qcol(bgcol,bgalpha)
    scene$setBackgroundBrush(qbrush(qcol))
  })
})


setMethod("geom","ScaleView",function(x,...){
  cat("Choosed geom: ",x$pars$geom,"\n")
  cat("---------------------\n")
  cat("Supported geoms: \n")
  geoms <- getOption("BioC")$visnab$ScaleView$geom
  if(!is.null(geoms))
    cat(geoms,"\n")
  else
    message("No supported geom is found for this object")
})

setReplaceMethod("geom","ScaleView", function(x,value){
  geoms <- getOption("BioC")$visnab$ScaleView$geom
  if(!(value %in% geoms))
    stop("Geom should be one of", geoms)
  else
    x$pars$geom <- value
  x
})






