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
                      row = 0L,
                      col = 0L,
                      rowSpan = 1L,
                      colSpan = 1L,
                      geom = c("twoside"),
                      ...){
  if(is(track,"GRanges"))
    track <- as(track,"MutableGRanges")
  if(is.null(seqname)){
    seqname <- as.character(unique(as.character(seqnames(track)))[1])
    start <- 0
    end <- max(end(ranges(track[seqnames(track)==seqname])))
  }
  if(is.null(scene)){
    scene <- qscene()
    view <- qplotView(scene,rescale="none")
    view$setDragMode(Qt$QGraphicsView$ScrollHandDrag)
    rootLayer <- qlayer(scene,geometry=qrect(0,0,800,600))
  }
  xlimZoom <- c(start,end)
  pars <- GraphicPars(xlimZoom = xlimZoom, seqname = seqname,
                      view = "ScaleView", geom = geom)
  obj <- ScaleView.gen$new(track=track,pars=pars,
                           row=row,col=col, rowSpan = rowSpan, colSpan = colSpan,
                           scene=scene,view=view,rootLayer=rootLayer)
  obj$regSignal()
  obj$createView()
  obj
}


ScaleView.gen$methods(createView = function(seqname=NULL){
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
  lengths <- diff(pars$xlimZoom)
  pfunScale <- function(layer,painter,exposed){
    if(pars$geom == "twoside"){
      xlimZoom <- as.matrix(exposed)[,1]
      pars$xlimZoom <<- xlimZoom
      st <- xlimZoom[1]
      ed <- xlimZoom[2]
      scaleUnit <- as.integer(diff(xlimZoom))/5L
      xscale <- as.integer(xlimZoom[1]+scaleUnit*(0:5))
      N <- length(xscale)
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
  ## Unfinished
  layer <- qlayer(rootLayer,paintFun=pfunScale,
                  limits=qrect(pars$xlimZoom[1],h/2-h/9-3*h,
                    pars$xlimZoom[2],h/2+h/9+h),
                  wheelFun=wheelEventZoom(view),
                  keyPressFun=keyPressEventZoom(track, view, sy = 1),
                  row=row, col=col, rowSpan=rowSpan, colSpan=colSpan)
  ## layer$setGeometry(0,0,600,100)
})

ScaleView.gen$methods(show = function(){
  view$show()
})

setMethod("print","ScaleView",function(x,..){
  x$show()
})


## show supported geoms
setMethod("Geom","ScaleView",function(x,...){
  geoms <- options("BioC")$bioc$visnab$ScaleView$geom
  if(!is.null(geoms))
    print(geoms)
  else
    message("No supported geom is found for this object")
})



ScaleView.gen$methods(regSignal = function(){
  pars$geomChanged$connect(function(){
    qupdate(scene)
  })
  ##FIXME: need to be fixed
  pars$seqnameChanged$connect(function(){
    start <- 0
    end <- max(end(ranges(track[seqnames(track)==pars$seqname])))
    pars$xlimZoom <<- c(start,end)
    rootLayer$close()
    rootLayer <<- qlayer(scene,geometry=qrect(0,0,800,600),row=row)
    view$resetTransform()
    .self$createView()
  })
})




