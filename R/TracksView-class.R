##----------------------------------------------------------------------------##
##                     "TracksView"
##----------------------------------------------------------------------------##

TracksView.gen <- setRefClass("TracksView",
                              contains="QtVisnabView",
                              fields=c(track="list",
                               win="QMainWindowORNULL",
                                signalingField("emit.id" , "numericORNULL")))


TracksView <- function(..., seqname="chr1"){
  track <- list(...)
  seqname <- track[[1]]$pars$seqname
  seqlength <- track[[1]]$pars$seqlength
  ## grand scene
  pars <- GraphicPars(seqname = seqname, xlimZoom = c(0, seqlength),
                      view = "TracksView")
  obj <- TracksView.gen$new(track=track,pars=pars, win = NULL,
                            selfSignal = TRUE, emit.id = 0)

  obj$createView()
  return(obj)
}


TracksView.gen$methods(createView = function(seqname=NULL){
  verticalTitleBar <- TRUE
  size <- c(1400,600)  
  lst <- track
  win <<- Qt$QMainWindow()
  win$resize(size[1],size[2])
  lapply(1:length(lst),function(i){
    cls <- class(lst[[i]])
    trackname <- cls
    ## track <- qplotView(scene,rescale="transform")
    ## track$setSceneRect(0,(i-1)*newh,xlim[1],newh*i)
    dw <- Qt$QDockWidget(trackname)
    if(verticalTitleBar){
      dw$setFeatures(Qt$QDockWidget$AllDockWidgetFeatures|
                     Qt$QDockWidget$DockWidgetVerticalTitleBar)
    }else{
      dw$setFeatures(Qt$QDockWidget$AllDockWidgetFeatures)
    }
    dw$setAllowedAreas(Qt$Qt$LeftDockWidgetArea|
                       Qt$Qt$RightDockWidgetArea)
    dw$setWidget(lst[[i]]$view)
    pars$seqnameChanged$connect(function(){
      lst[[i]]$pars$seqname <- pars$seqname
    })
    lst[[i]]$focusinChanged$connect(function(){
      other.id <- setdiff(1:length(lst),i)
      lst[[i]]$selfSignal <- FALSE
      sapply(other.id, function(n){
        lst[[n]]$selfSignal <- TRUE
      })
    })
    lst[[i]]$outputRangeChanged$connect(function(){
      pars$xlimZoom <<- lst[[i]]$pars$xlimZoom
    })
    pars$xlimZoomChanged$connect(function(){
      sapply(1:length(lst),function(n){
        lst[[n]]$pars$xlimZoom <- pars$xlimZoom
      })
    })
    win$addDockWidget(Qt$Qt$RightDockWidgetArea,dw,Qt$Qt$Vertical)
  })
})


TracksView.gen$methods(show = function(){
  win$show()
})

setMethod("print","TracksView",function(x,..){
  x$show()
})

## TracksView.gen <- setRefClass("TracksView",
##                               contains="QtVisnabView",
##                               fields=list(track="list",
##                                 ideogram="GRanges",
##                                 trackWidget="QWidgetORNULL"))

## TracksView <- function(...,ideogram=NULL,seqname=NULL){
##   if(is.null(ideogram))
##     ideogram <- getIdeogram()
##   if(is.null(seqname)){
##     seqname <- as.character(unique(as.character(seqnames(ideogram)))[1])
##     start <- 0
##     end <- max(end(ranges(ideogram[seqnames(ideogram)==seqname])))
##   }
##   xlimZoom <- c(start,end)
##   track <- list(...)
##   ## grand scene
##   pars <- GraphicPars(seqname=seqname, xlimZoom=xlimZoom)
##   obj <- TracksView.gen$new(track=track,pars=pars,ideogram=ideogram,
##                             trackWidget=NULL)
##   ## event
##   obj$pars$xlimZoomChanged$connect(function(){
##     qupdate(obj$scene.chrom)
##   })
##   obj$pars$seqnameChanged$connect(function(){
##     obj$createView()
##     gc()
##     obj$show()
##   })
  
##   obj$createView()
##   return(obj)
## }


## TracksView.gen$methods(createView = function(seqname=NULL){
##   scene <<- qscene()
##   ## grand view
##   view <<- qplotView(scene,rescale="none")
##   view$setDragMode(Qt$QGraphicsView$ScrollHandDrag)
##   ## rooy layer should be responsible for griding!
##   scene.chrom <<- qscene()
##   view.chrom <<- qplotView(scene.chrom)
##   if(is.null(trackWidget)){
##     trackWidget <<- Qt$QWidget()
##     trackLayout <- Qt$QGridLayout()
##     trackWidget$setLayout(trackLayout)
##   }
##   trackLayout <- trackWidget$layout()
##   trackLayout$addWidget(view.chrom,0,0)
##   trackLayout$addWidget(view,1,0)
##   trackLayout$setRowStretch(0,0)
##   trackLayout$setRowStretch(1,1)
##   trackLayout$setContentsMargins(5,5,5,5)
##   if(!is.null(seqname))
##     pars$seqname <<- seqname
##   seqname <- pars$seqname
##   start <- 0
##   end <- max(end(ranges(ideogram[seqnames(ideogram)==pars$seqname])))
##   pars$xlimZoom <<- c(start,end)
##   ## gr <- ideogram
##   ## col.lst <- list(gpos100 = "black",
##   ##                 gpos75 = "gray75",
##   ##                 gpos50 = "gray50",
##   ##                 gpos25 = "gray25",
##   ##                 gneg = "white",
##   ##                 acen = "white",
##   ##                 gvar = "white",
##   ##                 stalk = "white")
  
##   ## set geometry to rootLayer
##   ## pfunGrid <- function(layer,painter,exposed){
##   ##   xlimZoom <- as.matrix(exposed)[,1]
##   ##   ylimZoom <- c(0,600)
##   ##   aspect.ratio <- diff(xlimZoom)/20
##   ##   xscale <- seq(from=xlimZoom[1],to=xlimZoom[2],by=aspect.ratio)
##   ##   qdrawSegment(painter,xscale,-10,xscale,800,stroke="white")
##   ## }
##   ## gridLayer <- qlayer(scene,pfunGrid,limits=qrect(c(0,600),c(0,600)),
##   ##                     geometry=qrect(c(0,600),c(0,150*length(track))))
##   rootLayer <<- qlayer(scene,wheelFun=wheelEventZoom(view),cache=TRUE,
##                       geometry=qrect(c(0,600),c(0,150*length(track))))

##   ## grand layer
##   ## bgcol <- pars$bgColor
##   ## bgalpha <- pars$bgAlpha
##   ## qcol <- col2qcol(bgcol,bgalpha)
##   ## scene.chrom$setBackgroundBrush(qbrush(qcol))
  
##   sapply(1:length(track),function(i){
##     track[[i]]$pars$seqname <<- pars$seqname
##     track[[i]]$scene <<- scene
##     track[[i]]$view <<- view
##     track[[i]]$rootLayer <<- rootLayer
##     track[[i]]$row <<- as.integer(i-1)
##     ## add event
##     track[[i]]$pars$xlimZoomChanged$connect(function(){
##       pars$xlimZoom <<- track[[i]]$pars$xlimZoom
##     })
##     message("Constructing and Printing...",class(track[[i]]))
##     track[[i]]$createView()
##   })
##   layout <- rootLayer$gridLayout()
##   sapply(1:length(track),function(i){
##     layout$setRowStretchFactor(i-1,1)    
##   })
## })

## TracksView.gen$methods(show = function(){
##   trackWidget$show()
## })

## setMethod("print","TracksView",function(x,..){
##   x$show()
## })
