setOldClass("QWidget")
setClassUnion("QWidgetORNULL",c("QWidget","NULL"))
TracksView.gen <- setRefClass("TracksView",contains="QtVisnabView",
                              fields=list(track="list",
                                ideogram="GRanges",
                                trackWidget="QWidgetORNULL",
                                scene.chrom="QGraphicsSceneORNULL",
                                layer.chrom="Qanviz::RLayerORNULL",
                                view.chrom="Qanviz::PlotViewORNULL"))

TracksView <- function(...,ideogram=NULL,seqname=NULL){
  if(is.null(ideogram))
    ideogram <- getIdeogram()
  if(is.null(seqname)){
    seqname <- as.character(unique(as.character(seqnames(ideogram)))[1])
    start <- 0
    end <- max(end(ranges(ideogram[seqnames(ideogram)==seqname])))
  }
  xlimZoom <- c(start,end)
  track <- list(...)
  ## grand scene
  pars <- GraphicPars(seqname=seqname, xlimZoom=xlimZoom)
  obj <- TracksView.gen$new(track=track,pars=pars,ideogram=ideogram,
                            trackWidget=NULL)
  ## event
  obj$pars$xlimZoomChanged$connect(function(){
    qupdate(obj$scene.chrom)
  })
  obj$pars$seqnameChanged$connect(function(){
    obj$createView()
    gc()
    obj$show()
  })
  
  obj$createView()
  return(obj)
}


TracksView.gen$methods(createView = function(seqname=NULL){
  scene <<- qscene()
  ## grand view
  view <<- qplotView(scene,rescale="none")
  view$setDragMode(Qt$QGraphicsView$ScrollHandDrag)
  ## rooy layer should be responsible for griding!
  scene.chrom <<- qscene()
  view.chrom <<- qplotView(scene.chrom)
  if(is.null(trackWidget)){
    trackWidget <<- Qt$QWidget()
    trackLayout <- Qt$QGridLayout()
    trackWidget$setLayout(trackLayout)
  }
  trackLayout <- trackWidget$layout()
  trackLayout$addWidget(view.chrom,0,0)
  trackLayout$addWidget(view,1,0)
  trackLayout$setRowStretch(0,0)
  trackLayout$setRowStretch(1,1)
  trackLayout$setContentsMargins(5,5,5,5)
  if(!is.null(seqname))
    pars$seqname <<- seqname
  seqname <- pars$seqname
  start <- 0
  end <- max(end(ranges(ideogram[seqnames(ideogram)==pars$seqname])))
  pars$xlimZoom <<- c(start,end)
  gr <- ideogram
  col.lst <- list(gpos100 = "black",
                  gpos75 = "gray75",
                  gpos50 = "gray50",
                  gpos25 = "gray25",
                  gneg = "white",
                  acen = "white",
                  gvar = "white",
                  stalk = "white")
  
  pfunChrom <- function(layer,painter,exposed){
    xlimZoom <- as.matrix(exposed)[,1]
    ## pars$xlimZoom <<- xlimZoom
    chr <- gr[seqnames(gr)==pars$seqname]
    idx <- order(start(chr),decreasing=FALSE)
    chr <- chr[idx]
    nms <- values(chr)$name
    nms <- sapply(nms,substr,1,1)
    ## we need to make sure we draw it in the right order
    ## leftarms, if any
    idx.l <- nms=="p"
    chr.left <- chr[idx.l]
    chr.l.rect <- chr.left[-length(chr.left)]
    ## outbound rectangle
    qdrawRect(painter,min(start(chr.l.rect)),0,max(end(chr.l.rect)),10,stroke="black")
    qdrawRect(painter,start(chr.l.rect),0,end(chr.l.rect),10,stroke=NULL,
              fill=as.character(col.lst[as.character(values(chr.l.rect)$gieStain)]))

    ## draw right arms, if any
    idx.r <- nms=="q"
    chr.right <- chr[idx.r]
    chr.r.rect <- chr.right[-1]
    ## outboutn rectangle

    qdrawRect(painter,min(start(chr.r.rect)),0,max(end(chr.r.rect)),10,stroke="black")
    qdrawRect(painter,start(chr.r.rect),0,end(chr.r.rect),10,stroke=NULL,
              fill=as.character(col.lst[as.character(values(chr.r.rect)$gieStain)]))

    ## draw centroid
    chr.l.tri <- chr.left[length(chr.left)]
    chr.r.tri <- chr.right[1]  
    qdrawPolygon(painter,c(start(chr.l.tri), start(chr.l.tri),end(chr.l.tri)),
                 c(0,10,5),fill="darkred",stroke="darkred")
    qdrawPolygon(painter,c(start(chr.r.tri), end(chr.r.tri),end(chr.r.tri)),
                 c(5,10,0),fill="darkred",stroke="darkred")
    qdrawCircle(painter,start(chr.r.tri),5,2,fill="red",stroke=NA)
    ## zoomed rect
    qdrawRect(painter,pars$xlimZoom[1],-5,pars$xlimZoom[2],15,stroke="red",fill=NA)
  }
  ## event
  eventChrom <- function(layer,event){
    pos <- as.numeric(event$pos())
    pos.x <- pos[1]
    wids <- diff(pars$xlimZoom)
    xlimZoom <- c(pos.x-wids/2,pos.x+wids/2)
    pars$xlimZoom <<- xlimZoom
    pos.scene <- as.numeric(event$scenePos())
    view$centerOn(pos.scene[1],pos.scene[2])
  }
  ## set geometry to rootLayer
  pfunGrid <- function(layer,painter,exposed){
    xlimZoom <- as.matrix(exposed)[,1]
    ylimZoom <- c(0,600)
    aspect.ratio <- diff(xlimZoom)/20
    xscale <- seq(from=xlimZoom[1],to=xlimZoom[2],by=aspect.ratio)
    qdrawSegment(painter,xscale,-10,xscale,800,stroke="white")
  }
  wheelZoom <- function(layer, event) {
    zoom_factor <- 1.5
    if(event$delta()<0)
      zoom_factor <- 1/1.5
    tform <- view$transform()
    tform$scale(zoom_factor,1)
    view$setTransform(tform)
  }
  gridLayer <- qlayer(scene,pfunGrid,limits=qrect(c(0,600),c(0,600)),
                      geometry=qrect(c(0,600),c(0,150*length(track))))
  rootLayer <<- qlayer(scene,wheelFun=wheelZoom,cache=TRUE,
                      geometry=qrect(c(0,600),c(0,150*length(track))))

        ## grand layer
  bgcol <- pars$bgColor
  bgalpha <- pars$alpha
  qcol <- col2qcol(bgcol,bgalpha)
  scene.chrom$setBackgroundBrush(qbrush(qcol))
  
  lth <- max(end(gr[seqnames(gr)==pars$seqname]))
  layer.chrom <<- qlayer(scene.chrom,pfunChrom,
                        limits=qrect(-0.1*lth,-35,1.1*lth,45),
                        geometry=qrect(0,0,600,100),
                        mouseMoveFun=eventChrom)

  sapply(1:length(track),function(i){
    track[[i]]$pars$seqname <<- pars$seqname
    track[[i]]$scene <<- scene
    track[[i]]$view <<- view
    track[[i]]$rootLayer <<- rootLayer
    track[[i]]$row <<- as.integer(i-1)
    ## add event
    track[[i]]$pars$xlimZoomChanged$connect(function(){
      pars$xlimZoom <<- track[[i]]$pars$xlimZoom
    })
    message("Constructing and Printing...",class(track[[i]]))
    track[[i]]$createView()
  })
  layout <- rootLayer$gridLayout()
  sapply(1:length(track),function(i){
    layout$setRowStretchFactor(i-1,1)    
  })
})

TracksView.gen$methods(show = function(){
  trackWidget$show()
  ## view$show()
})

setMethod("print","TracksView",function(x,..){
  x$show()
})
