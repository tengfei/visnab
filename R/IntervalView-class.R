## TODO:
## 1. Fix tooltips, it looks like it map to the wrong place.
##    maybe locate to the wrong place.
## 2. Or find a work-around
## 3. MutableGRanges test

##----------------------------------------------------------##
##             For class "IntervalView"
##----------------------------------------------------------##
setClass("IntervalView",contains="QtVisnabView",
         representation(track="MutableGRanges"))

##----------------------------------------------------------##
##             "IntervalView" constructor
##----------------------------------------------------------##

IntervalView <- function(mr,idname=NULL,
                         seqnames=NULL,
                         scene=NULL,
                         view = NULL,
                         rootLayer = NULL,
                         start=NULL,
                         end=NULL,
                         show=TRUE,
                         row=0L,
                         col=0L,
                         stroke="black",
                         fill="black",
                         ...){
  if(is.null(seqnames))
    seqnames <- as.character(unique(seqnames(mr))[1])
  if(is.null(idname))
    idname <- colnames(values(mr))[1]
  if(is.null(start))
    start <- 0
  if(is.null(end))
    end <- max(end(ranges(mr)))
  if(is.null(scene)){
    scene=qscene()
    view = qplotView(scene,rescale="none")
    rootLayer = qlayer(scene,geometry=qrect(0,0,800,600))
  }
  if(inherits(class(mr),"GenomicRanges"))
    mr <- as(mr,"MutableGRanges")
  ## add default attributes

  ## connect signal
  mr$elementMetadataChanged$connect(function() {qupdate(scene)})
  pars <- GraphicPars(...,scene=scene,view=view,
                      rootLayer=rootLayer,idname=idname,
                      start=start, end=end,show=show,row=row,col=col,
                      stroke=stroke,fill=fill)@pars
  obj <- new("IntervalView",track=mr,pars=pars,seqnames=seqnames,show=TRUE)
  addDefAttr(obj)
  obj
}


setMethod("print","IntervalView",function(x,..){
  obj <- x
  scene <- obj@pars$scene
  lroot <- obj@pars$rootLayer
  view <- obj@pars$view
  seqnames <- obj@seqnames
  bgcol <- getAttr("bg.col")
  bgalpha <- getAttr("bg.alpha")
  qcol <- col2qcol(bgcol,bgalpha)
  scene$setBackgroundBrush(qbrush(qcol))
  env <- new.env()
  mr <- obj@track
  mr <- mr[seqnames(mr)==seqnames]
  start <- obj@pars$start
  end <- obj@pars$end
  if(!is.null(start)&!is.null(end)){
    idx <- findOverlaps(IRanges(start=start,end=end),
                        ranges(mr))@matchMatrix[,2]
    mr <- mr[idx]
  }
  irexon <- IRanges(start(mr),end(mr))
  binsexon <- disjointBins(irexon)
  binmx <- max(binsexon*10+5)
  pos.center <- NULL
  lvpainter <- function(layer,painter,exposed){
    xlimZoom <- as.matrix(exposed)[,1]
    ylimZoom <- as.matrix(exposed)[,2]
    pos.enter <<- c(mean(xlimZoom),mean(ylimZoom))
    env$xlimZoom <<- xlimZoom
    env$ylimZoom <<- ylimZoom
    qdrawRect(painter,start(mr),(binsexon*10)/binmx*5,end(mr),
              (binsexon*10+5)/binmx*5,
              stroke=NA,fill=values(obj@track)$.color)
  }

  layer <- qlayer(lroot,paintFun=lvpainter,
                  limits=qrect(min(start(mr)),-2,
                    max(end(mr)),max((binsexon*10+5)/binmx*5)),
                  wheelFun=  function(layer, event) {
                    zoom_factor <- 2
                    if (event$delta() < 0)
                      zoom_factor <- 1/2
                    view$scale(zoom_factor,1)
                  },
                  hoverMoveFun=  function(layer,event){
                    rect <- qrect(0,0,1,1)
                    mat <- layer$deviceTransform(event)$inverted()
                    rect <- mat$mapRect(rect)
                    pos <- event$pos()
                    rect$moveCenter(pos)
                    hits <- layer$locate(rect)+1
                    if(length(hits)>=1){
                      posS <- event$screenPos()
                      hits <- hits[1]
                      values(obj@track)$.color[hits] <- "pink"
                      text <- values(mr)[,colnames(values(mr))==obj@pars$idname][hits]
                      Qt$QToolTip$showText(posS,text)
                    }else{
                      setDefAttr(obj)
                      Qt$QToolTip$hideText()
                    }
                  },
                  keyPressFun=function(layer,event){
                    key <- event$key()
                    if(key==Qt$Qt$Key_U)
                      viewUCSC(seqnames,env$xlimZoom[1],env$xlimZoom[2])
                    if(key==Qt$Qt$Key_Space)
                      view$resetTransform()
                  },
                  row=obj@pars$row,col=obj@pars$col
                  ## rowSpan=rowSpan,colSpan=colSpan
                  )
  if(obj@show) view$show()
  invisible(list(scene=scene,view=view,layer=layer,env=env))
})


## Define handler


