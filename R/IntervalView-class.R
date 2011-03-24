## TODO:
## 1. Fix tooltips, it looks like it map to the wrong place.
##    maybe locate to the wrong place.

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
                         rowSpan=1L,
                         colSpan=1L,
                         stroke=NA,
                         fill="black"){

  ## if null, set first chromosome as viewed chromosome
  if(is.null(seqnames))
    seqnames <- as.character(unique(as.character(seqnames(mr)))[1])
  if(is.null(start))
    start <- 0
  if(is.null(end))
    end <- max(end(ranges(mr)))
  viewmr <- MutableGRanges(seqnames=seqnames,ranges=IRanges(start=start,end=end))
  if(is.null(idname))
    idname <- colnames(values(mr))[1]
  if(is.null(scene)){
    scene=qscene()
    view = qplotView(scene,rescale="none")
    rootLayer = qlayer(scene,geometry=qrect(0,0,800,600))
  }
  if(extends(class(mr),"GRanges"))
    mr <- as(mr,"MutableGRanges")
  ## connect signal
  mr$elementMetadataChanged$connect(function() {qupdate(scene)})

  pars <- GraphicPars(stroke=stroke,fill=fill)
  obj <- new("IntervalView",track=mr,pars=pars,viewmr=viewmr,idname=idname,
             row=row,col=col, rowSpan = rowSpan, colSpan = colSpan,
             scene=scene,view=view,rootLayer=rootLayer,show=show)
  ## event
  obj@pars$strokeChanged$connect(function(){qupdate(scene)})
  obj@pars$fillChanged$connect(function(){
    values(obj@track)$.color <- obj@pars$fill
  })
  obj@pars$bgColorChanged$connect(function(){
    bgcol <- obj@pars$bgColor
    bgalpha <- obj@pars$alpha
    qcol <- col2qcol(bgcol,bgalpha)
    scene$setBackgroundBrush(qbrush(qcol))
  })
  ## add default attributes
  addDefAttr(obj)
  return(obj)
}


setMethod("print","IntervalView",function(x,..){
  obj <- x
  scene <- obj@scene
  lroot <- obj@rootLayer
  view <- obj@view
  seqnames <- as.character(seqnames(obj@viewmr))
  bgcol <- obj@pars$bgColor
  bgalpha <- obj@pars$alpha
  qcol <- col2qcol(bgcol,bgalpha)
  scene$setBackgroundBrush(qbrush(qcol))
  env <- new.env()
  mr <- obj@track
  mr <- mr[seqnames(mr)==seqnames]
  start <- start(obj@viewmr)
  end <- end(obj@viewmr)
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
              stroke=obj@pars$stroke,fill=values(obj@track)$.color)
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
                      values(obj@track)$.color[hits] <- obj@pars$hoverColor
                      text <- values(mr)[,colnames(values(mr))==obj@idname][hits]
                      Qt$QToolTip$showText(posS,getTooltipInfo(mr,hits))
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
                  row=obj@row,col=obj@col,
                  rowSpan=obj@rowSpan,colSpan=obj@colSpan
                  )
  if(obj@show) view$show()
})



setGeneric("setDefAttr",function(obj,...) standardGeneric("setDefAttr"))
setMethod("setDefAttr","IntervalView",function(obj,...){
  ## suppose when create default attibute list
  ## we have a copy of that in pars.
  lst <- obj@pars$attrs
  nms <- names(lst)
  for(nm in nms){
    values(obj@track)[nm] <- lst[[nm]]
  }
  obj@track
})
