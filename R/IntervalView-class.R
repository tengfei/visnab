##----------------------------------------------------------##
##             For class "IntervalView"
##----------------------------------------------------------##
## FIXME: 
setClass("IntervalView",contains="VnView",
         representation(track="GenomicRanges"))

##----------------------------------------------------------##
##             "IntervalView" constructor
##----------------------------------------------------------##

IntervalView <- function(gr,idname=NULL,
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
    seqnames <- as.character(unique(seqnames(gr))[1])
  if(is.null(idname))
    idname <- colnames(values(gr))[1]
  if(is.null(start))
    start <- 0
  if(is.null(end))
    end <- max(end(ranges(gr)))
  if(is.null(scene)){
    scene=qscene()
    view = qplotView(scene,rescale="none")
    rootLayer = qlayer(scene,geometry=qrect(0,0,800,600))
  }
  pars <- GraphicPars(...,scene=scene,view=view,
                      rootLayer=rootLayer,idname=idname,
                      seqnames=seqnames,start=start,
                      end=end,show=show,row=row,col=col,
                      stroke=stroke,fill=fill)@pars
  new("IntervalView",track=gr,pars=pars,show=TRUE)
}


setMethod("print","IntervalView",function(x,..){
  obj <- x
  scene <- obj@pars$scene
  lroot <- obj@pars$rootLayer
  view <- obj@pars$view
  seqnames <- obj@pars$seqnames
  bgcol <- getAttr("bg.col")
  bgalpha <- getAttr("bg.alpha")
  qcol <- col2qcol(bgcol,bgalpha)
  scene$setBackgroundBrush(qbrush(qcol))
  env <- new.env()
  gr <- obj@track
  gr <- gr[seqnames(gr)==seqnames]
  start <- obj@pars$start
  end <- obj@pars$end
  if(!is.null(start)&!is.null(end)){
    idx <- findOverlaps(IRanges(start=start,end=end),
                        ranges(gr))@matchMatrix[,2]
    gr <- gr[idx]
  }
  irexon <- IRanges(start(gr),end(gr))
  binsexon <- disjointBins(irexon)
  binmx <- max(binsexon*10+5)
  pos.center <- NULL
  lvpainter <- function(layer,painter,exposed){
    xlimZoom <- as.matrix(exposed)[,1]
    ylimZoom <- as.matrix(exposed)[,2]
    pos.enter <<- c(mean(xlimZoom),mean(ylimZoom))
    env$xlimZoom <<- xlimZoom
    env$ylimZoom <<- ylimZoom
    qdrawRect(painter,start(gr),(binsexon*10)/binmx*5,end(gr),
              (binsexon*10+5)/binmx*5,
              stroke=obj@pars$stroke,fill=obj@pars$fill)
  }

  layer <- qlayer(lroot,paintFun=lvpainter,
                  limits=qrect(min(start(gr)),-2,
                    max(end(gr)),max((binsexon*10+5)/binmx*5)),
                  wheelFun=  function(layer, event) {
                    zoom_factor <- 2
                    if (event$delta() < 0)
                      zoom_factor <- 1/2
                    view$scale(zoom_factor,1)
                  },
                  hoverMoveFun=  function(layer,event){
                    rect <- qrect(0,0,5,5)
                    mat <- layer$deviceTransform(event)$inverted()
                    rect <- mat$mapRect(rect)
                    pos <- event$pos()
                    rect$moveCenter(pos)
                    hits <- layer$locate(rect)+1
                    if(length(hits)>=1){
                      posS <- event$screenPos()
                      hits <- hits[1]
                      text <- values(gr)[,colnames(values(gr))==obj@pars$idname][hits]
                      Qt$QToolTip$showText(posS,text)
                    }else{
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
  return(list(scene=scene,view=view,layer=layer,env=env))
})


