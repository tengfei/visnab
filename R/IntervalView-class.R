##----------------------------------------------------------##
##             For class "IntervalView"
##----------------------------------------------------------##
IntervalView.gen <- setRefClass("IntervalView",contains="QtVisnabView",
                                fields=list(track="MutableGRanges",
                                  flag="logical"))

##----------------------------------------------------------##
##             "IntervalView" constructor
##----------------------------------------------------------##

IntervalView <- function(mr,
                         seqname=NULL,
                         scene=NULL,
                         view = NULL,
                         rootLayer = NULL,
                         row=0L,
                         col=0L,
                         rowSpan=1L,
                         colSpan=1L,
                         fill="black",
                         title=NULL){

  ## if null, set first chromosome as viewed chromosome
  if(is.null(title))
    title <- deparse(substitute(mr))
  if(is.null(seqname))
    seqname <- as.character(unique(as.character(seqnames(mr)))[1])
  start <- 0
  end <- max(end(ranges(mr[seqnames(mr)==seqname])))
  xlimZoom <- c(start,end)
  if(is.null(scene)){
    scene <- qscene()
    view <- qplotView(scene,rescale="none")
    view$setDragMode(Qt$QGraphicsView$ScrollHandDrag)
    rootLayer <- qlayer(scene,geometry=qrect(0,0,800,600))
  }
  if(extends(class(mr),"GRanges"))
    mr <- as(mr,"MutableGRanges")
  ## connect signal
  mr$elementMetadataChanged$connect(function() {
    qupdate(scene)
  })
  pars <- GraphicPars(fill=fill,xlimZoom = xlimZoom, seqname=seqname)
  obj <- IntervalView.gen$new(track=mr,pars=pars,
                              row=row,col=col, rowSpan = rowSpan, colSpan = colSpan,
                              scene=scene,view=view,rootLayer=rootLayer,title=title)
  obj$pars$strokeChanged$connect(function(){qupdate(scene)})
  obj$pars$fillChanged$connect(function(){
    values(obj@track)$.color <- obj@pars$fill
  })
  obj$pars$bgColorChanged$connect(function(){
    bgcol <- obj@pars$bgColor
    bgalpha <- obj@pars$alpha
    qcol <- col2qcol(bgcol,bgalpha)
    scene$setBackgroundBrush(qbrush(qcol))
  })
  obj$pars$seqnameChanged$connect(function(){
    start <- 0
    end <- max(end(ranges(obj$track[seqnames(obj$track)==obj$pars$seqname])))
    obj$pars$xlimZoom <- c(start,end)
    ## obj$scene <- qscene()
    obj$rootLayer$close()
    obj$rootLayer <- qlayer(obj$scene,geometry=qrect(0,0,800,600),row=obj$row)
    obj$view$resetTransform()
    obj$createView()
  })

  ## add default attributes
  addAttr(obj$track,.color=obj$pars$fill,.hover=FALSE,.brushed=FALSE)
  obj$createView()
  return(obj)
}

############################################################
## createview method
############################################################
IntervalView.gen$methods(createView = function(seqname=NULL){
  if(!is.null(seqname))
    pars$seqname <<- seqname
  seqname <- pars$seqname
  bgcol <- pars$bgColor
  bgalpha <- pars$alpha
  qcol <- col2qcol(bgcol,bgalpha)
  scene$setBackgroundBrush(qbrush(qcol))
  mr <- track
  mr <- mr[seqnames(mr)==seqname]
  start <- pars$xlimZoom[1]
  end <- pars$xlimZoom[2]
  if(!is.null(start)&!is.null(end)){
    idx <- findOverlaps(IRanges(start=start,end=end),
                        ranges(mr))@matchMatrix[,2]
    mr <- mr[idx]
  }
  irexon <- IRanges(start(mr),end(mr))
  binsexon <- disjointBins(irexon)
  binmx <- max(binsexon*10+5)
  lvpainter <- function(layer,painter,exposed){
    xlimZoom <- as.matrix(exposed)[,1]
    ylimZoom <- as.matrix(exposed)[,2]
    pars$xlimZoom <<- xlimZoom
    pars$ylimZoom <<- ylimZoom
    ## Draw rectangle
    qdrawRect(painter,start(mr),(binsexon*10)/binmx*5,end(mr),
              (binsexon*10+5)/binmx*5,
              stroke=NA,fill=values(track)$.color)

    ## Draw title
    qdrawText(painter,title,sum(xlimZoom)/2,
              max((binsexon*10+5)/binmx*5),"center","top",color=pars$textColor)
  }
  keyPressEvent <- function(layer,event){
                    if(event$modifiers() == Qt$Qt$ControlModifier){
                      if(event$key() == Qt$Qt$Key_Equal)
                        view$scale(1.5,1)
                      if(event$key() == Qt$Qt$Key_Minus)
                        view$scale(1/1.5,1)
                      if(event$key() == Qt$Qt$Key_0)
                        view$resetTransform()
                    }
                      ## if(event$key() == Qt$Qt$Key_u)
                      ##    viewInUCSC(obj)
  }
  ## used for hover
  flag <<- FALSE
  ## construct layer
  layer <- qlayer(rootLayer,paintFun=lvpainter,
                  wheelFun=  function(layer, event) {
                    zoom_factor <- 2
                    if (event$delta() < 0)
                      zoom_factor <- 1/2
                    view$scale(zoom_factor,1)
                  },
                  keyPressFun = keyPressEvent,
                  hoverMoveFun=function(layer,event){
                    rect <- qrect(0,0,1,1)
                    mat <- layer$deviceTransform(event)$inverted()
                    rect <- mat$mapRect(rect)
                    pos <- event$pos()
                    rect$moveCenter(pos)
                    hits <- layer$locate(rect)+1
                    if(length(hits)>=1){
                      posS <- event$screenPos()
                      hits <- hits[1]
                      values(track)$.color[hits] <<- pars$hoverColor
                      ## Qt$QToolTip$showText(posS,getTooltipInfo(mr,hits))
                      flag <<- TRUE
                    }else{
                      if(flag){
                        values(track)$.color <<- pars$fill
                        flag <<- FALSE
                      }
                    }
                  },
                  row=row,col=col,
                  rowSpan=rowSpan,colSpan=colSpan)
  layer$setLimits(qrect(min(start(mr)),-2,max(end(mr)),7))                      
  layer$setGeometry(0,0,600,150)
})


IntervalView.gen$methods(show = function(){
  view$show()
})

setMethod("print","IntervalView",function(x,..){
  x$show()
})


