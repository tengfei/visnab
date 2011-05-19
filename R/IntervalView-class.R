##----------------------------------------------------------##
##             For class "IntervalView"
##----------------------------------------------------------##
IntervalView.gen <- setRefClass("IntervalView",
                                contains = "QtVisnabView",
                                fields = list(track = "MutableGRanges",
                                              flag = "logical"))

##----------------------------------------------------------##
##             "IntervalView" constructor
##----------------------------------------------------------##

IntervalView <- function(mr,
                         seqname = NULL,
                         scene = NULL,
                         view = NULL,
                         rootLayer = NULL,
                         row = 0L,
                         col = 0L,
                         rowSpan = 1L,
                         colSpan = 1L,
                         fill = "black",
                         geom = c("full","dense"),
                         ...){

  ## if null, set first chromosome as viewed chromosome
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
  if(is(mr, "GRanges"))
    mr <- as(mr,"MutableGRanges")
  ## connect signal
  mr$elementMetadataChanged$connect(function() {
    qupdate(scene)
  })
  pars <- GraphicPars(fill = fill,xlimZoom = xlimZoom, seqname = seqname,
                      geom = geom[1], ...,
                      view = "IntervalView")
  obj <- IntervalView.gen$new(track=mr,pars=pars,
                              row=row,col=col, rowSpan = rowSpan, colSpan = colSpan,
                              scene=scene,view=view,rootLayer=rootLayer)
  ## event
  ## add default attributes
  addAttr(obj$track,.color=obj$pars$fill,.hover=FALSE,.brushed=FALSE)
  obj$regSignal()
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
  mr <- track[seqnames(track)==seqname]
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
  pars$xlim <<- c(0, end(mr))
  pars$ylim <<- c(0, 5)
  mr.r <- reduce(mr)
  lvpainter <- function(layer,painter,exposed){
    xlimZoom <- as.matrix(exposed)[,1]
    ylimZoom <- as.matrix(exposed)[,2]
    pars$xlimZoom <<- xlimZoom
    pars$ylimZoom <<- ylimZoom
    ## Draw rectangle
    if(pars$geom == "full"){
      qdrawRect(painter,start(mr),(binsexon*10)/binmx*5,end(mr),
                (binsexon*10+5)/binmx*5,
                stroke=NA,fill=values(mr)$.color)
      pars$ylim <<- c(0,5)
    }
    if(pars$geom == "dense"){
      qdrawRect(painter,start(mr.r), 10, end(mr.r), 20,
                stroke=NA,fill=values(mr.r)$.color)
      pars$ylim <<- c(0,30)
    }
    ## qdrawText(painter,title,sum(xlimZoom)/2,
    ##           max((binsexon*10+5)/binmx*5),"center","top",color=pars$textColor)
    }

  hoverMoveEvent <- function(layer,event){
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
                  }
  ## used for hover
  flag <<- FALSE
  ## construct layer
  layer <- qlayer(rootLayer,paintFun=lvpainter,
                  wheelFun=  wheelEventZoom(view),
                  keyPressFun = keyPressEventZoom(track, view = view, sy = 1),
                  hoverMoveFun = hoverMoveEvent,
                  row=row,col=col,
                  rowSpan=rowSpan,colSpan=colSpan)
  layer$setLimits(qrect(pars$xlim[1], pars$ylim[1], pars$xlim[2], pars$ylim[2]))
  layer$setGeometry(0,0,600,150)
})


IntervalView.gen$methods(show = function(){
  view$show()
})

setMethod("print","IntervalView",function(x,..){
  x$show()
})

## show supported geoms
setMethod("Geom","IntervalView",function(x,...){
  geoms <- getOption("BioC")$visnab$IntervalView$geom
  if(!is.null(geoms))
    cat("Supported Geoms: ",geoms, "\n")
  else
    cat("No supported geom is found for this object\n")
})
