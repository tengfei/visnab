##----------------------------------------------------------------------------##
##             For class "LinearView"
##----------------------------------------------------------------------------##

setClass("LinearView",contains="GraphicPars",
         representation(track="GenomicRanges",
                        type="characterOrNULL",
                        idname="characterOrNULL"))

##----------------------------------------------------------------------------##
##             "LinearView" constructor
##----------------------------------------------------------------------------##

LinearView <- function(gr,type=NULL,idname=NULL,...){
  pars <- GraphicPars(...)@pars
  new("LinearView",track=gr,species=species,
      pars=pars,type=type,idname=idname)
}

setMethod("visplot","GenomicRanges",function(obj,seqname="chr1",idname=NULL,
                                             show=TRUE,start=NULL,end=NULL,
                                             scene=NULL,view=NULL,rootLayer=NULL,
                                             row=0L,col=0L){
  if(is.null(scene)){
    scene <- qscene()
    view <- qplotView(scene,rescale="none")
    rootLayer <- qlayer(scene,geometry=qrect(0,0,800,600))
  }

  ## rootLayer <- qlayer(scene)
  bgcol <- getAttr("bg.col")
  bgalpha <- getAttr("bg.alpha")
  qcol <- col2qcol(bgcol,bgalpha)
  scene$setBackgroundBrush(qbrush(qcol))
  view <- qplotView(scene,rescale="none")
  env <- new.env()
  layer <- linearViewLayer(obj,seqname,env,rootLayer,view,
                           start=start,
                           end=end,
                           row=row,col=col,
                              rowSpan=1L,colSpan=1L,idname=idname)
  if(show) view$show()
  return(list(scene=scene,view=view,layer=layer,env=env))
})

setMethod("visplot","LinearView",function(obj,chr="chr1",show=TRUE,
                                          start=NULL,
                                          end=NULL){
  scene <- qscene()
  lroot <- qlayer(scene)
  bgcol <- getAttr("bg.col")
  bgalpha <- getAttr("bg.alpha")
  qcol <- col2qcol(bgcol,bgalpha)
  scene$setBackgroundBrush(qbrush(qcol))

  env <- new.env()
  layer <- linearViewLayer(obj@track,seqname,env,lroot,view,
                           start=start,
                           end=end,
                           row=0L,col=0L,
                              rowSpan=1L,colSpan=1L,idname=idname)
  if(show) view$show()
  return(list(scene=scene,view=view,layer=layer,env=env))
})

linearViewLayer <- function(obj,chr,env,
                            lroot=lroot,view=NULL,start=NULL,
                            end=NULL,
                            color="blue",
                            row=0L,col=0L,
                            rowSpan=1L,colSpan=1L,idname=idname){
  gr <- obj
  gr <- gr[seqnames(gr)==chr]
  if(is.null(start))
    start <- 0
  if(is.null(end))
    end <- max(end(gr))
  if(!is.null(start)&!is.null(end)){
     idx <- findOverlaps(IRanges(start=start,end=end),ranges(gr))@matchMatrix[,2]
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
              (binsexon*10+5)/binmx*5,stroke=color,fill=color)
  }
  if(is.null(idname))
    idname <- colnames(values(obj))[1]
  else
    idname <- idname
##  if(!is.null(start)&!is.null(end)){
  qlayer(lroot,paintFun=lvpainter,
                  limits=qrect(min(start(gr)),-2,
                    max(end(gr)),max((binsexon*10+5)/binmx*5)),
                  wheelFun=visWheelFun(view),
                  hoverMoveFun=visHoverLinear(gr,idname,view),
                  keyPressFun=function(layer,event){
                    key <- event$key()
                    if(key==Qt$Qt$Key_U)
                      viewUCSC(chr,env$xlimZoom[1],env$xlimZoom[2])
                    if(key==Qt$Qt$Key_Space)
                      view$resetTransform()
                  },
                  row=row,col=col,
                              rowSpan=rowSpan,colSpan=colSpan
                   )
## }else{
##     qlayer(lroot,paintFun=lvpainter,
##                   limits=qrect(start,-10,
##                     end,max((binsexon*10+5)/binmx*5)),
##                   wheelFun=visWheelFun(view),
##                   hoverMoveFun=visHoverLinear(gr,idname,view),
##                   keyPressFun=function(layer,event){
##                     key <- event$key()
##                     if(key==Qt$Qt$Key_U)
##                       viewUCSC(chr,env$xlimZoom[1],env$xlimZoom[2])
##                     if(key==Qt$Qt$Key_Space)
##                       view$resetTransform()
##                   },
##                   row=row,col=col,
##                               rowSpan=rowSpan,colSpan=colSpan,
##                     clip=TRUE
##                   )
## }
}

visWheelFun <- function(view){
  function(layer, event) {
    zoom_factor <- 2
    if (event$delta() < 0)
      zoom_factor <- 1/2
    view$scale(zoom_factor,1)
  }
}

visHoverLinear <- function(obj,name,view){
  function(layer,event){
    ##      event$widnameget()$setMouseTracking(TRUE)
    rect <- qrect(0,0,5,5)
    mat <- layer$deviceTransform(event)$inverted()
    rect <- mat$mapRect(rect)
    pos <- event$pos()
    rect$moveCenter(pos)
    hits <- layer$locate(rect)+1
    if(length(hits)>=1){
      posS <- event$screenPos()
      hits <- hits[1]
      text <- values(obj)[,colnames(values(obj))==name][hits]
      Qt$QToolTip$showText(posS,text)
    }else{
      Qt$QToolTip$hideText()
    }
  }
}
