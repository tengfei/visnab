##----------------------------------------------------------------------------##
##             For class "SeqView"
##----------------------------------------------------------------------------##

SeqView.gen <- setRefClass("SeqView",contains="QtVisnabView",
                           fields=list(track="MutableGRanges",
                             BSgenome="BSgenomeORNULL"))


##----------------------------------------------------------------------------##
##             "IntervalView" constructor
##----------------------------------------------------------------------------##
## obj should expect a object form getIdeogram(simple reference, no base information)
## function or BSgenome (complex reference, with base paire information)

SeqView <- function(obj,
                    seqname=NULL,
                    scene=NULL,
                    view = NULL,
                    rootLayer = NULL,
                    row=0L,
                    col=0L,
                    rowSpan=1L,
                    colSpan=1L,
                    title=NULL,
                    fill="gray80",
                    ...){

  if(is.null(title))
    title <- deparse(substitute(obj))
  if(is.null(seqname)){
    seqname <- as.character(unique(as.character(seqnames(obj)))[1])
    start <- 0
    end <- max(end(ranges(obj[seqnames(obj)==seqname])))
  }
  xlimZoom <- c(start,end)
  if(extends(class(obj),"GRanges"))
    obj <- as(obj,"MutableGRanges")
  pars <- GraphicPars(xlimZoom = xlimZoom, seqname = seqname)
  if(!is.null(fill))
    pars$fill <- fill
  obj <- SeqView.gen$new(track=obj,pars=pars,title=title,
                         row=row,col=col, rowSpan = rowSpan, colSpan = colSpan,
                         scene=scene,view=view,rootLayer=rootLayer)
  obj$pars$seqnameChanged$connect(function(){
    start <- 0
    end <- max(end(ranges(obj$track[seqnames(obj$track)==seqname])))
    obj$pars$xlimZoom <- c(start,end)
    ## obj$scene <- qscene()
    obj$rootLayer$close()
    obj$rootLayer <- qlayer(obj$scene,geometry=qrect(0,0,800,600))
    obj$view$resetTransform()
    obj$createView()
  })

  obj$createView()
  obj
}


##----------------------------------------------------------------------------##
##             print method
##----------------------------------------------------------------------------##

SeqView.gen$methods(createView = function(seqname=NULL){
  if(is.null(scene)){
    scene <<- qscene()
    view <<- qplotView(scene,rescale="none")
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
  lengths <- diff(pars$xlimZoom)
  ## routing mouse control
  wheelZoom <- function(layer, event) {
    zoom_factor <- 1.5
    if(event$delta()<0)
      zoom_factor <- 1/1.5
    tform <- view$transform()
    tform$scale(zoom_factor,1)
    view$setTransform(tform)
  }

  keyPressEvent <- function(layer,event){
    if(event$modifiers() == Qt$Qt$ControlModifier&&
       event$key() == Qt$Qt$Key_Equal)
      view$scale(2,1)
    if(event$modifiers() == Qt$Qt$ControlModifier&&
       event$key() == Qt$Qt$Key_Minus)
      view$scale(1/2,1)
    if(event$modifiers() == Qt$Qt$ControlModifier&&
       event$key() == Qt$Qt$Key_0)
      view$resetTransform()
  }

  ## draw scale  
  pfunScale <- function(layer,painter,exposed){
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
  ## Unfinished
  pfunRef <- function(layer,painter,exposed){
    ## level1:draw gray bars
    if(diff(xlimZoom)>zoomLevels[1]){
      qdrawRect(painter,start,h/2,end,h/2+h/9,fill=pars$fill,stroke=NULL)
    }
    
    ## level3:draw colored text
    if(diff(xlimZoom)<zoomLevels[1]&diff(xlimZoom)>zoomLevels[2]){
      dna <- track[[seqname]]
      dna <- subseq(dna,start=xlimZoom[1],end=xlimZoom[2])
      dnas.split <- splitDNA(dna)
      dnacol <- baseColor(splitDNA("ACTG"))
      idx <- match(dnas.split,names(dnacol))
      cols <- unname(unlist(dnacol[idx]))
      w <- as.integer(diff(xlimZoom))
      x_pos <- as.integer(xlimZoom[1]+(1:w)-1)
      qdrawText(painter,dnas.split,x_pos,h/2,"center","bottom",
                color=cols)
    }}
  layer0 <- qlayer(rootLayer,row=row, col=col, rowSpan=rowSpan, colSpan=colSpan,
                   keyPressFun=keyPressEvent)
  layer0.scale <- qlayer(layer0,paintFun=pfunScale,
                         limits=qrect(pars$xlimZoom[1],h/2-h/9-3*h,
                           pars$xlimZoom[2],h/2+h/9+h),
                         wheelFun=wheelZoom,geometry=qrect(0,0,600,100))
  layer0$setGeometry(0,0,600,100)
})

SeqView.gen$methods(show = function(){
  view$show()
})

setMethod("print","SeqView",function(x,..){
  x$show()
})




