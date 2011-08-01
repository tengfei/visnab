##=================================================================##
##           Define all the methods for 'StackedView'
##=================================================================##

##-----------------------------------------------------------------##
##                  FOR class 'StackedView'
##-----------------------------------------------------------------##

SingleChromView.gen <- setRefClass("SingleChromView",
                                   contains=c("QtVisnabView", "LinearView"),
                                   fields=list(track="GRanges"))


## not support cytoband drawing temoprarily.
SingleChromView <- function(track,
                            seqname,
                            geom = c("cytoband"),
                            rescale = c("geometry", "transform", "none"),
                            ...){

  tooltips <- "not implemented yet"
    
  geom <- match.arg(geom)
  geom <- new("SingleChromViewGeomSingleEnum", geom)

  rescale <- match.arg(rescale)
  rescale <- new("RescaleSingleEnum", rescale)
  
  ## if(is(track,"GRanges"))
  ##   track <- as(track,"MutableGRanges")
  if(missing(seqname))
    seqname <- as.character(unique(as.character(seqnames(track)))[1])
  
  seqlength <- max(end(ranges(track[seqnames(track)==seqname])))
  
  pars <- GraphicPars(geom = geom, view = "SingleChromView",
                      xlimZoom = c(0, seqlength))

  viewrange <- MutableGRanges(seqname, IRanges(0, seqlength))
  seqlengths(viewrange) <- seqlength
  mode <- IModeGroup(scaleMode = ScaleMode(zoomMode = "Off"))
  obj <- SingleChromView.gen$new(track = track,pars = pars, mode = mode,
                                 viewrange = viewrange,
                                 rescale = rescale,
                                 eventTrace = new("EventTrace"))

  ## connected events
  obj$createView()
  obj$regSignal()
  obj
}

SingleChromView.gen$methods(createView = function(){

  seqname <- as.character(seqnames(viewrange))
  setDislayWidgets()
  setBgColor()
  ## FIXME: need to move to options?
  col.lst <- list(gpos100 = "black",
                  gpos75 = "gray75",
                  gpos50 = "gray50",
                  gpos25 = "gray25",
                  gneg = "white",
                  acen = "white",
                  gvar = "white",
                  stalk = "white")

  ## get ready to plot
  chr <- track[seqnames(track)==seqname]
  idx <- order(start(chr),decreasing=FALSE)
  chr <- chr[idx]
  nms <- values(chr)$name
  nms <- sapply(nms,substr,1,1)
  idx.l <- nms=="p"
  chr.left <- chr[idx.l]
  chr.l.rect <- chr.left[-length(chr.left)]

  idx.r <- nms=="q"
  chr.right <- chr[idx.r]
  chr.r.rect <- chr.right[-1]

  chr.l.tri <- chr.left[length(chr.left)]
  chr.r.tri <- chr.right[1]  
  chr.name <- as.character(unique(as.character(seqnames(chr))))
  offset <- (max(end(chr.r.rect))-min(start(chr.r.rect)))*0.05

  pfunChrom <- function(layer,painter,exposed){
    ## pars$xlimZoom <<- as.matrix(exposed)[,1]

    ## outbound rectangle
    qdrawRect(painter,min(start(chr.l.rect)),0,max(end(chr.l.rect)),10,stroke="black")
    qdrawRect(painter,start(chr.l.rect),0,end(chr.l.rect),10,stroke=NULL,
              fill=as.character(col.lst[as.character(values(chr.l.rect)$gieStain)]))

    ## draw right arms, if any
    qdrawRect(painter,min(start(chr.r.rect)),0,max(end(chr.r.rect)),10,stroke="black")
    qdrawRect(painter,start(chr.r.rect),0,end(chr.r.rect),10,stroke=NULL,
              fill=as.character(col.lst[as.character(values(chr.r.rect)$gieStain)]))

    ## draw centroid
    qdrawPolygon(painter,c(start(chr.l.tri), start(chr.l.tri),end(chr.l.tri)),
                 c(0,10,5),fill="darkred",stroke="darkred")
    qdrawPolygon(painter,c(start(chr.r.tri), end(chr.r.tri),end(chr.r.tri)),
                 c(5,10,0),fill="darkred",stroke="darkred")
    qdrawCircle(painter,start(chr.r.tri),5,2,fill="red",stroke=NA)
    ## Draw text
    qdrawText(painter,chr.name,0-offset,5,"right","center", color = "black")
    ## zoomed rect
  }
  pfunRect <- function(layer, painter){
    qdrawRect(painter,pars$xlimZoom[1],-5,pars$xlimZoom[2],15,stroke="red",fill=NA)    
  }
  ## event
  eventChrom <- function(layer,event){
    pos <- as.numeric(event$pos())
    wid <- width(viewrange$ranges)
    pars$xlimZoom <<- c(pos[1]-wid/2, pos[1]+wid/2)
    viewrange$ranges <<- IRanges(pars$xlimZoom[1], pars$xlimZoom[2])
  }
  lth <- max(end(track[seqnames(track) == seqname]))
  keyOutFun <- function(layer, event){
  eventTrace$focusin <<- FALSE
}
hoverEnterFun <- function(layer, event){
  eventTrace$focusin <<- TRUE
}
hoverLeaveFun <- function(layer, event){
  eventTrace$focusin <<- FALSE
}

 keyPressEvent <- function(layer, event){
   eventTrace$focusin <<- TRUE
 }
  
  rootLayer[0,0] <<- qlayer(scene, pfunChrom,
                       limits=qrect(-0.1*lth,-35,1.1*lth,45),cache = TRUE,
                       ## geometry=qrect(0,0,600,100),
                       row = row, col = col,
                       rowSpan = rowSpan, colSpan = colSpan,
                       keyPressFun = keyPressEvent,
                       hoverEnterFun = hoverEnterFun,
                       focusOutFun = keyOutFun, hoverLeaveFun = hoverLeaveFun) 
  ## thislayer$setAcceptedMouseButtons(0)
  ## rootLayer[0,0]$setAcceptedHoverEvents(FALSE)
  rectLayer <- qlayer(rootLayer, pfunRect,
                      limits=qrect(-0.1*lth,-35,1.1*lth,45),
                      mouseMoveFun=eventChrom, cache = FALSE)
  ## event
  pars$xlimZoomChanged$connect(function(){
    viewrange$ranges <<- IRanges(pars$xlimZoom[1], pars$xlimZoom[2])
    qupdate(rectLayer)
  })
  rootLayer$setGeometry(0,0,800, 50)
  ## layout <- rootLayer$gridLayout()
  ## layout$setRowStretchFactor(0,1)
})

SingleChromView.gen$methods(show = function(){
  view$show()
})

setMethod("print","SingleChromView",function(x,..){
  x$show()
})


## show supported geoms
SingleChromView.gen$methods(regSignal = function(){
  ## geom
  pars$geomChanged$connect(function(){
    qupdate(scene)
  })
  ## signal when change xlimZoom
  ## selectedRangeChanged$connect(function(){
  ##   qupdate(scene)
  ## })
  ## seqname change should update view and update seqlength
  viewrange$seqnamesChanged$connect(function(){
    viewrange$seqnamesChanged$block()
    seqlengths(viewrange) <<- seqlengths(track)[[as.character(viewrange$seqnames)]]
    viewrange$seqnamesChanged$unblock()
    ## pars$seqlength <<- end-start
    rootLayer[0,0]$close()
    view$resetTransform()
    createView()
    regSignal()
  })
  ## selectedRangesModelChanged$connect(function(){
  ##   qupdate(scene)
  ## })
  ## pars$bgColorChanged$connect(function(){
  ##   bgcol <- pars$bgColor
  ##   bgalpha <- pars$alpha
  ##   qcol <- col2qcol(bgcol,bgalpha)
  ##   scene$setBackgroundBrush(qbrush(qcol))
  ## })
})

