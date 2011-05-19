##=================================================================##
##           Define all the methods for 'StackedView'
##=================================================================##

##-----------------------------------------------------------------##
##                  FOR class 'StackedView'
##-----------------------------------------------------------------##

SingleChromView.gen <- setRefClass("SingleChromView",contains="QtVisnabView",
                                   fields=list(track="MutableGRanges"))


## This one is used for bird-eye overview
## not support cytoband drawing temoprarily.
SingleChromView <- function(track,species = NULL,
                            scene = NULL, view = NULL, rootLayer = NULL,
                            seqname = NULL, row = 0L, col = 0L,
                            rowSpan = 1L, colSpan = 1L,...){
  if(is(track,"GRanges"))
    track <- as(track,"MutableGRanges")
  if(is.null(scene)){
    scene <- qscene()
    view <- qplotView(scene,rescale="transform")
    rootLayer <- qlayer(scene,geometry=qrect(0,0,800,600))
  }
  if(is.null(seqname))
    seqname <- as.character(unique(as.character(seqnames(track)))[1])
  pars <- GraphicPars(seqname = seqname, view = "SingleChromView")
  obj <- SingleChromView.gen$new(track=track,pars=pars,scene=scene,
                             view=view,rootLayer=rootLayer, row = row, col = col,
                                 rowSpan = rowSpan, colSpan = colSpan)
  ## connected events
  obj$pars$seqnameChanged$connect(function(){qupdate(obj$scene)})  
  obj$createView()
  obj
}

SingleChromView.gen$methods(createView = function(seqname=NULL){
  bgcol <- pars$bgColor
  bgalpha <- pars$bgAlpha
  qcol <- col2qcol(bgcol,bgalpha)
  scene$setBackgroundBrush(qbrush(qcol))
  
  if(!is.null(seqname))
    pars$seqname <<- seqname
  seqname <- pars$seqname
  start <- 0
  end <- max(end(ranges(track[seqnames(track)==pars$seqname])))
  pars$xlimZoom <<- c(start,end)
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
    chr <- track[seqnames(track)==pars$seqname]
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
    ## Draw text
    chr.name <- as.character(unique(seqnames(chr)))
    offset <- (max(end(chr.r.rect))-min(start(chr.r.rect)))*0.05
    qdrawText(painter,chr.name,0-offset,5,"right","center", color = "black")
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
  lth <- max(end(track[seqnames(track)==pars$seqname]))
  layer <- qlayer(rootLayer, pfunChrom,
                  limits=qrect(-0.1*lth,-35,1.1*lth,45),
                  ## geometry=qrect(0,0,600,100),
                  mouseMoveFun=eventChrom,
                  row = row, col = col,
                  rowSpan = rowSpan, colSpan = colSpan)
  layout <- rootLayer$gridLayout()
  layout$setRowStretchFactor(0,1)
})

SingleChromView.gen$methods(show = function(){
  view$show()
})

setMethod("print","SingleChromView",function(x,..){
  x$show()
})

## show supported geoms
setMethod("Geom","SingleChromView",function(x,...){
  geoms <- options("BioC")$bioc$visnab$SingleChromView$geom
  if(!is.null(geoms))
    print(geoms)
  else
    message("No supported geom is found for this object")
})


                        

  
