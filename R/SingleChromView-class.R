##=================================================================##
##           Define all the methods for 'StackedView'
##=================================================================##

##-----------------------------------------------------------------##
##                  FOR class 'StackedView'
##-----------------------------------------------------------------##

SingleChromView.gen <- setRefClass("SingleChromView",contains="QtVisnabView",
                                   fields=list(track="MutableGRanges"))


## not support cytoband drawing temoprarily.
SingleChromView <- function(track,  genome=NULL,              
                            scene = NULL, view = NULL, rootLayer = NULL,
                            thisLayer = NULL,
                            selectedRangesModel = NULL,
                            selectedRangesModelColor = "red", 
                            seqname = NULL, row = 0L, col = 0L,
                            rowSpan = 1L, colSpan = 1L,
                            geom = c("cytoband"),
                            rescale = "geometry",...){
  if(is(track,"GRanges"))
    track <- as(track,"MutableGRanges")
  if(is.null(seqname))
    seqname <- as.character(unique(as.character(seqnames(track)))[1])
  if(is.null(genome)){
    cat("Set genome to hg19 automatically, please make sure it's the one you want\n")
    genome <- "hg19"
  }
  seqlength <- max(end(ranges(track[seqnames(track)==seqname])))
  if(is.null(selectedRangesModel))
    selectedRangesModel <- MutableGRanges()
  if(is(selectedRangesModel,"GRanges"))
    selectedRangesModel <- as(selectedRangesModel,"MutableGRanges")
  pars <- GraphicPars(seqname = seqname, seqlength = seqlength,
                      geom = geom[1], view = "SingleChromView")
  obj <- SingleChromView.gen$new(track = track,pars = pars,scene = scene,
                                 view = view,rootLayer = rootLayer,
                                 thisLayer = thisLayer,focusin = FALSE,
                                 row = row, col = col, outputRange = c(0, seqlength),
                                 rowSpan = rowSpan, colSpan = colSpan)
  ## connected events
  obj$createView(rescale = rescale)
  obj$regSignal()
  obj
}

SingleChromView.gen$methods(createView = function(seqname=NULL,
                              rescale = "geometry"){

  if(is.null(scene)){
    scene <<- qscene()
    view <<- qplotView(scene, rescale = rescale)
    rootLayer <<- qlayer(scene,geometry=qrect(0,0,800,600), cache = FALSE)
  }

  bgcol <- pars$bgColor
  bgalpha <- pars$bgAlpha
  qcol <- col2qcol(bgcol,bgalpha)
  scene$setBackgroundBrush(qbrush(qcol))
  if(!is.null(seqname))
    pars$seqname <<- seqname
  seqname <- pars$seqname
  start <- 0
  end <- max(end(ranges(track[seqnames(track)==pars$seqname])))
  pars$seqlength <<- end
  pars$xlimZoom <<- c(start,end)
  col.lst <- list(gpos100 = "black",
                  gpos75 = "gray75",
                  gpos50 = "gray50",
                  gpos25 = "gray25",
                  gneg = "white",
                  acen = "white",
                  gvar = "white",
                  stalk = "white")

  ## get ready to plot
  chr <- track[seqnames(track)==pars$seqname]
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
    wid <- diff(outputRange)
    pars$xlimZoom <<- c(pos[1]-wid/2, pos[1]+wid/2)
    outputRange <<- pars$xlimZoom
  }
  lth <- max(end(track[seqnames(track)==pars$seqname]))
  keyOutFun <- function(layer, event){
  focusin <<- FALSE
}
hoverEnterFun <- function(layer, event){
  focusin <<- TRUE
}
hoverLeaveFun <- function(layer, event){
  focusin <<- FALSE
}

 keyPressEvent <- function(layer, event){
   focusin <<- TRUE
 }
  
  thisLayer <<- qlayer(rootLayer, pfunChrom,
                       limits=qrect(-0.1*lth,-35,1.1*lth,45),cache = TRUE,
                       ## geometry=qrect(0,0,600,100),
                       row = row, col = col,
                       rowSpan = rowSpan, colSpan = colSpan,
                       keyPressFun = keyPressEvent,
                       hoverEnterFun = hoverEnterFun,
                       focusOutFun = keyOutFun, hoverLeaveFun = hoverLeaveFun) 
  ## thislayer$setAcceptedMouseButtons(0)
  ## thisLayer$setAcceptedHoverEvents(FALSE)
  rectLayer <- qlayer(rootLayer, pfunRect,
                      limits=qrect(-0.1*lth,-35,1.1*lth,45),
                      ## geometry=qrect(0,0,600,100),
                      mouseMoveFun=eventChrom, cache = FALSE)
  ## event
  pars$xlimZoomChanged$connect(function(){
    outputRange <<- pars$xlimZoom
    qupdate(rectLayer)
  })
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
  pars$seqnameChanged$connect(function(){
    start <- 0
    end <- seqlengths(track)[[pars$seqname]]
    pars$seqlength <<- end-start
    thisLayer$close()
    view$resetTransform()
    .self$createView()
    .self$regSignal()
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




setMethod("geom","SingleChromView",function(x,...){
  cat("Choosed geom: ",x$pars$geom,"\n")
  cat("---------------------\n")
  cat("Supported geoms: \n")
  geoms <- getOption("BioC")$visnab$SingleChromView$geom
  if(!is.null(geoms))
    cat(geoms,"\n")
  else
    message("No supported geom is found for this object")
})

setReplaceMethod("geom","SingleChromView", function(x,value){
  geoms <- getOption("BioC")$visnab$SingleChromView$geom
  if(!(value %in% geoms))
    stop("Geom should be one of", geoms)
  else
    x$pars$geom <- value
  x
})



