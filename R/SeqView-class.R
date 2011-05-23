##----------------------------------------------------------------------------##
##             For class "SeqView"
##----------------------------------------------------------------------------##

SeqView.gen <- setRefClass("SeqView",contains = "QtVisnabView",
                           fields=list(track = "BSgenome"))

##----------------------------------------------------------------------------##
##             "SeqView" constructor
##----------------------------------------------------------------------------##

SeqView <- function(track,
                    seqname=NULL,
                    scene=NULL,
                    view = NULL,
                    rootLayer = NULL,
                    thisLayer = NULL,
                    row=0L,
                    col=0L,
                    rowSpan=1L,
                    colSpan=1L,
                    rescale = "geometry",
                    ...){


  if(is.null(seqname))
    seqname <- as.character(unique(as.character(seqnames(track)))[1])
  start <- 0
  end <- length(track[[seqname]])
  xlimZoom <- c(start,end)
  if(extends(class(track),"GRanges"))
    track <- as(track,"MutableGRanges")
  pars <- GraphicPars(xlimZoom = xlimZoom, seqname = seqname,
                      view = "SeqView")
  obj <- SeqView.gen$new(track=track,pars=pars, thisLayer = thisLayer,
                         row=row,col=col, rowSpan = rowSpan, colSpan = colSpan,
                         scene=scene,view=view,rootLayer=rootLayer, 
                         outputRange = xlimZoom)
  obj$createView(rescale = rescale)
  obj$regSignal()
  obj
}

##----------------------------------------------------------------------------##
##             print method
##----------------------------------------------------------------------------##

SeqView.gen$methods(createView = function(seqname=NULL, rescale = "geometry"){

  if(is.null(scene)){
    scene <<- qscene()
    view <<- qplotView(scene,rescale = rescale)
    view$setDragMode(Qt$QGraphicsView$ScrollHandDrag)
    rootLayer <<- qlayer(scene)
  }
  
  if(!is.null(seqname))
    pars$seqname <<- seqname
  seqname <- pars$seqname
  
  bgcol <- pars$bgColor
  bgalpha <- pars$alpha
  qcol <- col2qcol(bgcol,bgalpha)
  scene$setBackgroundBrush(qbrush(qcol))
  ## set zoomLevels, this is not exposed to users
  zoomLevels <- c(5000,100,1)
  h <- 10
  lengths <- diff(pars$xlimZoom)
  dna <- track[[seqname]]
  ## Unfinished
  pfunSeq <- function(layer,painter,exposed){
    ## level1:draw gray bars
    pars$xlimZoomChanged$block()
    pars$xlimZoom <<- as.matrix(exposed)[,1]
    outputRange <<- pars$xlimZoom 
    pars$xlimZoomChanged$unblock()
    xlimZoom <- as.matrix(exposed)[,1]
    if(diff(xlimZoom)>zoomLevels[1]){
      ## qdrawRect(painter,start,h/2,end,h/2+h/9,fill=pars$fill,stroke=NULL)
      qdrawSegment(painter, xlimZoom[1], h/2, xlimZoom[2], h/2, stroke = "black")
    }
    ## level2:draw colored segment
    if(diff(xlimZoom)<zoomLevels[1]&diff(xlimZoom)>zoomLevels[2]){
      dnav <- Views(dna,start = as.integer(xlimZoom[1]),
                    end = as.integer(xlimZoom[2]))
      st <- start(dnav)
       ed <- end(dnav)
       wd <- width(dnav)
       x_pos <- st+(1:wd)-1
      dnas.split <- IRanges::safeExplode(toString(dnav))
      dnacol <- baseColor(IRanges:::safeExplode("ACTGN"))
      idx <- match(dnas.split,names(dnacol))
      cols <- unname(unlist(dnacol[idx]))
      qdrawSegment(painter,x_pos, h/2-h/10, x_pos, h/2+h/10, stroke=cols)
    }
    ## level3: draw colored text
     if(diff(xlimZoom) <= zoomLevels[2] &
        diff(xlimZoom) >= zoomLevels[3]){
       dnav <- Views(dna,start=as.integer(xlimZoom[1]),end=as.integer(xlimZoom[2]))
       st <- start(dnav)
       ed <- end(dnav)
       wd <- width(dnav)
       x_pos <- st+(1:wd)-1
      dnas.split <- IRanges::safeExplode(toString(dnav))
      dnacol <- baseColor(IRanges:::safeExplode("ACTGN"))
      idx <- match(dnas.split,names(dnacol))
      cols <- unname(unlist(dnacol[idx]))
      qdrawText(painter,dnas.split,x_pos,h/2,"center","bottom",
                color=cols)
     }
  }
  layer <- qlayer(rootLayer, pfunSeq, row=row, col=col, rowSpan=rowSpan, colSpan=colSpan,
                   keyPressFun=keyPressEventZoom(track, view, sy = 1),
                   wheelFun=wheelEventZoom(view))
  layer$setLimits(qrect(pars$xlimZoom[1],0,pars$xlimZoom[2],h))
  ## layer$setGeometry(0,0,600,100)
})

SeqView.gen$methods(show = function(){
  view$show()
})

setMethod("print","SeqView",function(x,..){
  x$show()
})


SeqView.gen$methods(regSignal = function(){
  ## pars$geomChanged$connect(function(){
  ##   qupdate(scene)
  ## })
  ##FIXME: need to be fixed
  pars$seqnameChanged$connect(function(){
    start <- 0
    end <- max(end(ranges(track[seqnames(track)==pars$seqname])))
    pars$seqlength <<- end-start
    thisLayer$close()
    view$resetTransform()
    .self$createView()
    .self$regSignal()
  })

  pars$bgColorChanged$connect(function(){
    bgcol <- pars$bgColor
    bgalpha <- pars$alpha
    qcol <- col2qcol(bgcol,bgalpha)
    scene$setBackgroundBrush(qbrush(qcol))
  })
})


setMethod("geom","SeqView",function(x,...){
  cat("Choosed geom: ",x$pars$geom,"\n")
  cat("---------------------\n")
  cat("Supported geoms: \n")
  geoms <- getOption("BioC")$visnab$TxdbView$geom
  if(!is.null(geoms))
    cat(geoms,"\n")
  else
    message("No supported geom is found for this object")
})

setReplaceMethod("geom","SeqView", function(x,value){
  geoms <- getOption("BioC")$visnab$SeqView$geom
  if(!(value %in% geoms))
    stop("Geom should be one of", geoms)
  else
    x$pars$geom <- value
  x
})

