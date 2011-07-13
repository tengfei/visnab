##----------------------------------------------------------------------------##
##             For class "SeqView"
##----------------------------------------------------------------------------##

SeqView.gen <- setRefClass("SeqView",
                           contains = c("QtVisnabView", "LinearView"),
                           fields=list(track = "BSgenome"))

##----------------------------------------------------------------------------##
##             "SeqView" constructor
##----------------------------------------------------------------------------##

SeqView <- function(track,
                    seqname,
                    geom=c("default"),
                    rescale = c("geometry", "transform", "none"),
                    viewname = "Reference Sequence",
                    ...){

  geom <- match.arg(geom)
  geom <- new("SeqViewGeomSingleEnum", geom)

  rescale <- match.arg(rescale)
  rescale <- new("RescaleSingleEnum", rescale)

  tooltips <- capture.output(print(track))
  
  if(missing(seqname))
    seqname <- as.character(unique(as.character(seqnames(track)))[1])
  start <- 1
  end <- length(track[[seqname]])
  xlimZoom <- c(start,end)

  viewrange <- MutableGRanges(seqname, IRanges(start, end))
  seqlengths(viewrange) <- end
  
  if(extends(class(track),"GRanges"))
    track <- as(track,"MutableGRanges")
  
  pars <- GraphicPars(xlimZoom = xlimZoom, geom = geom, view = "SeqView")
  obj <- SeqView.gen$new(track=track,pars=pars, focusin = FALSE,
                         viewrange = viewrange, rescale = rescale, 
                         selfSignal = FALSE, tooltipinfo = tooltips)
  obj$createView()
  obj$regSignal()
  obj
}

##----------------------------------------------------------------------------##
##             print method
##----------------------------------------------------------------------------##

SeqView.gen$methods(createView = function(){


  seqname <- as.character(seqnames(viewrange))
  setDislayWidgets()
  setBgColor()
  
  ## set zoomLevels, this is not exposed to users
  zoomLevels <- c(10000,500,1)
  h <- 10
  lengths <- diff(pars$xlimZoom)
  ## pars$seqlength <<- lengths
  dna <- track[[seqname]]
  ## Unfinished
  pfunSeq <- function(layer,painter,exposed){
    ## level1:draw gray bars
    pars$xlimZoomChanged$block()
    pars$xlimZoom <<- as.matrix(exposed)[,1]
    if(!selfSignal){
      viewrange$rangesChanged$unblock()
      viewrange$ranges <<- IRanges(pars$xlimZoom[1],pars$xlimZoom[2])  
    }
    if(selfSignal){
      viewrange$rangesChanged$block()
      viewrange$ranges <<- IRanges(pars$xlimZoom[1],pars$xlimZoom[2])  
    }
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
  keyOutFun <- function(layer, event){
  focusin <<- FALSE
}
hoverEnterFun <- function(layer, event){
  focusin <<- TRUE
}
hoverLeaveFun <- function(layer, event){
  focusin <<- FALSE
}

  rootLayer[0,0] <<- qlayer(scene, pfunSeq, row=row, col=col,
                       rowSpan=rowSpan, colSpan=colSpan,
                   keyPressFun=keyPressEventZoom(track, view, sy = 1,
                     focusin = focusin),
                   wheelFun=wheelEventZoom(view),
                       hoverEnterFun = hoverEnterFun,
                       focusOutFun = keyOutFun, hoverLeaveFun = hoverLeaveFun)
  rootLayer[0,0]$setLimits(qrect(pars$xlimZoom[1],0,pars$xlimZoom[2],h))
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
  viewrange$seqnamesChanged$connect(function(){
    viewrange$seqnamesChanged$block()
    seqlengths(viewrange) <<- max(end(track[seqnames(track)==viewrange$seqnames]))
    viewrange$seqnamesChanged$unblock()
    ## end <- length(track[[as.character(viewrange$seqnames)]])
    ## pars$seqlength <<- end-start
    pars$xlimZoom <<- c(0, end)
    rootLayer[0,0]$close()
    view$resetTransform()
    createView()
    regSignal()
  })
  pars$xlimZoomChanged$connect(function(){
    zoom_factor <- diff(pars$xlimZoom)/seqlengths(viewrange)
    ## then scale view
    view$resetTransform()
    view$scale(1/zoom_factor, 1)
    ## then center viewr
    pos.x <- mean(pars$xlimZoom)
    pos.y <- mean(pars$ylim)
    pos.scene <- as.numeric(rootLayer[0,0]$mapToScene(pos.x, pos.y))
    view$centerOn(pos.scene[1], pos.scene[2])
  })
  pars$bgColorChanged$connect(function(){
    bgcol <- pars$bgColor
    bgalpha <- pars$alpha
    qcol <- col2qcol(bgcol,bgalpha)
    scene$setBackgroundBrush(qbrush(qcol))
  })
})


## setMethod("geom","SeqView",function(x,...){
##   cat("Choosed geom: ",x$pars$geom,"\n")
##   cat("---------------------\n")
##   cat("Supported geoms: \n")
##   geoms <- getOption("BioC")$visnab$TxdbView$geom
##   if(!is.null(geoms))
##     cat(geoms,"\n")
##   else
##     message("No supported geom is found for this object")
## })

## setReplaceMethod("geom","SeqView", function(x,value){
##   geoms <- getOption("BioC")$visnab$SeqView$geom
##   if(!(value %in% geoms))
##     stop("Geom should be one of", geoms)
##   else
##     x$pars$geom <- value
##   x
## })

