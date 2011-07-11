##----------------------------------------------------------------------------##
##             For class "AlignmentView"
##----------------------------------------------------------------------------##
AlignmentView.gen <- setRefClass("AlignmentView",contains = "QtVisnabView",
                                 fields = list(track = "list",
                                   lower = "numeric",
                                   cutbin = "numeric",
                                   file = "character"))

##----------------------------------------------------------------------##
##             "AlignmentView" constructor
##----------------------------------------------------------------------##

AlignmentView <- function(file,
                          seqname,
                          geom = c("oneside","twoside","pairend"),
                          rescale = c("none", "geometry","transform"),
                          viewname = "Alignment",
                          lower = 10L,
                          cutbin = 30L,
                          ...){

  geom <- match.arg(geom)
  geom <- new("AlignmentViewGeomSingleEnum", geom)

  rescale <- match.arg(rescale)
  rescale <- new("RescaleSingleEnum", rescale)

  tooltips <- "none"

  
  ## read in header, to get seqnames
  hd <- scanBamHeader(file)
  seqname <- sort(names(hd[[1]]$targets))[1]
  seqlength <- hd[[1]]$targets[seqname]
  xlimZoom <- c(1, seqlength)
  pars <- GraphicPars(geom = geom,
                      xlimZoom = xlimZoom,
                      view = "AlignmentView")

  viewrange <- MutableGRanges(seqname, IRanges(1, seqlength))
  seqlengths(viewrange) <- seqlength

  
  obj <- AlignmentView.gen$new(file = file, focusin = FALSE,
                               selfSignal = FALSE, viewrange = viewrange,
                               viewname = viewname, tooltipinfo = tooltips,
                               pars = pars, lower = lower, cutbin = cutbin,
                               rescale = rescale)
  obj$createView()
  obj$regSignal()
  obj
}


##---------------------------------------------------------##
##             print method
##---------------------------------------------------------##

AlignmentView.gen$methods(createView = function(){

  seqname <- as.character(seqnames(viewrange))
  setDislayWidgets()
  setBgColor()
  
  ## hd <- scanBamHeader(file)
  ## pars$seqlength <<- hd[[1]]$targets[seqname]
  pars$xlim <<- c(0, seqlengths(viewrange))
  ## pars$xlimZoom <<- c(0, pars$seqlength)

  ## precessing data
  zoomLevel <- c(100000, 5000, 500, 0)  
  pfunAlign <- function(layer,painter,exposed){
    pars$xlimZoomChanged$block()
    pars$xlimZoom <<- as.matrix(exposed)[,1]
    ## viewrange$ranges <<- pars$xlimZoom 
    if(!selfSignal){
      viewrange$rangesChanged$unblock()
      viewrange$ranges <<- IRanges(pars$xlimZoom[1], pars$xlimZoom[2]) 
    }
    if(selfSignal){
      viewrange$rangesChanged$block()
      viewrange$ranges <<- IRanges(pars$xlimZoom[1], pars$xlimZoom[2]) 
    }
    pars$xlimZoomChanged$unblock()
    xlimZoom <- as.matrix(exposed)[,1]
    if(pars$geom == "oneside"){
      if(diff(xlimZoom)>zoomLevel[1]){
        .self$paintText(painter)
      }
      if(diff(xlimZoom) <= zoomLevel[1] &
         diff(xlimZoom) > zoomLevel[2]){
        .self$paintPolygon(painter)
      }
      if(diff(xlimZoom) <= zoomLevel[2]&
         diff(xlimZoom) > zoomLevel[3]){
        .self$paintRect(painter)
      }
      if(diff(xlimZoom) <= zoomLevel[3]&
         diff(xlimZoom) >= zoomLevel[4]){
        .self$paintSeq(painter)
      }
    }
    if(pars$geom == "pairend"){
      if(diff(xlimZoom)>zoomLevel[1]){
        .self$paintText(painter)
      }
      if(diff(xlimZoom) <= zoomLevel[1] &
         diff(xlimZoom) > zoomLevel[2]){
        .self$paintPolygon(painter)
      }
      if(diff(xlimZoom) <= zoomLevel[2]&
         diff(xlimZoom) >= zoomLevel[4]){
        .self$paintPair(painter)
      }
      ## if(diff(xlimZoom) <= zoomLevel[3]&
      ##    diff(xlimZoom) >= zoomLevel[4]){
      ##   .self$paintSeq(painter)
      ## }
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

  rootLayer[0,0] <<- qlayer(rootLayer,
                            paintFun = pfunAlign,
                            wheelFun = wheelEventZoom(view),
                            keyPressFun = keyPressEventZoom(.self, view, sy = 1,
                              focusin = focusin),
                            hoverEnterFun = hoverEnterFun,
                            focusOutFun = keyOutFun, hoverLeaveFun = hoverLeaveFun)

  pars$ylim <<- c(-(cutbin*90+80),0)
  rootLayer[0,0]$setLimits(qrect(pars$xlim, pars$ylim))
  pars$ylimChanged$connect(function(){
    rootLayer[0,0]$setLimits(qrect(pars$xlim, pars$ylim))
  })
  rootLayer[0,0]$setGeometry(0,0,600,150)
})

AlignmentView.gen$methods(paintText = function(painter){
  qdrawText(painter,"Keep zooming in to see more details",
            pars$xlimZoom[1]+0.5*(diff(pars$xlimZoom)), 5, "center","bottom")
  pars$ylim <<- c(0, 10)
})

AlignmentView.gen$methods(paintPolygon = function(painter){
  ## show coverage
  gr <- GRanges(seqnames = viewrange$seqnames,
                IRanges(pars$xlimZoom[1], pars$xlimZoom[2]))
  bam <- scanBam(file, param=ScanBamParam(which = gr,
                         what=c("pos", "qwidth", "strand")))
  bam <- bam[[1]]
  ir <- IRanges(start=bam$pos,width=bam$qwidth)
  if(length(ir)>0){
    gr <- GRanges(seqnames = viewrange$seqnames, ir,
                  strand = bam$strand)
    ## strand=bam$strand)
    cov <- coverage(ir, shift = -pars$xlimZoom[1])
    cov.n <- as.numeric(cov)
    covlen <- length(cov.n)
    x.pos <- pars$xlimZoom[1]:(pars$xlimZoom[1]+covlen-1)
    qdrawPolygon(painter, c(x.pos[1],x.pos,tail(x.pos)[1]),
                 c(0,-log(cov.n+1),0),
                 stroke=NA, fill="gray10")
    pars$ylim <<- c(-5,0)}

})
AlignmentView.gen$methods(paintRect = function(painter){
  gr <- GRanges(seqnames = viewrange$seqnames,
                IRanges(pars$xlimZoom[1], pars$xlimZoom[2]))
  bam <- scanBam(file, param=ScanBamParam(which = gr))
  bam <- bam[[1]]
  ir <- data.frame(start=bam$pos,width=width(bam$seq),
                   strand=bam$strand)
  if(nrow(ir)>0){
    ir <- GRanges(seqnames=viewrange$seqnames,
                  ranges=IRanges(start=ir$start, width=ir$width),
                  strand=ir$strand)
    ## ir <- ir[idxs]
    binir <- disjointBins(ranges(ir))
    binmx <- max(binir*90+80)
    x0 <- start(ir)
    y0 <- -binir*90
    x1 <- end(ir)
    y1 <- -(binir*90+80)
    idxp <- as.logical(strand(ir)=="+")
    idxn <- as.logical(strand(ir)=="-")
    if(sum(idxp)>0){
      qdrawRect(painter,x0[idxp],y0[idxp],x1[idxp],y1[idxp],
                stroke=rgb(0,0,1,0.8),fill=rgb(0,0,1,0.8))
    }
    if(sum(idxn)>0){
      qdrawRect(painter,x0[idxn],y0[idxn],x1[idxn],y1[idxn],
                stroke=rgb(0,1,0,0.8),fill=rgb(0,1,0,0.8))
    }
    pars$ylim <<- c(-(cutbin*90+80),0)}
})

AlignmentView.gen$methods(paintSeq = function(painter){
  gr <- GRanges(seqnames = viewrange$seqnames,
                IRanges(pars$xlimZoom[1], pars$xlimZoom[2]))
  bam <- scanBam(file, param=ScanBamParam(which = gr))
  bam <- bam[[1]]
  ir <- data.frame(start=bam$pos,width=width(bam$seq),
                   strand=bam$strand)
  if(nrow(ir)>0){
    ir <- GRanges(seqnames = viewrange$seqnames,
                  ranges=IRanges(start=ir$start, width=ir$width),
                  strand=ir$strand)
    seqs <- bam$seq
    wd <- width(seqs)
    seqstrs <- lapply(seq_along(wd),function(i)
                      safeExplode(toString(seqs[i])))
    ## need to show short read string
    ## ir <- ir[idxs]
    binir <- disjointBins(ranges(ir))
    binmx <- max(binir*90+80)
    x0 <- lapply(1:length(ir), function(i){
      start(ir[i])+(1:wd[i])-1
    })
    idxp <- as.logical(strand(ir)=="+")
    idxn <- as.logical(strand(ir)=="-")
    dnacol <- baseColor(safeExplode("ACTGN"))
    if(sum(idxp)>0){
      x0p <- unlist(x0[idxp])
      y0p <- rep(-binir[idxp]*90, times = wd[idxp])
      strsp <- unlist(seqstrs[idxp])
      idx <- match(strsp,names(dnacol))
      idxna <- is.na(idx)
      x0p <- x0p[!idxna]
      y0p <- y0p[!idxna]
      strsp <- strsp[!idxna]
      cols <- unname(unlist(dnacol[idx]))
      qdrawText(painter,strsp, x0p, y0p, color = cols)
    }
    if(sum(idxn)>0){
      x0n <- unlist(x0[idxn])
      y0n <- rep(-binir[idxn]*90, times = wd[idxn])
      strsn <- unlist(seqstrs[idxn])
      idx <- match(strsn,names(dnacol))
      idxna <- is.na(idx)
      x0n <- x0n[!idxna]
      y0n <- y0n[!idxna]
      strsn <- strsn[!idxna]
      cols <- unname(unlist(dnacol[idx]))
      qdrawText(painter, strsn, x0n, y0n, color = cols)
    }
    pars$ylim <<- c(-(cutbin*90+80),0)
  }
})


AlignmentView.gen$methods(paintPair = function(painter){
  ## cache it a little bi
  ## FIXME: when slide, order changes, cache may solve the problem
  xlimz <- expand_range(pars$xlimZoom, mul = 0.05)
  gr <- GRanges(seqnames = viewrange$seqnames,
                IRanges(xlimz[1], xlimz[2]))
  pgr <- pspanGR(file, gr)
  if(length(pgr)>0){
    lv <- disjointBins(ranges(pgr$pspan))
    p1 <- pgr$p1
    p2 <- pgr$p2
    gps <- pgr$pgaps
    ## link
    ## a little hack 
    qdrawSegment(painter, start(gps)-10, (lv-1)*10+5,
                 end(gps), (lv-1)*10+5, stroke = "gray70")
    ## paired read
    qdrawRect(painter, start(p1), (lv-1)*10+2, end(p1), lv*10,
              stroke = "blue", fill = "blue")
    qdrawRect(painter, start(p2), (lv-1)*10+2, end(p2), lv*10,
              stroke = "green", fill = "green")
    pars$ylim <<- expand_range(c(max(cutbin)*10, 0), mul = 0.05)
  }
})





AlignmentView.gen$methods(show = function(){
  view$show()
})

setMethod("print","AlignmentView",function(x,..){
  x$show()
})

AlignmentView.gen$methods(regSignal = function(){
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
    viewrange$ranges <<- IRanges(pars$xlimZoom[1], pars$xlimZoom[2]) 
  })
  pars$geomChanged$connect(function(){
    qupdate(scene)
  })
  viewrange$seqnamesChanged$connect(function(){
    hd <- scanBamHeader(file)
    viewrange$seqnamesChanged$block()
    seqlengths(viewrange) <<- hd[[1]]$targets[as.character(viewrange$seqnames)]
    viewrange$seqnamesChanged$unblock()
    rootLayer[0,0]$close()
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


