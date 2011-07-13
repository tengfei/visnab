##----------------------------------------------------------------------------##
##                    For class "CoverageView"
##----------------------------------------------------------------------------##
CoverageView.gen <- setRefClass("CoverageView",
                                contains = c("QtVisnabView", "LinearView"),
                                fields=list(track = "list",
                                  lower = "numeric",
                                  cutbin = "numeric",
                                  file = "character",
                                  BSgenome = "BSgenomeORNULL"
                                  ))

##----------------------------------------------------------------------##
##                   "CoverageView" constructor
##----------------------------------------------------------------------##

CoverageView <- function(file,
                         seqname,
                         BSgenome = NULL,
                         geom = c("total","mismatch","pairend","elength"),
                         rescale = c("geometry","transform" ,"none"),
                         viewname = "Coverage",
                         lower = 10L,
                         cutbin = 30L,
                         ...){

  ## get params
  geom <- match.arg(geom)
  geom <- new("CoverageViewGeomSingleEnum", geom)

  rescale <- match.arg(rescale)
  rescale <- new("RescaleSingleEnum", rescale)

  tooltips <- "not implemented"

  hd <- scanBamHeader(file)
  
  if(missing(seqname))
    seqname <- sort(names(hd[[1]]$targets))[1]
  seqlength <- hd[[1]]$targets[seqname]

  xlimZoom <- c(1, seqlength)

  viewrange <- MutableGRanges(seqname, IRanges(1, seqlength))
  seqlengths(viewrange) <- seqlength
  
  pars <- GraphicPars(geom = geom, xlimZoom = xlimZoom, view = "CoverageView")

  obj <- CoverageView.gen$new(file = file, focusin = FALSE, BSgenome = BSgenome,
                              selfSignal = FALSE, rescale = rescale,
                              viewrange = viewrange,
                              pars = pars, lower = lower, cutbin = cutbin)

  obj$createView()
  obj$regSignal()
  obj
}

##---------------------------------------------------------##
##             print method
##---------------------------------------------------------##

CoverageView.gen$methods(createView = function(){
  
  seqname <- as.character(seqnames(viewrange))
  setDislayWidgets()
  setBgColor()

  hd <- scanBamHeader(file)
  ## seqname <- sort(names(hd[[1]]$targets))[1]
  ## pars$seqlength <<- hd[[1]]$targets[seqname]
  pars$xlim <<- c(1, seqlengths(viewrange))
  ## pars$xlimZoom <<- c(1, pars$seqlength)
  seqname <- seqnames(viewrange)

  ## preset the level
  zoomLevel <- c(1e5, 1000,0)
  ## FIXME: make it flexible
  bam <- scanBam(file, param = ScanBamParam(which = GRanges(seqnames = seqname,
                                              IRanges(1, seqlengths(viewrange))),
                         what = c("pos", "qwidth")))
  bam <- bam[[1]]
  ir <- GRanges(seqnames=seqname,
                ranges=IRanges(start=bam$pos, width=bam$qwidth))
  covg <- coverage(ir)
  ## load("~/Datas/rdas/covlst.rda")
  ## covg <- covlst[[seqname]]
  ir.v <- slice(covg,lower = lower)
  xpos <- viewWhichMaxs(ir.v)
  ypos <- viewMaxs(ir.v)
  ## take log?make transformation formal
  pars$ylim <<- expand_range(c(0, max(log(ypos+1))), mul = 0.05)

  pfunCov <- function(layer,painter,exposed){
    pars$xlimZoomChanged$block()
    pars$xlimZoom <<- as.matrix(exposed)[,1]
    ## viewrange$ranges <<- pars$xlimZoom 
    if(!selfSignal){
      viewrange$rangesChanged$unblock()
      viewrange$ranges <<- IRanges(pars$xlimZoom[1] , pars$xlimZoom[2])
    }
    if(selfSignal){
      viewrange$rangesChanged$block()
      viewrange$ranges <<- IRanges(pars$xlimZoom[1] , pars$xlimZoom[2])
    }
    
    pars$xlimZoomChanged$unblock()
    xlimZoom <- as.matrix(exposed)[,1]
    ##======================================================================
    ## geom == "total"
    ##======================================================================
    if(pars$geom == "total"){
      ## level 1
      if(diff(xlimZoom)>zoomLevel[1]){
        .self$paintCovSeg(painter, xpos, ypos)
      }
      ## level 2&3
      if(diff(xlimZoom)<=zoomLevel[1] &
         diff(xlimZoom)>zoomLevel[3]){
        .self$paintCovRect(painter)
      }
    }
    ##======================================================================
    ## geom == "mismatch"
    ##======================================================================
    if(pars$geom == "mismatch"){
      ## level 1
      if(diff(xlimZoom)>zoomLevel[1]){
        .self$paintCovSeg(painter, xpos, ypos)
      }
      if(diff(xlimZoom)<=zoomLevel[1] &
         diff(xlimZoom)>zoomLevel[2]){
        .self$paintCovRect(painter)
      }
      if(diff(xlimZoom)<=zoomLevel[2] & diff(xlimZoom)>zoomLevel[3]){
        ## this should require loading or associated BSgenome
        ## And support sequence logo(maybe seperate painter)
        .self$paintCovMismatch(painter)
      }}
    if(pars$geom == "elength"){
      if(diff(xlimZoom)> 1e5){
        .self$paintCovSeg(painter, xpos, ypos)
      }
      if(diff(xlimZoom)<= 1e5 & diff(xlimZoom)>zoomLevel[3]){
        .self$paintFragLength(painter)
      }
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
  
  rootLayer[0,0] <<- qlayer(scene, paintFun=pfunCov,
                            row=row,  col=col, rowSpan=rowSpan, colSpan=colSpan,
                            wheelFun = wheelEventZoom(view),
                            keyPressFun = keyPressEventZoom(.self, view, sy = 1,
                              focusin = focusin),
                            hoverEnterFun = hoverEnterFun,
                            focusOutFun = keyOutFun, hoverLeaveFun = hoverLeaveFun)
  
  rootLayer[0,0]$setLimits(qrect(pars$xlim, pars$ylim))
  pars$ylimChanged$connect(function(){
    rootLayer[0,0]$setLimits(qrect(pars$xlim, pars$ylim))
  })
})


CoverageView.gen$methods(show = function(){
  view$show()
})

setMethod("print","CoverageView",function(x,..){
  x$show()
})

CoverageView.gen$methods(regSignal = function(){
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
    viewrange$ranges <<- IRanges(pars$xlimZoom[1] , pars$xlimZoom[2])
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
    createView()
    regSignal()
  })
  pars$bgColorChanged$connect(function(){
    bgcol <- pars$bgColor
    bgalpha <- pars$alpha
    qcol <- col2qcol(bgcol,bgalpha)
    scene$setBackgroundBrush(qbrush(qcol))
  })
})

CoverageView.gen$methods(paintCovSeg = function(painter, xpos, ypos){
  qdrawSegment(painter,xpos,0, xpos, log(ypos),stroke="gray50")
})

CoverageView.gen$methods(paintCovRect = function(painter){
  gr <- GRanges(seqnames = viewrange$seqnames,
                IRanges(pars$xlimZoom[1], pars$xlimZoom[2]))

  bam <- scanBam(file, param=ScanBamParam(which = gr))
  bam <- bam[[1]]
  ir <- IRanges(start=bam$pos,width=bam$qwidth)
  if(length(ir)>0){
    gr <- GRanges(seqnames = viewrange$seqnames, ir,
                  strand = bam$strand)
    cov <- coverage(ir, shift = -pars$xlimZoom[1])
    cov.n <- as.numeric(cov)
    covlen <- length(cov.n)
    x.pos <- pars$xlimZoom[1]:(pars$xlimZoom[1]+covlen-1)
    qdrawRect(painter, x.pos,0, x.pos+1, log(cov.n+1),fill="gray50", stroke = NA)
    ## qdrawPolygon(painter, c(x.pos[1],x.pos,tail(x.pos)[1]),
    ##              c(0,log(cov.n+1),0),
    ##              stroke=NA, fill="gray50")
  }})



CoverageView.gen$methods(paintCovMismatch = function(painter){
  if(is.null(BSgenome))
    stop("Please specify the associated BSgenome object
                if geom is set to mismatch")
  gr <- GRanges(seqnames = viewrange$seqnames,
                IRanges(pars$xlimZoom[1], pars$xlimZoom[2]))
  lgr <- pileupAsGRanges(file, gr)
  if(length(lgr)>0){
    lgr <- pileupGRangesAsVariantTable(lgr, BSgenome) #too slow...
    ## group by match and mismatch
    ## idx <- values(lgr)$read != values(lgr)$ref
    ## lgr <- lgr[idx]
    values(lgr)$isMatch <- values(lgr)$read == values(lgr)$ref
    idx <- order(start(lgr), values(lgr)$isMatch, values(lgr)$read)
    ## assumption: on the same chromosome
    lgr <- lgr[idx]
    eds <- unlist(lapply(split(values(lgr)$count, start(lgr)), function(x){
      cumsum(x)
    }))
    sts <- unlist(lapply(split(values(lgr)$count, start(lgr)), function(x){
      N <- length(x)
      c(0,cumsum(x)[-N])
    }))
    ism <- unlist(lapply(split(values(lgr)$isMatch, start(lgr)), function(x){
      x
    }))
    rds <- unlist(lapply(split(values(lgr)$read, start(lgr)), function(x){
      x
    }))
    ## assign color
    ## cols <- genLegend(lgr, color = "read")$color
    dnacol <- baseColor(IRanges:::safeExplode("ACTGN"))
    cols <- unlist(lapply(rds, function(rd){
      dnacol[[rd]]
    }))
    cols[ism] <- "gray80"
    ## values(lgr)$.color <- cols
    ## FIXME: need flexible way to make transformation
    ## paint coverage
    ## .self$paintCovRect(painter)
    ## only paint mismatch
    qdrawRect(painter, start(lgr), log(sts+1),
              start(lgr)+1, log(eds+1),
              stroke = NA, fill = cols)
  }
})

CoverageView.gen$methods(paintFragLength = function(painter){
  gr <- GRanges(seqnames = viewrange$seqnames,
                IRanges(pars$xlimZoom[1], pars$xlimZoom[2]))
  pspan <- pspanGR(file, gr)$pspan
  pspan <- subsetByOverlaps(ranges(pspan), ranges(gr), type = "within")
  ## draw point based gr
  ## THINK: MAKE IT GENERAL!
  x <- (start(pspan)+end(pspan))/2
  y <- width(pspan)
  ## cir <- qglyphCircle(r = 2)
  ## qdrawGlyph(painter, cir, x, y, stroke = NA, fill = "black")
  qdrawCircle(painter, x, y, r = 3, stroke = NULL, fill = "black")
  ## fixed or not?
  if(TRUE)
    pars$ylim <<- expand_range(c(min(y), max(y)), mul = 0.05)
})
