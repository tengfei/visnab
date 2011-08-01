BamView.gen <- setRefClass("BamView",
                                contains = c("QtVisnabView", "LinearView"),
                                fields=list(
                                  track = "list",
                                  file = "character",
                                  lower = "numeric",
                                  cutbin = "numeric",
                                  BSgenome = "BSgenomeORNULL"
                                  ))
##----------------------------------------------------------------------##
##                   "CoverageView" constructor
##----------------------------------------------------------------------##

BamView <- function(file,
                         seqname,
                         BSgenome = NULL,
                         geom = c("CoverageAndAlignment",
                               "Coverage","Alignment"),
                         rescale = c("geometry","transform" ,"none"),
                         viewname = "Coverage",
                         lower = 10L,
                         cutbin = 30L,
                         hint = FALSE,  #not implemented
                         ...){

  ## get params
  geom <- match.arg(geom)
  geom <- new("BamViewGeomSingleEnum", geom)

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
  
  pars <- GraphicPars(geom = geom, xlimZoom = xlimZoom, view = "BamView")
  mode <- IModeGroup(scaleMode = ScaleMode(zoomMode = "Horizontal"))
  obj <- BamView.gen$new(file = file, BSgenome = BSgenome, mode = mode,
                              rescale = rescale,
                              eventTrace = new("EventTrace"),
                              viewrange = viewrange,
                              pars = pars, lower = lower, cutbin = cutbin)

  obj$createView()
  obj$regSignal()
  obj
}


BamView.gen$methods(createView = function(){
  
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
  ## bam <- scanBam(file, param = ScanBamParam(which = GRanges(seqnames = seqname,
  ##                                             IRanges(1, seqlengths(viewrange))),
  ##                        what = c("pos", "qwidth")))
  ## bam <- bam[[1]]
  ## ir <- GRanges(seqnames=seqname,
  ##               ranges=IRanges(start=bam$pos, width=bam$qwidth))
  ## covg <- coverage(ir)
  ## ## load("~/Datas/rdas/covlst.rda")
  ## ## covg <- covlst[[seqname]]
  ## ir.v <- slice(covg,lower = lower)
  ## xpos <- viewWhichMaxs(ir.v)
  ## ypos <- viewMaxs(ir.v)
  ## take log?make transformation formal
  
  pars$ylim <<- expand_range(c(0, max(log(ypos+1))), mul = 0.05)
  ## pars$ylim <<- expand_range(c(0, max(ypos)), mul = 0.05)

  pfunCov <- function(layer,painter,exposed){
    pars$xlimZoomChanged$block()
    pars$xlimZoom <<- as.matrix(exposed)[,1]
    ## viewrange$ranges <<- pars$xlimZoom 
    if(!eventTrace$selfSignal){
      viewrange$rangesChanged$unblock()
      viewrange$ranges <<- IRanges(pars$xlimZoom[1] , pars$xlimZoom[2])
    }
    if(eventTrace$selfSignal){
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

  pfunAlign

  keyOutFun <- function(layer, event){
    eventTrace$focusin <<- FALSE
  }
  hoverEnterFun <- function(layer, event){
    eventTrace$focusin <<- TRUE
  }
  hoverLeaveFun <- function(layer, event){
    eventTrace$focusin <<- FALSE
  }
  
  rootLayer[0,0] <<- qlayer(scene, paintFun=pfunCov,
                            row=row,  col=col, rowSpan=rowSpan, colSpan=colSpan,
                            wheelFun = wheelEventZoom(),
                            keyPressFun = keyPressEventZoom(),
                            hoverEnterFun = hoverEnterFun,
                            focusOutFun = keyOutFun, hoverLeaveFun = hoverLeaveFun)

  rootLayer[1,0] <<- qlayer(scene, paintFun=pfunAlign,
                            row=row,  col=col, rowSpan=rowSpan, colSpan=colSpan,
                            wheelFun = wheelEventZoom(),
                            keyPressFun = keyPressEventZoom(),
                            hoverEnterFun = hoverEnterFun,
                            focusOutFun = keyOutFun, hoverLeaveFun = hoverLeaveFun)

  rootLayer[0,0]$setLimits(qrect(pars$xlim, pars$ylim))
  pars$ylimChanged$connect(function(){
    rootLayer[0,0]$setLimits(qrect(pars$xlim, pars$ylim))
  })

  rootLayer[0,0]$setLimits(qrect(pars$xlim, pars$ylim))
  pars$ylimChanged$connect(function(){
    rootLayer[0,0]$setLimits(qrect(pars$xlim, pars$ylim))
  })
})




## singals and paint utils
BamView.gen$methods(regSignal = function(){
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

BamView.gen$methods(paintCount = function(painter){
  print("Hello World")
})

BamView.gen$methods(paintCovSeg = function(painter, xpos, ypos){
  qdrawSegment(painter,xpos,0, xpos, log(ypos),stroke="gray80")
  ## qdrawSegment(painter,xpos,0, xpos, ypos,stroke="gray80")
})

BamView.gen$methods(paintCovRect = function(painter){
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
    qdrawRect(painter, x.pos,0, x.pos+1, log(cov.n+1),fill="gray80", stroke = NA)
  }})

BamView.gen$methods(paintCovMismatch = function(painter){
  if(is.null(BSgenome))
    stop("Please specify the associated BSgenome object
                if geom is set to mismatch")
  gr <- GRanges(seqnames = viewrange$seqnames,
                IRanges(pars$xlimZoom[1], pars$xlimZoom[2]))
  lgr <- pileupAsGRanges(file, gr)
  if(length(lgr)>0){
    lgr <- pileupGRangesAsVariantTable(lgr, BSgenome) #too slow...
    ## !!! need to be removed
    ## idx <- values(lgr)$count/values(lgr)$depth>0.05 & !values(lgr)$match
    idx <- values(lgr)$count/values(lgr)$depth>0.05 & !values(lgr)$match
    idx <- sample(which(idx), size = as.integer(sum(idx)*0.9), replace = FALSE)
    values(lgr)$count[idx] <- values(lgr)$count[idx]+as.integer(rnorm(1, mean = 10,
                                                                      sd = 5))
    values(lgr)$match[idx] <- TRUE

    ## group by match and mismatch
    ## idx <- values(lgr)$read != values(lgr)$ref
    ## lgr <- lgr[idx]
           idx <- order(start(lgr), values(lgr)$match, values(lgr)$read)
           ## assumption: on the same chromosome
           lgr <- lgr[idx]
           eds <- unlist(lapply(split(values(lgr)$count, start(lgr)), function(x){
             cumsum(x)
           }))
           sts <- unlist(lapply(split(values(lgr)$count, start(lgr)), function(x){
             N <- length(x)
             c(0,cumsum(x)[-N])
           }))
           ism <- unlist(lapply(split(values(lgr)$match, start(lgr)), function(x){
             x
           }))
           rds <- unlist(lapply(split(values(lgr)$read, start(lgr)), function(x){
             x
           }))
           ## assign color
           ## cols <- genLegend(lgr, color = "read")$color
           dnacol <- baseColor(IRanges:::safeExplode("ACTGN"))
           miscols <- unlist(lapply(rds, function(rd){
             dnacol[[rd]]
           }))
           miscols[ism] <- "gray80"

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
    pars$ylim <<- expand_range(range(log(eds + 1)), mul = 0.05)
    ## qdrawRect(painter, start(lgr), sts,
    ##           start(lgr)+1, eds,
    ##           stroke = NA, fill = miscols)
    qdrawRect(painter, start(lgr), log(sts+1),
              start(lgr)+1, log(eds+1),
              stroke = NA, fill = cols)

  }})

BamView.gen$methods(paintFragLength = function(painter){
  gr <- GRanges(seqnames = viewrange$seqnames,
                IRanges(pars$xlimZoom[1], pars$xlimZoom[2]))
  pspan <- pspanGR(file, gr)$pspan
  pspan <- subsetByOverlaps(ranges(pspan), ranges(gr), type = "within")
  ## draw point based gr
  ## THINK: MAKE IT GENERAL!
  x <- (start(pspan)+end(pspan))/2
  y <- width(pspan)
  cir <- qglyphCircle(r = 1)
  qdrawGlyph(painter, cir, x, y, stroke = NULL, fill = "black")
  ## qdrawCircle(painter, x, y, r = 1, stroke = NULL, fill = "black")
  ## fixed or not?
  if(TRUE)
    pars$ylim <<- expand_range(c(min(y), max(y)), mul = 0.05)
})
