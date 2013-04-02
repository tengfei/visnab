##----------------------------------------------------------------------------##
##                    For class "CoverageView"
##----------------------------------------------------------------------------##
## coverage/alignment, separate model?
##  make properties
gparslst <- list(view = "character",
                 lower = "numeric",
                 binNum = "PositiveInteger",
                 zoomLevel.cur = "numeric",
                 zoomLevel = "numeric",
                 geom = .GeomName("CoverageView"))
setPars("CoverageView", gparslst, methods = list(
                                    widget = function(visible = "binNum"){
                                      ControlPanel(.self, visible = visible)
                                    }
                                    ))

CoverageView.gen <- setRefClass("CoverageView",
                                contains = c("QtVisnabView", "LinearView"),
                                fields=list(
                                  track = "list",
                                  file = "character",
                                  BSgenome = "BSgenomeORNULL",
                                  gr.v = "GRanges",
                                  res = "data.frame",
                                  cached = "logical",
                                  cachedSeqnames = "character"
                                  ))



##----------------------------------------------------------------------##
##                   "CoverageView" constructor
##----------------------------------------------------------------------##

CoverageView <- function(file,
                         seqname,
                         BSgenome = NULL,
                         geom = c("total","mismatch","pairend","elength"),
                         rescale = c("none", "geometry","transform"),
                         viewname = "Coverage",
                         lower = 10L,
                         binNum = 50L, 
                         hint = FALSE,  #not implemented
                         ...){


  ## get params
  geom <- match.arg(geom)
  geom <- new("CoverageViewGeomSingleEnum", geom)

  rescale <- match.arg(rescale)
  rescale <- new("RescaleSingleEnum", rescale)

  tooltips <- "not implemented"

  hd <- scanBamHeader(file)
  .levels <- unique(names(hd[[1]]$targets))
  if(missing(seqname))
    seqname <- sort(names(hd[[1]]$targets))[1]

  seqlength <- hd[[1]]$targets

  xlimZoom <- c(1, seqlength[seqname])
  viewrange <- MutableGRanges(factor(seqname, levels = sort(.levels)),
                              IRanges(1, seqlength[seqname]),
                              seqlengths = seqlength)
  ## seqlengths(viewrange) <- seqlength
  ## viewrange$seqinfo@seqlengths <- as.integer(seqlength)
  pars <- GraphicPars(geom = geom,
                      lower = lower, binNum  = PositiveInteger(binNum), 
                      view = "CoverageView")

  md <- IModeGroupWidget(scaleMode = ScaleMode(zoomMode = "Horizontal"))
  obj <- CoverageView.gen$new(file = file,
                              xlimZoom = xlimZoom,
                              BSgenome = BSgenome, mode = md,
                              rescale = rescale,
                              eventTrace = new("EventTrace"),
                              viewrange = viewrange,
                              pars = pars,
                              cached = FALSE
                              )

  obj$createView()
  obj$regSignal()
  obj
}

##---------------------------------------------------------##
##             print method
##---------------------------------------------------------##

CoverageView.gen$methods(createView = function(){
  ## TODO: for all??
  cachedSeqnames <<- seqname <- as.character(viewrange$seqnames)
  setDislayWidgets()
  setBgColor()
  hd <- scanBamHeader(file)
    xlim <<- c(1, seqlengths(viewrange)[seqname])
  pars$zoomLevel <<- c(1e6, 1e5,0)
  pars$zoomLevel.cur <<- 1
  ## if(width(viewrange) > pars$zoomLevel[1]){
    ## FIXME: make it flexible
    message("Scanning bam files ...")
    bam <- scanBam(file, param = ScanBamParam(which = GRanges(seqnames = seqname,
                                                IRanges(1,   seqlengths(viewrange)[seqname])),
                           what = c("pos", "qwidth")))
    bam <- bam[[1]]
    message("Parsing coverage")  
    ir <- GRanges(seqnames=seqname,
                  ranges=IRanges(start=bam$pos, width=bam$qwidth))
    covg <- coverage(ir)[[1]]
    ## load("~/Datas/rdas/covlst.rda")
    ## covg <- covlst[[seqname]]
    ir.v <- slice(covg, lower = 10)
    xpos <- viewWhichMaxs(ir.v)
    ypos <- viewMaxs(ir.v)
    ## take log?make transformation formal
    ylim <<- expand_range(c(0, max(log(ypos))), mul = 0.05)
  ## }else{
  ##   print("FALSE")
  ##   .diff <- width(viewrange)
  ##   sts <- seq(start(viewrange)-.diff, end(viewrange) + .diff, length.out = pars$binNum)
  ##   gr.v <<- GRanges(viewrange$seqnames@values,
  ##                    IRanges(start = sts, width = diff(sts)[1]))
  ##   res <<- countBam(file, param = ScanBamParam(which = gr.v))
  ##   ylim <<- expand_range(c(0, log(max(res$record +1))), mul = 0.05)
  ## }
  ## ylim <<- expand_range(c(0, max(ypos)), mul = 0.05)
  ## ylim <<- expand_range(c(0, max(ypos)), mul = 0.05)
  gr.v <<- GRanges(viewrange$seqnames@values, IRanges(start = 1, width = 1)  )
  pfunCov <- function(layer,painter,exposed){
    xlimZoomChanged$block()
    xlimZoom <<- as.matrix(exposed)[,1]
    ## viewrange$ranges <<- xlimZoom 
    if(!eventTrace$selfSignal){
      ## viewrange$changed$connect(function)
      viewrange$changed$unblock()
      viewrange$ranges <<- IRanges(xlimZoom[1] , xlimZoom[2])
    }
    if(eventTrace$selfSignal){
      viewrange$changed$block()
      viewrange$ranges <<- IRanges(xlimZoom[1] , xlimZoom[2])
    }
    xlimZoomChanged$unblock()
    xlimZoom <<- as.matrix(exposed)[,1]
    ##======================================================================
    ## geom == "total"
    ##======================================================================
    if(pars$geom == "total"){
      ## level 1
      if(diff(xlimZoom)>pars$zoomLevel[1]){
        .self$paintCovSeg(painter, xpos, ypos)
      }
      if(diff(xlimZoom)<=pars$zoomLevel[1] &
         diff(xlimZoom)>pars$zoomLevel[2]){
        if(pars$zoomLevel.cur != 2){
          .diff <- diff(xlimZoom)
            message("computing bam count...")
            sts <- seq(xlimZoom[1]-.diff, xlimZoom[2] + .diff, length.out = pars$binNum)
          gr.v <<- GRanges(viewrange$seqnames@values,
                           IRanges(start = sts, width = diff(sts)[1]))
            res <<- countBam(file, param = ScanBamParam(which = gr.v))
        }else{
          .diff <- diff(xlimZoom)
          if(xlimZoom[2] >= max(end(gr.v)) | xlimZoom[1] <= min(start(gr.v))){
            message("clear cache... recomputing bam count...")
            sts <- seq(xlimZoom[1]-.diff, xlimZoom[2] + .diff, length.out = pars$binNum)
            gr.v <<- GRanges(viewrange$seqnames@values,
                                           IRanges(start = sts, width = diff(sts)[1]))
            ## gr.v <<- GRanges(viewrange$seqnames@values, IRanges(start = sts, width = diff(sts)[1]))
            res <<- countBam(file, param = ScanBamParam(which = gr.v))
          }
        }
        .self$paintCovRect(painter, res)
         pars$zoomLevel.cur <<- 2        
      }
      if(diff(xlimZoom)<=pars$zoomLevel[2] &
         diff(xlimZoom)>pars$zoomLevel[3]){
        if(pars$zoomLevel.cur != 3){
          .diff <- diff(xlimZoom)
            message("computing bam count...")
            sts <- seq(xlimZoom[1]-.diff, xlimZoom[2] + .diff, length.out = pars$binNum)
            gr.v <<- GRanges(viewrange$seqnames@values,
                                           IRanges(start = sts, width = diff(sts)[1]))

            ## gr.v <<- GRanges(viewrange$seqnames@values, IRanges(start = sts, width = diff(sts)[1]))
            res <<- countBam(file, param = ScanBamParam(which = gr.v))
        }else{
          .diff <- diff(xlimZoom)
          if(xlimZoom[2] >= max(end(gr.v)) | xlimZoom[1] <= min(start(gr.v))){
            message("clear cache... recomputing bam count...")
            sts <- seq(xlimZoom[1]-.diff, xlimZoom[2] + .diff, length.out = pars$binNum)
            gr.v <<- GRanges(viewrange$seqnames@values, IRanges(start = sts, width = diff(sts)[1]))
            res <<- countBam(file, param = ScanBamParam(which = gr.v))
          }
        }
        .self$paintCovRect(painter, res)
         pars$zoomLevel.cur <<- 3        
      }
    }
    ##======================================================================
    ## geom == "mismatch"
    ##======================================================================
    if(pars$geom == "mismatch"){
      ## level 1
      if(diff(xlimZoom)>pars$zoomLevel[1]){
        .self$paintCovSeg(painter, xpos, ypos)
      }
      if(diff(xlimZoom)<=pars$zoomLevel[1] &
         diff(xlimZoom)>pars$zoomLevel[2]){
        .self$paintCovRect(painter)
      }
      if(diff(xlimZoom)<=pars$zoomLevel[2] & diff(xlimZoom)>pars$zoomLevel[3]){
        ## this should require loading or associated BSgenome
        ## And support sequence logo(maybe seperate painter)
        .self$paintCovMismatch(painter)
      }}
    if(pars$geom == "elength"){
      if(diff(xlimZoom)> 1e5){
        .self$paintCovSeg(painter, xpos, ypos)
      }
      if(diff(xlimZoom)<= 1e5 & diff(xlimZoom)>pars$zoomLevel[3]){
        .self$paintFragLength(painter)
      }
    }
  }

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
  
  rootLayer[0,0]$setLimits(qrect(xlim, ylim))
  ylimChanged$connect(function(){
    rootLayer[0,0]$setLimits(qrect(xlim, ylim))
  })
})


CoverageView.gen$methods(show = function(){
  view$show()
})

setMethod("print","CoverageView",function(x,..){
  x$show()
})

CoverageView.gen$methods(regSignal = function(){
  pars$binNumChanged$connect(function(){
    qupdate(scene)
  })  
  xlimZoomChanged$connect(function(){
    zoom_factor <- diff(xlimZoom)/seqlengths(viewrange)[cachedSeqnames]
    ## then scale view
    view$resetTransform()
    view$scale(1/zoom_factor, 1)
    ## then center viewr
    pos.x <- mean(xlimZoom)
    pos.y <- mean(ylim)
    pos.scene <- as.numeric(rootLayer[0,0]$mapToScene(pos.x, pos.y))
    view$centerOn(pos.scene[1], pos.scene[2])
    viewrange$ranges <<- IRanges(xlimZoom[1] , xlimZoom[2])
  })
  pars$geomChanged$connect(function(){
    qupdate(scene)
  })
  viewrange$changed$connect(function(change){
    if(rangeChanged(change)){
      if(as.character(viewrange$seqnames) != cachedSeqnames){
        cachedSeqnames <<- as.character(viewrange$seqnames)
        ## hd <- scanBamHeader(file)
        rootLayer[0,0]$close()
        view$resetTransform()
        createView()
        regSignal()
    }
  }
  })
  theme$bgColorChanged$connect(function(){
    bgcol <- pars$bgColor
    bgalpha <- pars$alpha
    qcol <- col2qcol(bgcol,bgalpha)
    scene$setBackgroundBrush(qbrush(qcol))
  })
})

CoverageView.gen$methods(paintCovSeg = function(painter, xpos, ypos){
  qdrawSegment(painter,xpos,0, xpos, log(ypos),stroke="gray40")
})

CoverageView.gen$methods(paintCovCount = function(painter, xpos1, xpos2, ypos){
      qdrawRect(painter, xpos1, 0, xpos2, ypos, stroke = NA, fill = "gray40")

})

CoverageView.gen$methods(paintCovRect = function(painter, res){
    qdrawRect(painter, res$start, 0, res$end, log(res$records + 1),fill="gray40", stroke = "white")
})

CoverageView.gen$methods(paintCovMismatch = function(painter){
  if(is.null(BSgenome))
    stop("Please specify the associated BSgenome object
                if geom is set to mismatch")
  gr <- GRanges(seqnames = viewrange$seqnames,
                IRanges(xlimZoom[1], xlimZoom[2]))
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
           miscols[ism] <- "gray40"


    dnacol <- baseColor(IRanges:::safeExplode("ACTGN"))
    cols <- unlist(lapply(rds, function(rd){
      dnacol[[rd]]
    }))
    cols[ism] <- "gray40"
    ylim <<- expand_range(range(log(eds + 1)), mul = 0.05)
    ## qdrawRect(painter, start(lgr), sts,
    ##           start(lgr)+1, eds,
    ##           stroke = NA, fill = miscols)
    qdrawRect(painter, start(lgr), log(sts+1),
              start(lgr)+1, log(eds+1),
              stroke = NA, fill = cols)

  }})

CoverageView.gen$methods(paintFragLength = function(painter){
  gr <- GRanges(seqnames = viewrange$seqnames,
                IRanges(xlimZoom[1], xlimZoom[2]))
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
    ylim  <<- expand_range(c(min(y), max(y)), mul = 0.05)
})
