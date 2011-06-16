##----------------------------------------------------------------------------##
##                    For class "CoverageView"
##----------------------------------------------------------------------------##
CoverageView.gen <- setRefClass("CoverageView",contains = "QtVisnabView",
                                fields=list(track = "list",
                                  lower = "numeric",
                                  cutbin = "numeric",
                                  file = "character",
                                  BSgenome = "BSgenome"
                                  ))

##----------------------------------------------------------------------##
##                   "CoverageView" constructor
##----------------------------------------------------------------------##

CoverageView <- function(file,
                         BSgenome,
                         seqname,
                         scene,
                         view,
                         rootLayer,
                         thisLayer,     #do we need this one?
                         row = 0L,
                         col = 0L,
                         rowSpan = 1L,
                         colSpan = 1L,
                         lower = 10L,
                         cutbin = 20L,
                         geom = c("total","mismatch"),
                         rescale = c("none", "geometry","transform"),
                         ...){

  ## get params
  geom <- match.arg(geom)
  rescale <- match.arg(rescale)
  
  hd <- scanBamHeader(file)
  
  if(missing(seqname))
    seqname <- sort(names(hd[[1]]$targets))[1]
  seqlength <- hd[[1]]$targets[seqname]
  
  pars <- GraphicPars(seqname = seqname, geom = geom,
                      seqlength = seqlength,
                      view = "AlignmentView")

  obj <- CoverageView.gen$new(file = file, focusin = FALSE,
                              row = row, col = col, selfSignal = FALSE,
                              rowSpan = rowSpan, colSpan = colSpan,
                              scene = scene, view = view,rootLayer = rootLayer,
                              thisLayer = thisLayer, outputRange = c(0, seqlength),
                              pars = pars, lower = lower, cutbin = cutbin)

  obj$createView(rescale = rescale)
  obj$regSignal()
  obj
}


##---------------------------------------------------------##
##             print method
##---------------------------------------------------------##

CoverageView.gen$methods(createView = function(seqname,
                           rescale = "geometry"){
  if(missing(scene)){
    scene <<- qscene()
    view <<- qplotView(scene,rescale = rescale)
    view$setDragMode(Qt$QGraphicsView$ScrollHandDrag)
    rootLayer <<- qlayer(scene,geometry=qrect(0,0,800,600))
  }
  
  if(missing(seqname))
    pars$seqname <<- seqname
  seqname <- pars$seqname

  bgcol <- pars$bgColor
  bgalpha <- pars$alpha
  qcol <- col2qcol(bgcol,bgalpha)
  scene$setBackgroundBrush(qbrush(qcol))

  hd <- scanBamHeader(file)
  seqname <- sort(names(hd[[1]]$targets))[1]
  pars$seqlength <<- hd[[1]]$targets[seqname]
  pars$xlim <<- c(0, pars$seqlength)
  pars$xlimZoom <<- c(0, pars$seqlength)

  ## preset the level
  zoomLevel <- c(1e5, 1000,0)
  ## precessing data
  ## gr <- GRanges(seqnames = seqname, IRanges(0, pars$seqlength))
  ## ##
  ## message("Loading bam file for chrom ", seqname)
  ## bam <- scanBam(file, param = ScanBamParam(which = gr))
  ## bam <- bam[[1]]
  ## ir <- GRanges(seqnames=seqname,
  ##               ranges=IRanges(start=bam$pos, width=bam$qwidth),
  ##               strand=bam$strand)

  ## covg <- coverage(ranges(ir))
  ## for temporary
  ## FIXME: make it flexible
  load("~/Datas/rdas/covlst.rda")
  covg <- covlst[[seqname]]
  ir.v <- slice(covg,lower=20)
  xpos <- viewWhichMaxs(ir.v)
  ypos <- viewMaxs(ir.v)
  pars$ylim <<- c(0, max(log(ypos))+2)

  pfunCov <- function(layer,painter,exposed){
    pars$xlimZoomChanged$block()
    pars$xlimZoom <<- as.matrix(exposed)[,1]
    ## outputRange <<- pars$xlimZoom 
    if(!selfSignal){
      outputRangeChanged$unblock()
      outputRange <<- pars$xlimZoom 
    }
    if(selfSignal){
      outputRangeChanged$block()
      outputRange <<- pars$xlimZoom 
    }
    
    pars$xlimZoomChanged$unblock()
    xlimZoom <- as.matrix(exposed)[,1]

    if(diff(xlimZoom)>zoomLevel[1]){
      qdrawSegment(painter,xpos,0, xpos, log(ypos),stroke="gray50")
    }
    
    if(diff(xlimZoom)<=zoomLevel[1] &
       diff(xlimZoom)>zoomLevel[2]){
      gr <- GRanges(seqnames = seqname, IRanges(xlimZoom[1], xlimZoom[2]))
      bam <- scanBam(file, param=ScanBamParam(which = gr,
                             what=c("pos", "qwidth", "strand")))
      bam <- bam[[1]]
      ir <- IRanges(start=bam$pos,width=bam$qwidth)
      if(length(ir)>0){
        gr <- GRanges(seqnames = seqname, ir,
                      strand = bam$strand)
        cov <- coverage(ir, shift = -xlimZoom[1])
        cov.n <- as.numeric(cov)
        covlen <- length(cov.n)
        x.pos <- xlimZoom[1]:(xlimZoom[1]+covlen-1)
        qdrawSegment(painter, x.pos,0, x.pos, log(cov.n+1),stroke="gray50")
      }}
    ##======================================================================
    ## zoom level 3: show mismatch
    ##======================================================================
    if(diff(xlimZoom)<=zoomLevel[2] &
       diff(xlimZoom)>zoomLevel[3]){
      ## geom total, doesn't require a reference genome data(like BSGenome)
      if(pars$geom == "total"){
      gr <- GRanges(seqnames = seqname, IRanges(xlimZoom[1], xlimZoom[2]))
      bam <- scanBam(file, param=ScanBamParam(which = gr,
                             what=c("pos", "qwidth", "strand")))
      bam <- bam[[1]]
      ir <- IRanges(start=bam$pos,width=bam$qwidth)
      if(length(ir)>0){
        gr <- GRanges(seqnames = seqname, ir,
                      strand = bam$strand)
        cov <- coverage(ir, shift = -xlimZoom[1])
        cov.n <- as.numeric(cov)
        covlen <- length(cov.n)
        x.pos <- xlimZoom[1]:(xlimZoom[1]+covlen-1)
        qdrawPolygon(painter, c(x.pos[1],x.pos,tail(x.pos)[1]),
                     c(0,log(cov.n+1),0),
                     stroke=NA, fill="gray50")
      }}
      
    if(pars$geom == "mismatch"){
      ## this should require loading or associated BSgenome
      ## And support sequence logo(maybe seperate painter)
      if(missing(BSgenome))
        stop("Please specify the associated BSgenome object if geom is set to mismatch")
      gr <- GRanges(seqnames = seqname, IRanges(xlimZoom[1], xlimZoom[2]))
      lgr <- pileupAsGRanges(file, gr)
      lgr <- pileupGRangesAsVariantTable(lgr, BSgenome)
      ## group by match and mismatch
      values(lgr)$isMatch <- values(lgr)$read == values(lgr)$ref
      ## make sure the mismatched color is down below the matched color
      ## change order first
      idx <- order(start(lgr), values(lgr)$isMatch, values(lgr)$read)
      lgr <- lgr[idx]
      ## assign color
      idx.m <- values(lgr)$isMatch
      cols <- genLegend(lgr, color = "read")$color
      cols[idx.m] <- "gray80"
      values(lgr)$.color <- cols
      ## assign level
      print(system.time(lgr <- addLevels(lgr)))
      ## painting...
      lvmx <- max(values(lgr)$.level)
      lvs <- values(lgr)$.level
      qdrawRect(painter, start(lgr), (lvs*10)/(lvmx*10+5)*5,
                start(lgr)+1, (lvs*10+5)/(lvmx*10+5)*5,
                stroke = NA, fill = values(lgr)$.color)
      pars$ylim <<- c(0, 5)
    }}
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
  
  thisLayer <<- qlayer(rootLayer, paintFun=pfunCov,
                       row=row,  col=col, rowSpan=rowSpan, colSpan=colSpan,
                       wheelFun = wheelEventZoom(view),
                       keyPressFun = keyPressEventZoom(.self, view, sy = 1,
                         focusin = focusin),
                       hoverEnterFun = hoverEnterFun,
                       focusOutFun = keyOutFun, hoverLeaveFun = hoverLeaveFun)
  
  thisLayer$setLimits(qrect(pars$xlim, pars$ylim))
  
  pars$ylimChanged$connect(function(){
    thisLayer$setLimits(qrect(pars$xlim, pars$ylim))
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
    zoom_factor <- diff(pars$xlimZoom)/pars$seqlength
    ## then scale view
    view$resetTransform()
    view$scale(1/zoom_factor, 1)
    ## then center viewr
    pos.x <- mean(pars$xlimZoom)
    pos.y <- mean(pars$ylim)
    pos.scene <- as.numeric(thisLayer$mapToScene(pos.x, pos.y))
    view$centerOn(pos.scene[1], pos.scene[2])
    outputRange <<- pars$xlimZoom 
  })
  pars$geomChanged$connect(function(){
    qupdate(scene)
  })
  pars$seqnameChanged$connect(function(){
    hd <- scanBamHeader(file)
    pars$seqlength <<- hd[[1]]$targets[pars$seqname]
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

