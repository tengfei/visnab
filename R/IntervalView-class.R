## TODO:
## 1. need a plot for IRanges which share the core code
## 2. supported geom "midpoint", "rectangle", "line",
## "interval" for IRanges, "heatmap" and "barchart"
##----------------------------------------------------------##
##             For class "IntervalView"
##----------------------------------------------------------##
IntervalView.gen <- setRefClass("IntervalView",
                                contains = "QtVisnabView",
                                fields = c(track = "SimpleMutableGRanges",
                                  flag = "logical",
                                  signalingField("group", "character")))



##----------------------------------------------------------##
##             "IntervalView" constructor
##----------------------------------------------------------##
IntervalView <- function(track,
                         seqname,
                         group,
                         color,
                         viewname = "Interval Data",
                         geom = c("full", "reduce", "midpoint", "length",
                           "barchart", "heatmap", "segment"),
                         rescale = c("geometry", "transform", "none")){
  
  ##
  tooltips <- "not implemented"
  ## if null, set first chromosome as viewed chromosome
  if(missing(seqname))
    seqname <- as.character(unique(as.character(seqnames(track)))[1])

  if(missing(color))
    color <- I("red")

  start <- 0
  end <- max(end(ranges(track[seqnames(track)==seqname])))
  seqlength <- end
  xlimZoom <- c(start,end)

  geom <- match.arg(geom)
  geom <- new("IntervalViewGeomSingleEnum", geom)

  rescale <- match.arg(rescale)
  rescale <- new("RescaleSingleEnum", rescale)

  viewrange <- MutableGRanges(seqname, IRanges(start, end))
  seqlengths(viewrange) <- end


  if(is(track, "GRanges"))
    track <- as(track,"MutableGRanges")
  ## need to make sure it's a long form
  ## FIXME:
  track <- addLevels(track)
  ## connect signal
  ## FIXME:
  track$elementMetadataChanged$connect(function() {
    qupdate(scene)
  })
  
  pars <- GraphicPars(xlimZoom = xlimZoom, 
                      geom = geom, color = color,
                      view = "IntervalView")

  obj <- IntervalView.gen$new(track=track,pars=pars, rescale = rescale,
                              tooltipinfo = tooltips, viewname = viewname,
                              group = group,
                              viewrange = viewrange)
  ## event
  ## add default attributes
  addAttr(obj$track,.color=obj$pars$fill,.hover=FALSE,.brushed=FALSE)
  ## obj$regSignal()
  obj$createView()
  return(obj)
}

############################################################
## createview method
############################################################
IntervalView.gen$methods(createView = function(){
  seqname <- as.character(seqnames(viewrange))
  setDislayWidgets()
  setBgColor()

  start <- min(start(ranges(track[seqnames(track)==seqname])))
  end <- max(end(ranges(track[seqnames(track)==seqname])))
  seqlength <- end
  mr <- track[seqnames(track)==seqname]
  pars$xlim <<- expand_range(c(start, end), mul = 0.05)
  pars$ylim <<- expand_range(c(0, 10), mul = 0.05)
  ## idx <- findOverlaps(IRanges(start=start,end=end),
  ##                     ranges(mr))@matchMatrix[,2]
  ## mr <- mr[idx]
  ## return a painter function
  ## lvpainter <- IntervalPainter(mr, obj = .self,      geom = pars$geom)
  ## if(sing(geom)&!(missing(obj)))
  ##   geom <- pars$geom
  ## if(missing(geom)&missing(obj))
  ##   geom <- "full"
  ## if(length(color))
    color <- pars$color
  ## assigne color
  if(length(color)){
    vals <- values(mr)[,color]
    if(length(vals)>100)
      vallen <- 100
    testpal <- function(to = c(0, 1), vallen = vallen){
      function(x){
        vals <- x
        to = c(0, 1)
        vals <- rescale_mid(vals, to = to, mid = 0)
        ## cols <- cscale(vals, div_gradient_pal("blue", "white", "red"))
        bks <- seq(0, 1, length = vallen)
        ints <- as.character(cut(vals, bks))
        lvs <- as.list(by(vals, ints, mean))
        vls <- unname(unlist(lvs))
        cols <- cscale(vls, div_gradient_pal("blue", "white", "red"))
        colss <- cols[match(ints, names(lvs))]
      }
    }
    cols <- cscale(vals, testpal(vallen = vallen))
    values(mr)$.color <- cols
  }
                                        # Can use with gradient_n to create a continous gradient

  ## if(!missing(group)){
  ##   vs <- eval(as.symbol(group), elementMetadata(gr))
  ##   grl <- split(gr, vs)
  ##   group <- as.symbol(group)
  ## }else{
  ##   group <- NULL
  ##   grl <- list(gr)
  ## }

  ## ## not used for every case
  ## ts <- lapply(grl, function(gr){
  ##   mr <- gr
  ##   ir <- ranges(gr)
  ##   bins <- disjointBins(ir)
  ##   binmx <- max(bins*10+5)
  ##   mr.r <- reduce(mr)
  ##   sts <- start(gr)
  ##   wds <- width(gr)
  ##   ptx <- sts+wds/2
  ##   cov <- coverage(gr)
  ##   list(bins = bins, binmx = binmx,  mr = mr,
  ##        mr.r = mr.r, sts = sts, wds = wds, ptx = ptx, cov = cov)
  ## })
  ## mrl <- split(mr, values(mr)[,group])
  ## mrl <- lapply(mrl, function(mr){
  ##   st <- start(mr)
  ##   ed <- end(mr)
  ##   col <- values(mr)$.color
  ##   data.frame(st = st, ed = ed, col = col)
  ## })
  lv <- as.numeric(as.factor(values(mr)[,group]))
  st <- start(mr)
  ed <- st
  col <- values(mr)$.color
  hoverMoveEvent <- function(layer,event){
    rect <- qrect(0,0,1,1)
    mat <- layer$deviceTransform(event)$inverted()
    rect <- mat$mapRect(rect)
    pos <- event$pos()
    rect$moveCenter(pos)
    hits <- layer$locate(rect)+1
    if(length(hits)>=1){
      posS <- event$screenPos()
      hits <- hits[1]
      values(track)$.color[hits] <<- pars$hoverColor
      ## Qt$QToolTip$showText(posS,getTooltipInfo(mr,hits))
      flag <<- TRUE
    }else{
      if(flag){
        values(track)$.color <<- pars$fill
        flag <<- FALSE
      }
    }
  }
  lvpainter <- function(layer,painter,exposed){
    xlimZoom <- as.matrix(exposed)[,1]
    ylimZoom <- as.matrix(exposed)[,2]
    pars$xlimZoom <<- xlimZoom
    pars$ylimZoom <<- ylimZoom
    ## compute color on the fly
    ## Draw rectangle
    ##----------------------------------------------------------------------
    ##  pars$geom "full"
    ##----------------------------------------------------------------------
    if(pars$geom == "full"){
      ## values(mr)$.color <- cols
      gps <- names(ts)
      if(!is.null(group)){
        cols <- genLegend(gr, color = color)$colorLegend$colors
        if(length(cols)==1)
          cols <- rep(cols, length(ts))
        lapply(1:length(ts), function(i){
          gs <- 0+5*(i-1)+2
          with(ts[[i]],{
            gp <- gps[i]
            col <- cols[i]
            qdrawRect(painter,start(mr),gs+(bins*10)/binmx*5,end(mr),
                      gs+(bins*10+5)/binmx*5,
                      stroke=NA,fill=col)
            qdrawRect(painter, 0, gs-2, pars$seqlength, gs+5-2,
                      fill = NA, stroke = "gray80")
          })
        })
        pars$ylim <<- c(0,5*(length(ts)))
      }else{
        cols <- genLegend(gr, color = color)$color
        with(ts[[1]], 
             qdrawRect(painter,start(mr),(bins*10)/binmx*5,end(mr),
                       (bins*10+5)/binmx*5,
                       stroke=NA,fill=cols))
        pars$ylim <<- expand_range(c(0, 5), mul = 0.05)
      }
    }
    if(pars$geom == "dense"){
      qdrawRect(painter,start(mr.r), 10, end(mr.r), 20,
                stroke=NA,fill=values(mr.r)$.color)
      pars$ylim <<- c(0,30)
    }
    if(pars$geom == "segment"){
      qdrawSegment(painter, ptx, 10, ptx, 20, stroke = values(mr)$.color)
      pars$ylim <<- c(0,30)
    }
    if(pars$geom == "barchart"){
      ## don't need gap in barchart
      ## single position first
      ## Ignore multiple sample or facets first
      if(!is.null(fill)){
        idx <- order(start(ts[[1]]$mr), values(ts[[1]]$mr)[,fill])
        ts[[1]]$mr <- ts[[1]]$mr[idx]
        ts[[1]]$bins <- ts[[1]]$bins[idx]
        cols <- genLegend(ts[[1]]$mr, color = fill)$color
      }else{
        cols <- "black"
      }
      with(ts[[1]],{
        qdrawRect(painter,start(mr),((bins-1)*10)/binmx*5,end(mr)+1,
                  ((bins-1)*10+10)/binmx*5,
                  stroke=NA, fill = cols)}
           )
      pars$ylim <<- expand_range(c(0, 5), mul = 0.05)
    }
    if(pars$geom == "mismatch"){
      ## suppose we have "read" and "ref" column
      ## long form
      ## read: color(mismatched)
      ## ref: color
      ## read: gray80(matched)
      ## support: seqlogo(zoom level)
      idx <- order(start(ts[[1]]$mr), values(ts[[1]]$mr)[,fill])
      ts[[1]]$mr <- ts[[1]]$mr[idx]
      ts[[1]]$bins <- ts[[1]]$bins[idx]
      idx <- values(ts[[1]]$mr)$read != values(ts[[1]]$mr)$ref
      if(diff(xlimZoom)/600>1){      
        cols <- rep("gray80", length(idx))
        cols.mis <- genLegend(ts[[1]]$mr[idx], color = "read")$color
        cols[idx] <- cols.mis
        with(ts[[1]],{
          qdrawRect(painter,start(mr),((bins-1)*10)/binmx*5,end(mr)+1,
                    ((bins-1)*10+10)/binmx*5,
                    stroke=NA, fill = cols)}
             )
        pars$ylim <<- expand_range(c(0, 5), mul = 0.05)
      }
      if(diff(xlimZoom)/600<1){
        ## show all the reference first
        
        ## show seqlogo
        ## now the short form works better here
        ## use sumlong
        cols <- genLegend(sumlong, color = "read")$color
        sumlong <- addLevels(sumlong)
        print(head(as.character(values(sumlong)$read)))
        qdrawText(painter, as.character(values(sumlong)$read),
                  start(sumlong), (values(sumlong)$.level-1)*10,
                  halign = "center", valign = "bottom",
                  vcex = values(sumlong)$count)
        pars$ylim <<- expand_range(c(0, max(values(sumlong)$.level-1)), mul = 0.05)
      }      

    }
    if(pars$geom == "seqlogo"){
      
    }
    if(pars$geom == "heatmap"){
      segs <- qglyphSegment(x = 30, dir = pi/2)
      ## qdrawSegment(painter, st, 50*(lv-1), ed, 50*lv, 
      ##                stroke = col)
      qdrawGlyph(painter, segs, st, 10*(lv-1), stroke = col)
      pars$ylim <<- c(-10, 10*max(lv))
    }
    if(pars$geom == "line"){
      
    }
  }
  ## used for hover
  flag <<- FALSE
  ## construct layer
  rootLayer[0,0] <<- qlayer(scene,paintFun=lvpainter,
                            wheelFun=  wheelEventZoom(view),
                            keyPressFun = keyPressEventZoom(track, view = view, sy = 1),
                            cache = FALSE)
  ## hoverMoveFun = hoverMoveEvent,


  rootLayer[0,0]$setLimits(qrect(pars$xlim[1], pars$ylim[1],
                                 pars$xlim[2], pars$ylim[2]))
  pars$ylimChanged$connect(function(){
    rootLayer[0,0]$setLimits(qrect(pars$xlim[1], pars$ylim[1],
                                   pars$xlim[2], pars$ylim[2]))
  })
  ## rootLayer[0,0]$layer$setGeometry(0,0,600,150)
})


IntervalView.gen$methods(show = function(){
  view$show()
})

setMethod("print","IntervalView",function(x){
  x$show()
})



