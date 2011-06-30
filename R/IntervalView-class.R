## TODO:
## 1. need a plot for IRanges which share the core code
## 2. supported geom "midpoint", "rectangle", "line",
## "interval" for IRanges, "heatmap" and "barchart"
##----------------------------------------------------------##
##             For class "IntervalView"
##----------------------------------------------------------##
IntervalView.gen <- setRefClass("IntervalView",
                                contains = "QtVisnabView",
                                fields = list(track = "MutableGRanges",
                                  flag = "logical"))


##----------------------------------------------------------##
##             "IntervalView" constructor
##----------------------------------------------------------##

IntervalView <- function(track,
                         seqname,
                         group,
                         color,
                         viewname = "Interval Data",
                         geom = c("full", "reduce", "midpoint",
                           "length", "barchart", "histogram",
                           "segment"),
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
  seqlength <<- end
  xlimZoom <- c(start,end)

  geom <- match.arg(geom)
  geom <- new("IntervalViewGeomEnum", geom)

  rescale <- match.arg(rescale)
  rescale <- new("RescaleEnum", rescale)

  if(is(track, "GRanges"))
    track <- as(track,"MutableGRanges")
  ## need to make sure it's a long form
  track <- addLevels(track)
  ## connect signal
  track$elementMetadataChanged$connect(function() {
    qupdate(scene)
  })
  
  pars <- GraphicPars(xlimZoom = xlimZoom, seqname = seqname,
                      geom = geom[1], color = color, seqlength = seqlength,
                      view = "IntervalView")
  
  obj <- IntervalView.gen$new(track=track,pars=pars)

  ## event
  ## add default attributes
  addAttr(obj$track,.color=obj$pars$fill,.hover=FALSE,.brushed=FALSE)
  ## obj$regSignal()
  obj$createView(group = group)
  return(obj)
}

############################################################
## createview method
############################################################
IntervalView.gen$methods(createView = function(group, fill){

  seqname <- seqnames(seqinfo)
  setDislayWidgets(.self)
  setBgColor(.self)

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
  lvpainter <- IntervalPainter(mr, obj = .self, group = group, fill = fill,
                               geom = obj$pars$geom)
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
  ## used for hover
  flag <<- FALSE
  ## construct layer
  layer <- qlayer(rootLayer,paintFun=lvpainter,
                  wheelFun=  wheelEventZoom(view),
                  keyPressFun = keyPressEventZoom(track, view = view, sy = 1),
                  ## hoverMoveFun = hoverMoveEvent,
                  row=row,col=col,
                  rowSpan=rowSpan,colSpan=colSpan)
  layer$setLimits(qrect(pars$xlim[1], pars$ylim[1], pars$xlim[2], pars$ylim[2]))
  pars$ylimChanged$connect(function(){
    layer$setLimits(qrect(pars$xlim[1], pars$ylim[1], pars$xlim[2], pars$ylim[2]))
  })
  layer$setGeometry(0,0,600,150)
})


IntervalView.gen$methods(show = function(){
  view$show()
})

setMethod("print","IntervalView",function(x){
  x$show()
})

IntervalPainter <- function(gr, geom, color, group, facet, fill, obj){
  if(missing(geom)&!(missing(obj)))
    geom <- obj$pars$geom
  if(missing(geom)&missing(obj))
    geom <- "full"
  if(missing(color)&(!missing(obj)))
    color <- obj$pars$color
  if(!missing(group)){
    vs <- eval(as.symbol(group), elementMetadata(gr))
    grl <- split(gr, vs)
    group <- as.symbol(group)
  }else{
    group <- NULL
    grl <- list(gr)
  }
  ts <- lapply(grl, function(gr){
    mr <- gr
    ir <- ranges(gr)
    bins <- disjointBins(ir)
    binmx <- max(bins*10+5)
    mr.r <- reduce(mr)
    sts <- start(gr)
    wds <- width(gr)
    ptx <- sts+wds/2
    cov <- coverage(gr)
    list(bins = bins, binmx = binmx,  mr = mr,
         mr.r = mr.r, sts = sts, wds = wds, ptx = ptx, cov = cov)
  })

  lvpainter <- function(layer,painter,exposed){
    xlimZoom <- as.matrix(exposed)[,1]
    ylimZoom <- as.matrix(exposed)[,2]
    obj$pars$xlimZoom <- xlimZoom
    obj$pars$ylimZoom <- ylimZoom
    ## compute color on the fly
    ## Draw rectangle
    ##----------------------------------------------------------------------
    ##  geom "full"
    ##----------------------------------------------------------------------
    if(geom == "full"){
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
            qdrawRect(painter, 0, gs-2, obj$pars$seqlength, gs+5-2,
                      fill = NA, stroke = "gray80")
          })
        })
        obj$pars$ylim <- c(0,5*(length(ts)))
      }else{
        cols <- genLegend(gr, color = color)$color
        with(ts[[1]], 
             qdrawRect(painter,start(mr),(bins*10)/binmx*5,end(mr),
                       (bins*10+5)/binmx*5,
                       stroke=NA,fill=cols))
      obj$pars$ylim <- expand_range(c(0, 5), mul = 0.05)
    }
    }
    if(geom == "dense"){
      qdrawRect(painter,start(mr.r), 10, end(mr.r), 20,
                stroke=NA,fill=values(mr.r)$.color)
      obj$pars$ylim <<- c(0,30)
    }
    if(geom == "segment"){
      qdrawSegment(painter, ptx, 10, ptx, 20, stroke = values(mr)$.color)
      obj$pars$ylim <<- c(0,30)
    }
    if(geom == "barchart"){
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
      obj$pars$ylim <- expand_range(c(0, 5), mul = 0.05)
    }
    if(geom == "mismatch"){
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
        obj$pars$ylim <- expand_range(c(0, 5), mul = 0.05)
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
        obj$pars$ylim <- expand_range(c(0, max(values(sumlong)$.level-1)), mul = 0.05)
      }      

    }
    if(geom == "seqlogo"){
      
    }
    if(geom == "histogram"){
      
    }
    if(geom == "line"){
      
    }
  }
}


