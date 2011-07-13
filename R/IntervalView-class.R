##----------------------------------------------------------##
##             For class "IntervalView"
##----------------------------------------------------------##
IntervalView.gen <- setRefClass("IntervalView",
                                contains = c("QtVisnabView", "LinearView"),
                                fields = c(track = "SimpleMutableGRanges",
                                  flag = "logical",
                                  signalingFields(list(group = "character",
                                                       facetBy = "character",
                                                       y = "character"))))

##----------------------------------------------------------##
##             "IntervalView" constructor
##----------------------------------------------------------##
IntervalView <- function(track,
                         seqname,
                         group,
                         color,
                         facetBy, 
                         y,
                         viewname = "Interval Data",
                         geom = c("full", "reduce", "point", "length",
                           "barchart", "heatmap", "segment", "line"),
                         rescale = c("geometry", "transform", "none"),
                         overview = FALSE){
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
  
  pars <- GraphicPars(xlimZoom = xlimZoom, 
                      geom = geom, color = color,
                      view = "IntervalView")

  ##FIXME: a little hack of addAttr doesn't work for MutableGRanges right now
  track <- addAttr(track, .color = "red")

  if(is(track, "GRanges"))
    track <- as(track,"MutableGRanges")

  ## FIXME:
  if(missing(group))
    group <- character()
  if(missing(facetBy))
    facetBy <- character()
  if(missing(y))
    y <- character()
  
  message("Create new instance...")
  obj <- IntervalView.gen$new(track=track,pars=pars, rescale = rescale,
                              tooltipinfo = tooltips, viewname = viewname,
                              group = group, selfSignal = FALSE, focusin = FALSE,
                              viewrange = viewrange, facetBy = facetBy,
                              y = y)
  
  obj$track$changed$connect(function(change){
    if(is(change, "GRangesChanges"))
      if(elementMetadataChanged(change))
        qupdate(obj$scene)
  })
  
  message("Creating view...")
  obj$createView(overview = overview)
  obj$regSignal()
  message("Done")
  return(obj)
}

############################################################
## createview method
############################################################
IntervalView.gen$methods(createView = function(overview = FALSE){

  setDislayWidgets()
  setBgColor()
  ## FIXME: what happened when you try to facet it
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

  if(!overview){
    seqs <- sortChr(as.character(seqnames(viewrange)))
  }else{
    seqs <- sortChr(unique(as.character(seqnames(track))))
  }
  sapply(seq_along(seqs), function(j){
    message("Start creating layer for ",seqs[j])
    seqname <- seqs[j]
    start <- min(start(ranges(track[seqnames(track)==seqname])))
    end <- max(end(ranges(track[seqnames(track)==seqname])))
    seqlength <- end
    mr <- track[seqnames(track)==seqname]
    if(length(facetBy))
      facetby <- unique(values(mr)[,facetBy])
    else
      facetby <- unique(values(mr)[,group])
    color <- pars$color
    ## assigne color
    genColor <- function(x){
      ## continuous
      if(is.numeric(x)){
        cscale(x, div_prox_pal())
      }else{
        dscale(x, dichromat_pal("DarkRedtoBlue.12"))
      }
    }
    
    if(!(is(color,"AsIs"))){
      vals <- values(mr)[,color]
      message("Generating colors...")
      cols <- genColor(vals)
      message("Assigning colors...")
      values(mr)$.color <- cols
    }else{
      values(mr)$.color <- color
    }
    pars$xlim <<- expand_range(c(start, end), mul = 0.05)
    ## pars$ylim <<- expand_range(c(0, 10), mul = 0.05)
    if(length(y)){
      yval <- values(mr)[,y]
      ymax <- max(yval)
      ymin <- min(yval)
    }
    sapply(seq_len(length(facetby)), function(i){

      curval <- facetby[i]
      if(length(facetBy))
        idx <- values(mr)[, facetBy] == curval
      else
        idx <- values(mr)[, group] == curval

    # Can use with gradient_n to create a continous gradient
    if(length(group)){
      lv <- as.numeric(as.factor(values(mr)[idx,group]))
      pars$ylim <<- expand_range(c(0, 10*(max(lv)-1)), mul = 0.05)
    }else{
      lv <- 1
    }
    st <- start(mr)[idx]
    ed <- st

    lvpainter <- function(layer,painter,exposed){
      col <- values(mr[idx])$.color
      pars$xlimZoomChanged$block()
      pars$xlimZoom <<- as.matrix(exposed)[,1]
      ylimZoom <- as.matrix(exposed)[,2]
      xlimZoom <- pars$xlimZoom
      if(!selfSignal){
        viewrange$rangesChanged$unblock()
        ranges(viewrange) <<- IRanges(pars$xlimZoom[1], pars$xlimZoom[2])
      }
      if(selfSignal){
        viewrange$rangesChanged$block()
        ranges(viewrange) <<- IRanges(pars$xlimZoom[1], pars$xlimZoom[2])
      }
      pars$xlimZoomChanged$unblock()
      pars$ylimZoom <<- ylimZoom
      ## compute color on the fly
      ## Draw rectangle
      ##----------------------------------------------------------------------
      ##  pars$geom "full"
      ##----------------------------------------------------------------------
      if(pars$geom == "full"){
        if(!".level" %in% colnames(values(mr))){
          addLevels(mr)
        }
        lvs <- values(mr)$.level
        qdrawRect(painter, st, (lvs-1)*10, ed, lvs*10, stroke = NA, fill = col)
        pars$ylim <<- expand_range(c(0, max(lvs)*10), mul = 0.05)
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
      if(pars$geom == "length"){
        yval <- width(track)
        ## cir <- qglyphCircle(r = 2)
        ## qdrawGlyph(painter, cir, (st+ed)/2, yval+10*(lv-1), stroke = NA, fill = col)
        qdrawSegment(painter, st, yval, ed, yval, stroke = col)
        pars$ylim <<- expand_range(c(0, max(yval)+max(10*(lv-1))), mul = 0.05)
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
        segs <- qglyphSegment(x = 20, dir = pi/2)
        qdrawGlyph(painter, segs, st, 10*(lv-1), stroke = col)
      }
      if(pars$geom == "line"){
        if(!length(y))
          stop("need to provide y value")
        yval <- values(mr)[idx, y]
        xval <- (st+ed)/2
        idx <- order(xval, decreasing = FALSE)
        N <- length(idx)
        qdrawSegment(painter, xval[idx][-N], yval[idx][-N],
                     xval[idx][-1], yval[idx][-1],
                     stroke = col)
        pars$ylim <<- expand_range(c(ymin, ymax), mul = 0.05)
      }

      if(pars$geom == "point"){
        .self$drawPoint(painter, data = mr[idx], y = y)
      }
      if(pars$geom == "length"){
        ## this accept the bam file and query
        yval <- values(track)[,y]
        cir <- qglyphCircle(r = 2)
        qdrawGlyph(painter, cir, (st+ed)/2, yval, stroke = NA, fill = col)
        pars$ylim <<- expand_range(c(0, max(yval)), mul = 0.05)
      }
  }
    ## used for hover
    flag <<- FALSE
    ## construct layer
      if(!length(facetBy)){
        i <- 1
      }
    rootLayer[i-1,j-1] <<- qlayer(scene,paintFun=lvpainter,
                              wheelFun=  wheelEventZoom(view),
                              keyPressFun =
                              keyPressEventZoom(track, view = view, sy = 1),
                              cache = FALSE)
    rootLayer[i-1,j-1]$setLimits(qrect(pars$xlim[1], pars$ylim[1],
                                   pars$xlim[2], pars$ylim[2]))
    pars$ylimChanged$connect(function(){
      rootLayer[i-1,j-1]$setLimits(qrect(pars$xlim[1], pars$ylim[1],
                                     pars$xlim[2], pars$ylim[2]))
    })
    })  
  })
  ## rootLayer[0,0]$layer$setGeometry(0,0,600,150)
})


IntervalView.gen$methods(show = function(){
  view$show()
})

setMethod("print","IntervalView",function(x){
  x$show()
})

IntervalView.gen$methods(regSignal = function(){
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
    viewrange$seqnamesChanged$block()
    seqlengths(viewrange) <<- seqlengths(track)[[as.character(seqnames(viewrange))]]
    viewrange$seqnamesChanged$unblock()
    ## hd <- scanBamHeader(file)
    ## pars$seqlength <<- hd[[1]]$targets[as.character(viewrange$seqnames)]
    rootLayer[0,0]$close()
    view$resetTransform()
    .self$createView()
    .self$regSignal()
  })
  pars$bgColorChanged$connect(function(){
    bgcol <- pars$bgColor$names()
    bgalpha <- pars$alpha
    qcol <- col2qcol(bgcol,bgalpha)
    scene$setBackgroundBrush(qbrush(qcol))
  })
})


IntervalView.gen$methods(drawPoint = function(painter, data, y){
  if(!length(y))
    stop("need to provide y value")
  yval <- values(data)[, y]
  cir <- qglyphCircle(r = 1)
  qdrawGlyph(painter, cir, (st+ed)/2, yval, stroke = NA, fill = col)
  pars$ylim <<- expand_range(c(ymin, ymax), mul = 0.05)
})


IntervalView.gen$methods(drawLine = function(painter){

})
