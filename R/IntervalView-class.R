#----------------------------------------------------------##
##             For class "IntervalView"
##----------------------------------------------------------##
IntervalView.gen <-
  setRefClass("IntervalView",
              contains = c("QtVisnabView", "LinearView"),
              fields = c(track = "SimpleMutableGRanges",
                signalingFields(list(group = "character",
                                     facetBy = "character",
                                     size = "numericORcharacter",
                                     y = "character",
                                     x = "character"),
                                signalName = "argChanged")))

##----------------------------------------------------------##
##             "IntervalView" constructor
##----------------------------------------------------------##
IntervalView <- function(track,
                         seqname,
                         group,
                         color,
                         facetBy,
                         x = c("midpoint", "start", "end"),
                         y,
                         size,
                         alpha = 1,
                         side = c(1, 2),
                         bin = 10,
                         viewname = "Interval Data",
                         geom = c("point", "reduce", "full", "length",
                           "barchart", "heatmap", "segment",
                           "line", "histogram"),
                         rescale = c("geometry", "transform", "none"),
                         overview = FALSE,
                         grid = TRUE){
  ##
  tooltips <- "not implemented"
  ## if null, set first chromosome as viewed chromosome
  if(missing(seqname))
    seqname <- as.character(unique(as.character(seqnames(track)))[1])

  if(is.numeric(bin)&!is(bin, "PositiveInteger"))
    bin <- new("PositiveInteger", bin)
  x <- match.arg(x)
  start <- 0
  end <- max(end(ranges(track[seqnames(track)==seqname])))
  seqlength <- end
  xlimZoom <- c(start,end)

  geom <- match.arg(geom)
  geom <- new("IntervalViewGeomMultipleEnum", geom)

  rescale <- match.arg(rescale)
  rescale <- new("RescaleSingleEnum", rescale)

  viewrange <- MutableGRanges(seqname, IRanges(start, end))
  seqlengths(viewrange) <- end
  
  pars <- GraphicPars(xlimZoom = xlimZoom, 
                      geom = geom, 
                      alpha = alpha,
                      bin = bin,
                      view = "IntervalView")
  if(!missing(color))
    pars$color <- color

  mode <- IModeGroup(scaleMode = ScaleMode(zoomMode = "Horizontal"))
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
  ## FIXME:
  if(!length(facetBy))
    facetBy <- "1"
  
  obj <- IntervalView.gen$new(track=track,pars=pars, rescale = rescale,
                              tooltipinfo = tooltips, viewname = viewname,
                              group = group, eventTrace = new("EventTrace"),
                              viewrange = viewrange, facetBy = facetBy,
                              mode = mode, size = size,
                              x = x, y = y)
  
  obj$track$changed$connect(function(change){
    if(is(change, "GRangesChanges"))
      if(elementMetadataChanged(change))
        qupdate(obj$scene)
  })
  
  message("Creating view...")
  obj$createView(overview = overview, side = side,
                 grid = grid)
  obj$regSignal()
  message("Done")
  return(obj)
}

############################################################
## createview method
############################################################
IntervalView.gen$methods(createView = function(overview = FALSE,
                           side = c(1, 2),
                           grid = TRUE){
  setDislayWidgets()
  setBgColor()
  setMargin()
  if(grid)
    drawGrid()
  ## FIXME: what happened when you try to facet it
  .self$drawSummary(x = x, y = y, geom = pars$geom,
                    overview = overview)
  drawAxis(side = side)
})  

IntervalView.gen$methods(regSignal = function(...){
  .self$argChanged$connect(function(){
    qupdate(scene)
  })
  pars$xlimZoomChanged$connect(function(){
    zoom_factor <- diff(pars$xlimZoom)/seqlengths(viewrange)
    ## then scale view
    view$resetTransform()
    view$scale(1/zoom_factor, 1)
    ## then center viewr
    pos.x <- mean(pars$xlimZoom)
    pos.y <- mean(pars$ylim)
    pos.scene <- as.numeric(rootLayer[3,2]$mapToScene(pos.x, pos.y))
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
    rootLayer$close()
    view$resetTransform()
    createView()
    regSignal()
  })
  pars$ThemeChanged$connect(function(name){
    qupdate(scene)
  })
  pars$bgColorChanged$connect(function(){
    bgcol <- pars$bgColor$names()
    bgalpha <- pars$bgAlpha
    qcol <- col2qcol(bgcol,bgalpha)
    scene$setBackgroundBrush(qbrush(qcol))
  })
  callSuper(...)
})

IntervalView.gen$methods(
     drawSummary = function(x = c("start", "midpoint", "end"),
       y,
       geom = c("full", "reduce", "point",
         "length", "barchart", "heatmap",
         "segment", "line"),
       overview = FALSE){

       geom <- match.arg(geom)
       pars$geom <<- geom
       
       x <<- match.arg(x)
       
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
         if(length(facetBy)){
           ## if(length(facetBy))
           if(facetBy %in% colnames(values(mr)))
             facetby <- unique(values(mr)[,facetBy])
           else
             facetby <- facetBy
           ## else
           ##   facetby <- unique(values(mr)[,group])
         }

         ## assigne color
         sapply(seq_len(length(facetby)), function(i){         
         pars$xlim <<- expand_range(c(start, end), mul = 0.05)
         pars$ylim <<- expand_range(c(0, 10), mul = 0.05)
         
         if(facetBy %in% colnames(values(mr))){
           curval <- facetby[i]
           idx <- values(mr)[, facetBy] == curval
         }else{
           if(length(group)){
             curval <- facetby[i]
             idx <- values(mr)[, group] == curval
           }else{
             idx <- seq(length(mr))
           }
         }
         
         if(geom != "full"){
           if(length(y)){
             yval <- values(mr[idx])[,y]
           }}else{
             yval <- disjointBins(ranges(mr[idx]))
           }

         st <- start(mr)[idx]
         ed <- end(mr)[idx]

         ymax <- max(yval)
         ymin <- min(yval)

         xval <- switch(x,
                        start = st,
                        end = ed,
                        midpoint = (start(mr)+end(mr))/2)
         
         xmax <- max(xval)
         xmin <- min(xval)
         
         pars$ylim <<- expand_range(range(yval), mul = 0.05)
         pars$xlim <<- expand_range(range(xval), mul = 0.05)

         if(length(group)){
           lv <- as.numeric(as.factor(values(mr)[idx,group]))
           pars$ylim <<- expand_range(c(0, 10*(max(lv)-1)), mul = 0.05)
         }else{
           lv <- 1
         }

         sumPainter <- function(layer,painter,exposed){
           ## check size
           if(length(size)){
             if(is.numeric(size)){
               cex <- size
             }
             if(is.character(size)){
             if(size %in% colnames(values(mr))){
               vals <- values(mr)[,size]
               cex <- cscale(vals, rescale_pal(c(1, 3)))
             }}
           }else{
             cex <- as.numeric(pars$pointSize)
             cat("cex:",cex, "\n")
           }
           ## check alpha
           if(length(pars$alpha)){
             if(is.numeric(pars$alpha)){
               al <- pars$alpha
             }
             ## if(alpha %in% colnames(values(mr))){
             ##   vals <- values(mr)[,alpha]
             ##   al <- cscale(vals, rescale_pal(c(0.1, 1)))
             ## }
           }else{
               al <- 1
             }
           ## color
           genColor <- function(x){
             ## continuous
             if(is.numeric(x)){
               if("heatmap" %in% pars$geom)
                 cscale(x, div_prox_pal())
               else
                 cscale(x,
                        seq_gradient_pal("yellow", "red"))
             }else{
               if(length(unique(x)) <= 12)
                 dscale(as.character(x), brewer_pal(type = "qual",
                                                    palette = "Set3"))
               else{
                 cols <- brewer_pal()(9)
                 cscale(as.numeric(as.factor(x)), gradient_n_pal(cols))
               }
               ## dscale(x, dichromat_pal("DarkRedtoBlue.12"))
             }
           }
           color <- pars$color
           if(!(is(color,"AsIs"))){
             vals <- values(mr)[,color]
             ## message("Generating colors...")
             cols <- genColor(vals)
             cols <- alpha(cols, al)
             ## message("Assigning colors...")
             values(mr)$.color <- cols
           }else{
             values(mr)$.color <- alpha(color, al)
           }

           col <- values(mr[idx])$.color
           pars$xlimZoomChanged$block()
           pars$xlimZoom <<- as.matrix(exposed)[,1]
           ylimZoom <- as.matrix(exposed)[,2]
           xlimZoom <- pars$xlimZoom
           if(!eventTrace$selfSignal){
             viewrange$rangesChanged$unblock()
             ranges(viewrange) <<- IRanges(pars$xlimZoom[1], pars$xlimZoom[2])
           }
           if(eventTrace$selfSignal){
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
           if("full" %in% pars$geom){
             if(identical(st, ed))
               stop("no interval found, start == end")
             cols <- values(mr[idx])$.color
             qdrawRect(painter, st, (yval-1)*10, ed, yval*10, stroke = NA, fill = cols)
             pars$xlim <<- expand_range(range(c(min(st), max(ed))), mul = 0.05)
             pars$ylim <<- expand_range(c(0, max(yval)*10), mul = 0.05)
           }
           ## if(pars$geom == "reduce"){
           ##   qdrawRect(painter,start(mr.r), 10, end(mr.r), 20,
           ##             stroke=NA,fill=values(mr.r)$.color)
           ##   pars$ylim <<- c(0,30)
           ## }
           if("segment" %in% pars$geom){
             cols <- values(mr[idx])$.color
             qdrawSegment(painter, xval, 10, xval, 20, stroke = cols)
             pars$ylim <<- c(0,30)
           }
           if("barchart" %in% pars$geom){
             ## if(!is.null(fill)){
             ##   idx <- order(start(ts[[1]]$mr), values(ts[[1]]$mr)[,fill])
             ##   ts[[1]]$mr <- ts[[1]]$mr[idx]
             ##   ts[[1]]$bins <- ts[[1]]$bins[idx]
             ##   cols <- genLegend(ts[[1]]$mr, color = fill)$color
             ## }else{
             ##   cols <- "black"
             ## }
             ## with(ts[[1]],{
             ##   qdrawRect(painter,start(mr),((bins-1)*10)/binmx*5,end(mr)+1,
             ##             ((bins-1)*10+10)/binmx*5,
             ##             stroke=NA, fill = cols)}
             ##      )
             ## pars$ylim <<- expand_range(c(0, 5), mul = 0.05)
             cols <- values(mr[idx])$.color
             pars$ylim <<- expand_range(range(yval), mul = 0.05)
             qdrawSegment(painter, xval, min(pars$ylim), xval, yval, stroke = cols)
             ## fix me: qdrawRect doesn't work
             ## qdrawRect(painter, xval, min(pars$ylim),
             ##           xval+1, yval, stroke = NA, fill = cols)
           }
           if("seqlogo" %in% pars$geom){
           }
           if("heatmap" %in% pars$geom ){
             segs <- qglyphSegment(x = 20, dir = pi/2)
             qdrawGlyph(painter, segs, st, 10*(lv-1), stroke = col)
           }
           if("histogram" %in% pars$geom){
             bks <- pretty(range(xval), n = pars$bin)
             lvs <- cut(xval, breaks = bks)
             mx <- cbind(bks[-(length(bks))], bks[-1])
             dt <- as.data.frame(table(lvs))
             qdrawRect(painter, mx[,1], 0, mx[,2], dt$Freq,
                       fill = pars$fill, stroke = NA)
             .self$pars$ylim <- expand_range(c(0,max(dt$Freq)), mul = 0.05)
           }
           if("line" %in% pars$geom){
             ## by(1:length(xval), values(mr[idx])[,group], function(id){
               idxx <- order(xval, decreasing = FALSE)
               N <- length(idxx)
               cols <- alpha(pars$stroke, pars$alpha)
               qdrawSegment(painter, xval[idxx][-N], yval[idxx][-N],
                            xval[idxx][-1], yval[idxx][-1],
                            stroke = cols)
               ## })
             pars$ylim <<- expand_range(c(ymin, ymax), mul = 0.05)
             pars$xlim <<- expand_range(range(xval), mul = 0.05)
           }
           if("point" %in% pars$geom){
             cir <- qglyphCircle(r = 5)
             cols <- values(mr[idx])$.color
             qdrawGlyph(painter, cir, xval, yval, cex = cex, stroke = NA, fill = cols)
             pars$ylim <<- expand_range(range(yval), mul = 0.05)
             pars$xlim <<- expand_range(range(xval), mul = 0.05)
           }
           if("length" %in% pars$geom){
             ## this accept the bam file and query
             yval <- values(track)[,y]
             cir <- qglyphCircle(r = 2)
             qdrawGlyph(painter, cir, (st+ed)/2, yval, stroke = NA, fill = col)
             pars$ylim <<- expand_range(c(0, max(yval)), mul = 0.05)
           }
         }
         rootLayer[3, 2] <<- layer <- qlayer(scene, paintFun = sumPainter,
                                       wheelFun = wheelEventZoom(),
                                       keyPressFun = keyPressEventZoom(),
                                       ## hoverMoveFun = hoverMoveEvent(.self, mr[idx]),
                                       cache = TRUE,
                                       limits = qrect(pars$xlim[1], pars$ylim[1],
                                         pars$xlim[2], pars$ylim[2]))
         ## root.copy <- .self$copy()
         root.copy <- rootLayer
         syncLimits(layer)
         facetLayer[i, j] <<- root.copy
       })
       })
       })   
