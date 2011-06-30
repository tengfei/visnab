##----------------------------------------------------------##
##             For class "IntervalView"
##----------------------------------------------------------##

TxdbView.gen <- setRefClass("TxdbView",contains="QtVisnabView",
                            fields=list(track="TranscriptDb",
                              introns="GRangesList",
                              fiveUTR="GRangesList",
                              threeUTR="GRangesList",
                              cds="GRangesList",
                              tx="GRanges",
                              exons = "GRanges"))

##----------------------------------------------------------##
##             "IntervalView" constructor
##----------------------------------------------------------##

##' To create a view for displaying information in a \code{TranscriptDb}
##' object.
##'
##' The constructor may take some time to retrieve information from
##' a \code{TranscriptDb} object, and store all the information which
##' required for visualiztion as fields, this is currently not ideal,
##' since \pkg{GenomicFeatures} doesn't support sending a small query to
##' SQLite database yet.
##'
##' ##' @title  \code{TxdbView} object constructor.
##' @param track A TranscriptDb object
##' @param seqname Chromosome name, expect the
##' name with 'chr' prefix, e.g. 'chr1'.
##' @param geom Geometry of the view. default is 'full'.
##' \describe{
##'   \item{\code{full}}{Showing all the introns/cds/5'-UTR/3'-UTR,
##' grouped by transcripts.}
##'   \item{\code{dense}}{Showing one single genmoic structure, collapse
##' all information into one single strand.}
##'   \item{\code{slice}}{Cut off introns and facilitate gene-centric view.}
##' }
##' @param rescale Control view port behaviors when zoom in/out
##' \describe{
##'   \item{\code{geometry}}{Hide sroll bar when zoomed in, default}   
##'   \item{\code{transform}}{...}
##'   \item{\code{none}}{Showing scroll bar when zoomed in}
##' }
##' @param viewname Name used for this view, will show as widget title when
##' embeded into tracks view.
##' @return A \code{TxdbView} object.
##' @author Tengfei Yin <yintengfei@gmail.com>
TxdbView <- function(track,
                     seqname,
                     geom=c("full", "dense", "slice"),
                     rescale = c("geometry", "transform", "none"),
                     viewname = "TranscriptDb",
                     ...){
  ## tootip information
  ## TODO: need to get information automatical some where
  ## may be  integrate some data automatically
  tooltips <- capture.output(print(track))

  if(missing(seqname))
    seqname <- as.character(unique(as.character(seqnames(seqinfo(track))))[1])
  start <- 0
  end <- seqlengths(track)[[seqname]]
  seqlength <- end
  xlimZoom <- c(start,end)
  
  geom <- match.arg(geom)
  geom <- new("TxdbViewGeomSingleEnum", geom)

  rescale <- match.arg(rescale)
  rescale <- new("RescaleSingleEnum", rescale)
  
  pars <- GraphicPars(xlimZoom = xlimZoom,
                      geom = geom,
                      view = "TxdbView")
  ## store those infor in object, so make switch to other chromosome fast
  message("Loading Introns...")
  introns <- intronsByTranscript(track)
  message("Loading 5' UTR...")
  fiveUTR <- fiveUTRsByTranscript(track)
  message("Loading 3' UTR...")
  threeUTR <- threeUTRsByTranscript(track)
  message("Loading CDS...")
  cds <- cdsBy(track,by="tx")
  message("Loading transcripts...")
  tx <- transcripts(track)
  ## loading exons?                        
  viewrange <- MutableGRanges(seqname, IRanges(start, end))
  seqlengths(viewrange) <- end
  obj <- TxdbView.gen$new(track = track, pars = pars,
                          viewrange = viewrange,
                          introns = introns, fiveUTR = fiveUTR, threeUTR = threeUTR,
                          rescale = rescale, tooltipinfo = tooltips,
                          cds = cds,tx = tx, viewname = viewname, 
                          selfSignal = FALSE, focusin = FALSE)

  ## add default attributes
  ## addAttr(obj$track,.color=obj$pars$fill,.hover=FALSE,.brushed=FALSE)
  message("Processing and creating view...")
  obj$createView()
  obj$regSignal()
  message("Ready")
  return(obj)
}

############################################################
## createview method
############################################################
TxdbView.gen$methods(createView = function(){
  
  seqname <- as.character(seqnames(viewrange))
  setDislayWidgets()
  setBgColor()

  ## compute levels
  tx.sub <- tx[seqnames(tx)==seqname]
  tx_id <- values(tx.sub)$tx_id

  .levels <- disjointBins(ranges(tx.sub))
  names(.levels) <- tx_id
  
  introns.sub <- introns[names(introns) %in% as.character(tx_id)]
  fiveUTR.sub <- fiveUTR[names(fiveUTR) %in% as.character(tx_id)]
  threeUTR.sub <- threeUTR[names(threeUTR) %in% as.character(tx_id)]
  cds.sub <- cds[names(cds) %in% as.character(tx_id)]
  ## exons.sub <- exons[names(exons) %in% as.character(tx_id)]

  int.l <- elementLengths(introns.sub)
  futr.l <- elementLengths(fiveUTR.sub)
  tutr.l <- elementLengths(threeUTR.sub)
  cds.l <- elementLengths(cds.sub)
  ## exons.l <- elementLengths(exons.sub)

  int <- unlist(introns.sub)
  futr <- unlist(fiveUTR.sub)
  tutr <- unlist(threeUTR.sub)
  cdss <- unlist(cds.sub)
  ## exn <- unlist(exons.sub)
  ## prepare for "reduce" geom
  int.r <- reduce(int)
  cds.r <- reduce(cdss)
  five.r <- reduce(futr)
  three.r <- reduce(tutr)
  ## should think a better way to do this

  irs <- reduce(c(cds.r,five.r,three.r))
  int.r <- setdiff(int.r,irs)

  values(int)$tx_id <- rep(names(introns.sub),int.l)
  values(futr)$tx_id <- rep(names(fiveUTR.sub),futr.l)
  values(tutr)$tx_id <- rep(names(threeUTR.sub),tutr.l)
  values(cdss)$tx_id <- rep(names(cds.sub),cds.l)
  ## values(exn)$tx_id <- rep(names(exons.sub),exons.l)

  values(int)$.level <- .levels[as.character(values(int)$tx_id)]
  values(futr)$.level <- .levels[as.character(values(futr)$tx_id)]
  values(tutr)$.level <- .levels[as.character(values(tutr)$tx_id)]
  values(cdss)$.level <- .levels[as.character(values(cdss)$tx_id)]
  ## values(exn)$.level <- .levels[as.character(values(exn)$tx_id)]

  ## int.pos <- int[strand(int)=="+"]
  ## int.neg <- int[strand(int)=="-"]

  ## tail(sort(width(introns.sub)))

  st.int <- start(int)
  ed.int <- end(int)
  lv.int <- values(int)$.level
  strand.int <- as.character(strand(int))

  st.five <- start(futr)
  ed.five <- end(futr)
  lv.five <- values(futr)$.level

  st.three <- start(tutr)
  ed.three <- end(tutr)
  lv.three <- values(tutr)$.level

  st.cds <- start(cdss)
  ed.cds <- end(cdss)
  lv.cds <- values(cdss)$.level

  ## for "dense"
  st.five.r <- start(five.r)
  ed.five.r <- end(five.r)

  st.three.r <- start(three.r)
  ed.three.r <- end(three.r)

  st.cds.r <- start(cds.r)
  ed.cds.r <- end(cds.r)

  st.int.r <- start(int.r)
  ed.int.r <- end(int.r)
  strand.int.r <- as.character(strand(int.r))

  ylim <- c(0,max(c(lv.int, lv.five, lv.cds, lv.three))*5+10)
  xlim <- c(0,seqlengths(viewrange))
  ## xlim.mar <- 0.05*diff(xlim)
  ## ylim.mar <- 0.05*diff(ylim)

  ## pars$xlim <<- c(xlim[1]-xlim.mar,xlim[2]+xlim.mar)
  ## pars$ylim <<- c(ylim[1]-ylim.mar,ylim[2]+ylim.mar)
  pars$xlimChanged$block()
  pars$ylimChanged$block()
  pars$xlim <<- xlim
  pars$ylim <<- expand_range(ylim, mul = 0.05)
  pars$xlimChanged$unblock()
  pars$ylimChanged$unblock()


  ## canonical strucuture
  drawfun <- function(layer,painter,exposed){
    pars$xlimZoomChanged$block()
    pars$xlimZoom <<- as.matrix(exposed)[,1]
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

    if(pars$geom=="full"){
      ## 5'UTR
      if(length(st.five)>0)
        qdrawRect(painter,st.five,10*lv.five-2,ed.five,
                  10*lv.five+2,fill="black",stroke=NA)
      ## 3'
      if(length(st.three)>0)
        qdrawRect(painter,st.three,10*lv.three-2,ed.three,
                  10*lv.three+2,fill="black",stroke=NA)
      ## cds
      if(length(st.cds)>0)
        qdrawRect(painter,st.cds,10*lv.cds-4,ed.cds,10*lv.cds+4,
                  fill="black",stroke=NA)
      ## intron
      if(length(st.int)>0)
        qdrawSegment(painter,st.int,10*lv.int,ed.int,10*lv.int,stroke="black")

      if(diff(xlimZoom)<1e5){
        unit <- as.integer(diff(xlimZoom)/50)
      ## draw arrow to indicate strand
        ## subset first
        intsub <- subsetByOverlaps(int, viewrange)
        st.int <- start(intsub)
        ed.int <- end(intsub)
        lv.int <- values(intsub)$.level
        ardf <- lapply(seq_along(st.int), function(i){
          n <- round((ed.int[i]-st.int[i])/unit, dig = 0)
          xs <- cbreaks(c(st.int[i], ed.int[i]), pretty_breaks(n))$breaks
          xs <- xs[xs >= st.int[i] & xs <= ed.int[i]]
          df <- data.frame(x = xs, y = rep(as.numeric(lv.int[i])*10, length(xs)))
        })
        ardf <- do.call("rbind", ardf)
        ## negative strand
        idx <- as.character(strand(intsub)) == "-"
        arrow <- qglyphArrow(dir = "left")
        qdrawGlyph(painter, arrow, ardf[idx, "x"], ardf[idx, "y"], cex = 0.5, fill = NA)
        arrow <- qglyphArrow(dir = "right")
        qdrawGlyph(painter, arrow, ardf[!idx, "x"], ardf[!idx, "y"], cex = 0.5, fill = NA)
        ## positive strand
      }
      pars$ylim <<- ylim
    }
    if(pars$geom=="dense"){
      ## reduced structure
      ## 5'UTR
      if(length(st.five.r)>0)
        qdrawRect(painter,st.five.r,10-2,ed.five.r,
                  10+2,fill="black",stroke=NA)
      ## 3'UTR
      if(length(st.three.r)>0)
        qdrawRect(painter,st.three.r,10-2,ed.three.r,
                  10+2,fill        
                  ="black",stroke=NA)
      ## cds
      if(length(st.cds.r)>0)
        qdrawRect(painter,st.cds.r,10-4,ed.cds.r,10+4,
                  fill="black",stroke=NA)
      ## introns
      if(length(st.int.r)>0)
        qdrawSegment(painter,st.int.r,10,ed.int.r,10,stroke="black")

      if(diff(xlimZoom)<1e5){
        unit <- as.integer(diff(xlimZoom)/50)
      ## draw arrow to indicate strand
        ## subset first
        intsub <- subsetByOverlaps(int.r, viewrange)
        st.int <- start(intsub)
        ed.int <- end(intsub)
        lv.int <- values(intsub)$.level
        ardf <- lapply(seq_along(st.int), function(i){
          n <- round((ed.int[i]-st.int[i])/unit, dig = 0)
          xs <- cbreaks(c(st.int[i], ed.int[i]), pretty_breaks(n))$breaks
          xs <- xs[xs >= st.int[i] & xs <= ed.int[i]]
          df <- data.frame(x = xs, y = rep(10, length(xs)))
        })
        ardf <- do.call("rbind", ardf)
        ## negative strand
        idx <- as.character(strand(intsub)) == "-"
        arrow <- qglyphArrow(dir = "left")
        qdrawGlyph(painter, arrow, ardf[idx, "x"], ardf[idx, "y"], cex = 0.5, fill = NA)
        arrow <- qglyphArrow(dir = "right")
        qdrawGlyph(painter, arrow, ardf[!idx, "x"], ardf[!idx, "y"], cex = 0.5, fill = NA)
        ## positive strand
      }

      pars$ylim <<- c(-20,40)
    }
  }
  ## selectedRangesFun
  ## selectedRangesFun <- function(layer, painter){
  ##   srm <- selectedRangesModel
  ##   if(length(srm)>0){
  ##     cols <- selectedRangesModelColor
  ##     if((as.character(cols) %in% names(elementMetadata(srm)))){
  ##       cols.value <- elementMetadata(srm)[[cols]]
  ##       if(is.numeric(cols.value)){
  ##         cols <- cscale(cols.value, pars$cpal)
  ##       }else{
  ##         cols <- dscale(factor(cols.value), pars$dpal)
  ##       }}else{
  ##         cols <- rep(cols,length(srm))
  ##       }
  ##     idx <- as.character(seqnames(selectedRangesModel)) == seqname
  ##     qdrawRect(painter, start(srm)[idx], 0,
  ##               end(srm)[idx], 10, stroke = NA, fill = cols[idx])
  ##   }
  ## }
  keyOutFun <- function(layer, event){
    focusin <<- FALSE
  }
  hoverEnterFun <- function(layer, event){
    focusin <<- TRUE
  }
  hoverLeaveFun <- function(layer, event){
    focusin <<- FALSE
  }

  rootLayer[0,0] <<- qlayer(scene, drawfun, 
                       wheelFun= wheelEventZoom(view, sy = 1),
                       keyPressFun = keyPressEventZoom(.self, view, focusin = focusin),
                       hoverEnterFun = hoverEnterFun,
                       focusOutFun = keyOutFun, hoverLeaveFun = hoverLeaveFun)
  
  rootLayer[0,0]$setLimits(qrect(pars$xlim[1],pars$ylim[1],
                            pars$xlim[2],pars$ylim[2]))

  pars$ylimChanged$connect(function(){
    rootLayer[0,0]$setLimits(qrect(pars$xlim,pars$ylim))
  })
})

TxdbView.gen$methods(regSignal = function(){
  
  viewrange$rangesChanged$connect(function(){
    qupdate(scene)
  })
  
  pars$xlimZoomChanged$connect(function(){
    zoom_factor <- diff(pars$xlimZoom)/seqlengths(viewrange)
    ## then scale view
    view$resetTransform()
    view$scale(1/zoom_factor, 1)
    ## then center view
    pos.x <- mean(pars$xlimZoom)
    pos.y <- mean(pars$ylim)
    pos.scene <- as.numeric(rootLayer[0,0]$mapToScene(pos.x, pos.y))
    view$centerOn(pos.scene[1], pos.scene[2])
  })
  ## geom
  pars$geomChanged$connect(function(){
    qupdate(scene)
  })
  ## signal when change xlimZoom
  ## seqname change should update view and update seqlength
  viewrange$seqnamesChanged$connect(function(){
    ## end <- max(end(ranges(track[seqnames(track)==seqname])))
    viewrange$seqnamesChanged$block()
    seqlengths(viewrange) <<- seqlengths(track)[[as.character(seqnames(viewrange))]]
    viewrange$seqnamesChanged$unblock()
    ## pars$xlimZoom <<- c(0 ,end)
    ## selectedRange <<- c(start,end)
    ## rootLayer[0,0]$close()
    ## obj$rootLayer[0,0]$gridLayout()$removeAt(0)
    rootLayer[0,0]$close()
    view$resetTransform()
    createView()
    regSignal()
  })
  ## selectedRangesModelChanged$connect(function(){
  ##   qupdate(scene)
  ## })
  pars$bgColorChanged$connect(function(){
    bgcol <- pars$bgColor
    bgalpha <- pars$alpha
    qcol <- col2qcol(bgcol,bgalpha)
    scene$setBackgroundBrush(qbrush(qcol))
  })
})

TxdbView.gen$methods(show = function(){
  view$show()
})

setMethod("print","TxdbView",function(x,..){
  x$show()
})


setMethod("geom","TxdbView",function(x,...){
  cat("Choosed geom: ",x$pars$geom,"\n")
  cat("---------------------\n")
  cat("Supported geoms: \n")
  geoms <- levels(x$pars$geom)
  if(!is.null(geoms))
    cat(geoms,"\n")
  else
    message("No supported geom is found for this object")
})

setReplaceMethod("geom","TxdbView", function(x,value){
  geoms <- levels(x$pars$geom)
  if(!(value %in% geoms))
    stop("Geom should be one of ", toString(geoms))
  else
    x$pars$geom@.Data <- value
  x
})


