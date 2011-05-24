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

TxdbView <- function(track,
                     genome=NULL,
                     seqname=NULL,
                     scene=NULL,
                     view = NULL,
                     rootLayer = NULL,
                     thisLayer = NULL,
                     selectedRangesModel = NULL,
                     selectedRangesModelColor = "red", 
                     row=0L,
                     col=0L,
                     rowSpan=1L,
                     colSpan=1L,
                     geom=c("full","dense"),
                     rescale = "none",...){
  if(is.null(genome)){
    cat("Set genome to hg19 automatically, please make sure it's the one you want\n")
    genome <- "hg19"
  }
    
  if(is.null(seqname))
    seqname <- as.character(unique(as.character(seqnames(track)))[1])
  if(is.null(selectedRangesModel))
    selectedRangesModel <- MutableGRanges()
  if(is(selectedRangesModel,"GRanges"))
    selectedRangesModel <- as(selectedRangesModel,"MutableGRanges")
  start <- 0
  end <- seqlengths(track)[[seqname]]
  seqlength <- end
  xlimZoom <- c(start,end)
  pars <- GraphicPars(xlimZoom = xlimZoom,
                      seqname=seqname, geom=geom[1],
                      seqlength = seqlength,
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
  ir <- IRanges(start, end)
  obj <- TxdbView.gen$new(track = track, pars = pars, genome = genome,
                          selectedRangesModel = selectedRangesModel,
                          outputRange = xlimZoom,
                          selectedRangesModelColor = selectedRangesModelColor,
                          row = row,col = col, rowSpan = rowSpan, colSpan = colSpan,
                          scene = scene,view = view,rootLayer = rootLayer,
                          thisLayer = thisLayer, 
                          introns = introns, fiveUTR = fiveUTR, threeUTR = threeUTR,
                          cds = cds,tx = tx, selfSignal = FALSE, focusin = FALSE)


  ## add default attributes
  ## addAttr(obj$track,.color=obj$pars$fill,.hover=FALSE,.brushed=FALSE)
  message("Processing and creating view...")
  obj$createView(rescale = rescale)
  obj$regSignal()
  message("Ready")
  return(obj)
}

############################################################
## createview method
############################################################
TxdbView.gen$methods(createView = function(seqname=NULL, geom=NULL,
                       rescale = "geometry"){
  
  if(!is.null(geom))
    pars$geom <<-  geom
  
  if(is.null(scene)){
    scene <<- qscene()
    view <<- qplotView(scene,rescale = rescale)
    view$setDragMode(Qt$QGraphicsView$ScrollHandDrag)
    rootLayer <<- qlayer(scene,geometry=qrect(0,0,800,600))
  }

  if(!is.null(seqname))
    pars$seqname <<- seqname
  seqname <- pars$seqname
  
  bgcol <- pars$bgColor
  bgalpha <- pars$alpha
  qcol <- col2qcol(bgcol,bgalpha)
  scene$setBackgroundBrush(qbrush(qcol))
  
  start <- 0
  end <- seqlengths(track)[[seqname]]
  pars$seqlength <<- end
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

  ylim <- c(5,max(c(lv.int, lv.five, lv.cds, lv.three))*5+10)
  xlim <- c(0,end)
  xlim.mar <- 0.05*diff(xlim)
  ylim.mar <- 0.05*diff(ylim)

  pars$xlim <<- c(xlim[1]-xlim.mar,xlim[2]+xlim.mar)
  ## pars$xlim <<- xlim
  pars$ylim <<- c(ylim[1]-ylim.mar,ylim[2]+ylim.mar)
  ## pars$ylim <<- ylim

  ## canonical strucuture
  drawfun <- function(layer,painter,exposed){
    pars$xlimZoomChanged$block()
    pars$xlimZoom <<- as.matrix(exposed)[,1]
    if(!selfSignal){
      outputRangeChanged$unblock()
      outputRange <<- pars$xlimZoom 
    }
    if(selfSignal){
      outputRangeChanged$block()
      outputRange <<- pars$xlimZoom 
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
      ## FIXME: when zoom-in show arrow
      ## draw arrow to indicate direction
      ## qdrawSegment(painter,start(introns.pos),8,start(introns.pos)+,8)
      ## draw selectedRangesModel
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
      if(length(st.int.r)>0)
        qdrawSegment(painter,st.int.r,10,ed.int.r,10,stroke="black")
      pars$ylim <<- c(-20,40)
    }
  }
  ## selectedRangesFun
  selectedRangesFun <- function(layer, painter){
    srm <- selectedRangesModel
    if(length(srm)>0){
    cols <- selectedRangesModelColor
    if((as.character(cols) %in% names(elementMetadata(srm)))){
      cols.value <- elementMetadata(srm)[[cols]]
      if(is.numeric(cols.value)){
        cols <- cscale(cols.value, pars$cpal)
      }else{
        cols <- dscale(factor(cols.value), pars$dpal)
      }}else{
        cols <- rep(cols,length(srm))
      }
    idx <- as.character(seqnames(selectedRangesModel)) == seqname
    qdrawRect(painter, start(srm)[idx], 0,
              end(srm)[idx], 10, stroke = NA, fill = cols[idx])
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

  thisLayer <<- qlayer(rootLayer, drawfun, col = col, row = row,
                       rowSpan = rowSpan, colSpan = colSpan,
                       wheelFun= wheelEventZoom(view, sy = 1),
                       keyPressFun = keyPressEventZoom(.self, view, focusin = focusin),
                       hoverEnterFun = hoverEnterFun,
                       focusOutFun = keyOutFun, hoverLeaveFun = hoverLeaveFun)
  thisLayer$setLimits(qrect(pars$xlim[1],pars$ylim[1],
                         pars$xlim[2],pars$ylim[2]))
  ## txLayer <- qlayer(thisLayer, drawfun,
  ##                      limits=qrect(pars$xlim[1],pars$ylim[1],
  ##                        pars$xlim[2],pars$ylim[2]))
  


  ## signal
  pars$ylimChanged$connect(function(){
    thisLayer$setLimits(qrect(pars$xlim,pars$ylim))
  })
  ## selectedRangesModelLayer <- qlayer(thisLayer, selectedRangesFun, row=1,
  ##                          limits=qrect(pars$xlim[1],-5, pars$xlim[2],15))
  ## layout <- thisLayer$gridLayout()
  ## layout$setRowPreferredHeight(0,300)
  ## layout$setRowPreferredHeight(1,50)
  ## layout$setRowStretchFactor(0,1)
  ## layout$setRowStretchFactor(1,0)
})

TxdbView.gen$methods(regSignal = function(){
  pars$xlimZoomChanged$connect(function(){
    zoom_factor <- diff(pars$xlimZoom)/pars$seqlength
    ## then scale view
    view$resetTransform()
    view$scale(1/zoom_factor, 1)
    ## then center view
    pos.x <- mean(pars$xlimZoom)
    pos.y <- mean(pars$ylim)
    pos.scene <- as.numeric(thisLayer$mapToScene(pos.x, pos.y))
    view$centerOn(pos.scene[1], pos.scene[2])
  })
  ## geom
  pars$geomChanged$connect(function(){
    qupdate(scene)
  })
  ## signal when change xlimZoom
  ## seqname change should update view and update seqlength
  pars$seqnameChanged$connect(function(){
    start <- 0
    ## end <- max(end(ranges(track[seqnames(track)==pars$seqname])))
    end <- seqlengths(track)[[pars$seqname]]
    pars$seqlength <<- end-start
    ## pars$xlimZoom <<- c(0 ,end)
    ## selectedRange <<- c(start,end)
    thisLayer$close()
    view$resetTransform()
    .self$createView()
    ## .self$regSignal()
  })
  selectedRangesModelChanged$connect(function(){
    qupdate(scene)
  })
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
  geoms <- getOption("BioC")$visnab$TxdbView$geom
  if(!is.null(geoms))
    cat(geoms,"\n")
  else
    message("No supported geom is found for this object")
})

setReplaceMethod("geom","TxdbView", function(x,value){
  geoms <- getOption("BioC")$visnab$TxdbView$geom
  if(!(value %in% geoms))
    stop("Geom should be one of", geoms)
  else
    x$pars$geom <- value
  x
})


