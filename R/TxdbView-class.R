##----------------------------------------------------------##
##             For class "IntervalView"
##----------------------------------------------------------##
TxdbView.gen <- setRefClass("TxdbView",contains="QtVisnabView",
                            fields=list(track="TranscriptDb",
                              introns="GRangesList",
                              fiveUTR="GRangesList",
                              threeUTR="GRangesList",
                              cds="GRangesList",
                              tx="GRanges"))

##----------------------------------------------------------##
##             "IntervalView" constructor
##----------------------------------------------------------##

TxdbView <- function(track,
                     seqname=NULL,
                     scene=NULL,
                     view = NULL,
                     rootLayer = NULL,
                     row=0L,
                     col=0L,
                     rowSpan=1L,
                     colSpan=1L,
                     fill="black",
                     title=NULL,
                     geom=c("full","dense")){

  ## if null, set first chromosome as viewed chromosome
  ## if(is.null(title))
  ##   title <- deparse(substitute(mr))
  
  if(is.null(seqname))
    seqname <- as.character(unique(as.character(seqnames(track)))[1])
  start <- 0
  end <- seqlengths(track)[[seqname]]
  xlimZoom <- c(start,end)
  ## if(extends(class(mr),"GRanges"))
  ##   mr <- as(mr,"MutableGRanges")
  ## ## connect signal
  ## mr$elementMetadataChanged$connect(function() {
  ##   qupdate(scene)
  ## })
  pars <- GraphicPars(fill=fill,xlimZoom = xlimZoom,
                      seqname=seqname, geom=geom)
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

  obj <- TxdbView.gen$new(track=track, pars=pars,
                          row=row,col=col, rowSpan=rowSpan, colSpan=colSpan,
                          scene=NULL,view=NULL,rootLayer=NULL,title=title,
                          introns=introns, fiveUTR=fiveUTR, threeUTR=threeUTR,
                          cds=cds,tx=tx)

  obj$pars$bgColorChanged$connect(function(){
    bgcol <- obj@pars$bgColor
    bgalpha <- obj@pars$alpha
    qcol <- col2qcol(bgcol,bgalpha)
    scene$setBackgroundBrush(qbrush(qcol))
  })

  obj$pars$seqnameChanged$connect(function(){
    start <- 0
    ## end <- max(end(ranges(obj$track[seqnames(obj$track)==obj$pars$seqname])))
    end <- seqlengths(track)[[obj$pars$seqname]]
    obj$pars$xlimZoom <- c(start,end)
    obj$scene <- qscene()
    layout <- obj$rootLayer$gridLayout()
    layout[obj$row,obj$col]$close()
    ## obj$thisLayer$close()
    ## obj$rootLayer <- qlayer(obj$scene,geometry=qrect(0,0,800,600),row=obj$row)
    ## obj$view$resetTransform()
    obj$createView()
  })

  ## add default attributes
  ## addAttr(obj$track,.color=obj$pars$fill,.hover=FALSE,.brushed=FALSE)
  message("Processing and creating view...")
  obj$createView()
  obj$pars$geomChanged$connect(function(){
    qupdate(obj$scene)
  })
  message("Ready")
  return(obj)
}

############################################################
## createview method
############################################################
TxdbView.gen$methods(createView = function(seqname=NULL, geom=NULL){
  
  if(!is.null(geom))
    pars$geom <<-  geom

  if(is.null(scene)){
    scene <<- qscene()
    view <<- qplotView(scene,rescale="none")
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
    xlimZoom <- as.matrix(exposed)[,1]
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
      ## when zoom-in show arrow
      ## draw arrow to indicate direction
      ## qdrawSegment(painter,start(introns.pos),8,start(introns.pos)+,8)
      pars$ylim <<- ylim
    }else if(pars$geom=="dense"){
      ## reduced structure
      ## 5'UTR
      if(length(st.five.r)>0)
      qdrawRect(painter,st.five.r,10-2,ed.five.r,
                10+2,fill="black",stroke=NA)
      ## 3'UTR
      if(length(st.three.r)>0)
      qdrawRect(painter,st.three.r,10-2,ed.three.r,
                10+2,fill="black",stroke=NA)
      ## cds
      if(length(st.cds.r)>0)
      qdrawRect(painter,st.cds.r,10-4,ed.cds.r,10+4,
                fill="black",stroke=NA)
      if(length(st.int.r)>0)
        qdrawSegment(painter,st.int.r,10,ed.int.r,10,stroke="black")
      pars$ylim <<- c(0,20)
    }else{
      cat("geom must be one of full/dense")
    }
  }

  thisLayer <- qlayer(rootLayer,drawfun,limits=qrect(pars$xlim[1],pars$ylim[1],
                                       pars$xlim[2],pars$ylim[2]),
                  wheelFun=  function(layer, event) {
                    zoom_factor <- 2
                    if (event$delta() < 0)
                      zoom_factor <- 1/2
                    view$scale(zoom_factor,1)
                  })
  
  pars$ylimChanged$connect(function(){
    thisLayer$setLimits(qrect(pars$xlim,pars$ylim))
  })

})

TxdbView.gen$methods(show = function(){
  view$show()
})

setMethod("print","TxdbView",function(x,..){
  x$show()
})

