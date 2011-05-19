##----------------------------------------------------------------------------##
##             For class "AlignmentView"
##----------------------------------------------------------------------------##
AlignmentView.gen <- setRefClass("AlignmentView",contains = "QtVisnabView",
                                 fields = list(track = "list",
                                   lower = "numeric",
                                   cutbin = "numeric",
                                   file = "character",
                                   model = "GenomicRanges"))

##----------------------------------------------------------------------##
##             "AlignmentView" constructor
##----------------------------------------------------------------------##

AlignmentView <- function(file = NULL,
                          model = NULL,
                          lower = 10L,
                          cutbin = 20L,
                          seqname = NULL,
                          scene = NULL,
                          view = NULL,
                          rootLayer = NULL,
                          row = 0L,
                          col = 0L,
                          rowSpan = 1L,
                          colSpan = 1L,
                          fill = "black",
                          geom = c("oneside","twoside"),
                          ...){

  if(is.null(scene)){
    scene <- qscene()
    view <- qplotView(scene,rescale="none")
    view$setDragMode(Qt$QGraphicsView$ScrollHandDrag)
    rootLayer <- qlayer(scene,geometry=qrect(0,0,800,600))
  }

  if(is.null(seqname)){
    seqname <- as.character(seqnames(model))[1]
  }
  pars <- GraphicPars(seqname = seqname, fill = fill, geom = geom,
                      view = "AlignmentView")
  obj <- AlignmentView.gen$new(track = NULL, model= model, file = file,
                               row = row, col = col,
                               rowSpan = rowSpan, colSpan = colSpan,
                               scene = scene, view = view,rootLayer = rootLayer,
                               pars = pars, lower = lower, cutbin = cutbin)
  obj$createView()
  obj
}


##---------------------------------------------------------##
##             print method
##---------------------------------------------------------##

AlignmentView.gen$methods(createView = function(seqname = NULL){
  bgcol <- pars$bgColor
  bgalpha <- pars$alpha
  qcol <- col2qcol(bgcol,bgalpha)
  scene$setBackgroundBrush(qbrush(qcol))

  if(!is.null(seqname))
    pars$seqname <<- seqname
  seqname <- pars$seqname
  start <- 0
  end <- max(end(model[seqnames(model)==seqname]))
  pars$xlimZoom <<- c(start,end)
  
  gr <- GRanges(seqnames=seqname,ranges=IRanges(start=start,end=end))
  
  zoomLevel <- c(10000, 250, 5)
  ## precessing data
  
  pfunAlign <- function(layer,painter,exposed){
    xlimZoom <- as.matrix(exposed)[,1]
    pars$xlimZoom <<- xlimZoom
    if(diff(xlimZoom)>zoomLevel[1]){
      qdrawText(painter,"Keep zooming in to see more details",
                xlimZoom[1]+0.5*(diff(xlimZoom)), 5, "center","bottom")
      pars$ylim <<- c(0, 10)
    }
    if(diff(xlimZoom) <= zoomLevel[1]&
       diff(xlimZoom) > zoomLevel[2]){
      if(pars$geom == "oneside"){
        ## idxs <- findOverlaps(ranges(ir),IRanges(xlimZoom[1],xlimZoom[2]))
        ## idxs <- idxs@matchMatrix[,1]
        ## idxs <- unique(idxs)
        gr <- GRanges(seqnames = seqname,
                      IRanges(xlimZoom[1], xlimZoom[2]))
        bam <- scanBam(file, param=ScanBamParam(which = gr))
        bam <- bam[[1]]
        ir <- data.frame(start=bam$pos,width=width(bam$seq),
                          strand=bam$strand)
        if(nrow(ir)>0){
          ir <- GRanges(seqnames=seqname,
                      ranges=IRanges(start=ir$start, width=ir$width),
                      strand=ir$strand)
          ## ir <- ir[idxs]
          binir <- disjointBins(ranges(ir))
          binmx <- max(binir*90+80)
          x0 <- start(ir)
          y0 <- -binir*90
          x1 <- end(ir)
          y1 <- -(binir*90+80)
          idxp <- as.logical(strand(ir)=="+")
          idxn <- as.logical(strand(ir)=="-")
          if(sum(idxp)>0){
            qdrawRect(painter,x0[idxp],y0[idxp],x1[idxp],y1[idxp],
                      stroke=rgb(0,0,1,0.8),fill=rgb(0,0,1,0.8))
          }
          if(sum(idxn)>0){
            qdrawRect(painter,x0[idxn],y0[idxn],x1[idxn],y1[idxn],
                      stroke=rgb(0,1,0,0.8),fill=rgb(0,1,0,0.8))
          }
          pars$ylim <<- c(-(cutbin*90+80),0)
        }}
      if(pars$geom == "twoside"){ print("not supported yet")}
    }
    ## level 3:
    if(diff(xlimZoom) <= zoomLevel[2]&
       diff(xlimZoom) >= zoomLevel[3]){
            if(pars$geom == "oneside"){
        ## idxs <- findOverlaps(ranges(ir),IRanges(xlimZoom[1],xlimZoom[2]))
        ## idxs <- idxs@matchMatrix[,1]
        ## idxs <- unique(idxs)
        gr <- GRanges(seqnames = seqname,
                      IRanges(xlimZoom[1], xlimZoom[2]))
        bam <- scanBam(file, param=ScanBamParam(which = gr))
        bam <- bam[[1]]
        ir <- data.frame(start=bam$pos,width=width(bam$seq),
                          strand=bam$strand)
        if(nrow(ir)>0){
          ir <- GRanges(seqnames=seqname,
                      ranges=IRanges(start=ir$start, width=ir$width),
                      strand=ir$strand)
          seqs <- bam$seq
          wd <- width(seqs)
          seqstrs <- lapply(seq_along(wd),function(i)
                                   IRanges::safeExplode(toString(seqs[i])))
          ## need to show short read string
          ## ir <- ir[idxs]
          binir <- disjointBins(ranges(ir))
          binmx <- max(binir*90+80)
          x0 <- lapply(1:length(ir), function(i){
            start(ir[i])+(1:wd[i])-1
          })
          idxp <- as.logical(strand(ir)=="+")
          idxn <- as.logical(strand(ir)=="-")
          dnacol <- baseColor(IRanges:::safeExplode("ACTGN"))

          if(sum(idxp)>0){
            x0p <- unlist(x0[idxp])
            strsp <- unlist(seqstrs[idxp])
            idx <- match(strsp,names(dnacol))
            cols <- unname(unlist(dnacol[idx]))
            y0p <- rep(-binir[idxp]*90, times = wd[idxp])
            qdrawText(painter,strsp, x0p, y0p, color = cols)
          }
          if(sum(idxn)>0){
            x0n <- unlist(x0[idxn])
            strsn <- unlist(seqstrs[idxn])
            y0n <- rep(-binir[idxn]*90, times = wd[idxn])
            idx <- match(strsp,names(dnacol))
            cols <- unname(unlist(dnacol[idx]))
            qdrawText(painter, strsn, x0n, y0n, color = cols)
          }
          pars$ylim <<- c(-(cutbin*90+80),0)
        }
        }
          }}

      layer <- qlayer(rootLayer,
                      paintFun = pfunAlign,
                      row = row,
                      col = col,
                      rowSpan = rowSpan,
                      colSpan = colSpan,
                      wheelFun = wheelEventZoom(view),
                      keyPressFun = keyPressEventZoom(.self, view, sy = 1))
      pars$ylim <<- c(-(cutbin*90+80),0)
      layer$setLimits(qrect(c(0, end),pars$ylim))
      pars$ylimChanged$connect(function(){
        layer$setLimits(qrect(c(0, end),pars$ylim))
      })
      layer$setGeometry(0,0,600,150)
    })




  AlignmentView.gen$methods(show = function(){
    view$show()
  })

  setMethod("print","AlignmentView",function(x,..){
    x$show()
  })

  ## obj$pars$seqnameChanged$connect(function(){
  ##   obj$rootLayer$close()
  ##   obj$rootLayer <- qlayer(obj$scene,geometry=qrect(0,0,800,600),row=obj$row)
  ##   obj$view$resetTransform()
  ##   obj$createView()
  ##   gc()
  ##   ## obj$show()
  ## })
