##----------------------------------------------------------------------------##
##             For class "AlignmentView"
##----------------------------------------------------------------------------##
setClass("AlignmentView",contains="GraphicPars",
         representation(track="list",
                        lower="numeric",
                        cutbin="numeric"))

##----------------------------------------------------------------------##
##             "AlignmentView" constructor
##----------------------------------------------------------------------##

AlignmentView <- function(bam=NULL,
                          seqnames=NULL,
                          start=NULL,
                          end=NULL,
                          lower=10L,
                          cutbin=30L,
                          show=TRUE,
                          scene=qscene(),
                          view=qplotView(scene,rescale="none"),
                          rootLayer=qlayer(scene,geometry=qrect(0,0,800,600)),
                          row=0L,
                          col=0L,
                          ...){
  
  pars <- GraphicPars(...)@pars 
  new("AlignmentView",track=bam,seqnames=seqnames,
      pars=pars, lower=lower,cutbin=cutbin)
}


##---------------------------------------------------------##
##             print method
##---------------------------------------------------------##

setMethod("print","AlignmentView",function(x,...){
  obj <- x
  lower <- obj@lower
  cutbin <- obj@cutbin
  bgcol <- getAttr("bg.col")
  bgalpha <- getAttr("bg.alpha")
  qcol <- col2qcol(bgcol,bgalpha)
  scene$setBackgroundBrush(qbrush(qcol))
  wheelZoom <- function(layer, event) {
    zoom_factor <- 1.5
    if (event$delta() < 0)
      zoom_factor <- 1/1.5
    tform <- view$transform()
    tform$scale(zoom_factor,1)
    view$setTransform(tform)
  }

  ## preset the level
  zoomLevel <- c(10000)
  seqname <- 1
  ## precessing data
  bam <- obj@track[[seqname]]
  ir <- data.frame(start=bam$pos,width=width(bam$seq),
                   strand=bam$strand)

  if(!is.null(start)&!is.null(end)){
    irs <- na.omit(ir)
    idxs <- findInterval(c(start,end),irs$start)
    idx[1:idxs[1]] <- FALSE
    idx[idxs[2]:length(idx)] <- FALSE
  }
  
  ir <- GRanges(seqnames=seqname,
                ranges=IRanges(start=ir$start, width=ir$width),
                strand=ir$strand)
  
  covg <- coverage(ranges(ir))
  ir.v <- slice(covg,lower=lower)
  xpos <- viewWhichMaxs(ir.v)
  ypos <- viewMaxs(ir.v)
  xlimZoom <- c(min(start(ir)),max(end(ir)))

  pfun <- function(layer,painter,exposed){
    xlimZoom <<- as.matrix(exposed)[,1]
    if(diff(xlimZoom)>zoomLevel[1]){
      qdrawSegment(painter,xpos,log(ypos),xpos,0)
      sr.layer$setLimits(qrect(range(xpos),c(0,max(log(ypos)))))
    }
    if(diff(xlimZoom)<=zoomLevel[1]){
      idxs <- findOverlaps(ranges(ir),IRanges(xlimZoom[1],xlimZoom[2]))
      idxs <- idxs@matchMatrix[,1]
      idxs <- unique(idxs)
      if(length(idxs)>1){
        ir <- ir[idxs]
        binir <- disjointBins(ranges(ir))
        binmx <- max(binir*90+80)
        x0 <- start(ir)
        y0 <- binir*90
        x1 <- end(ir)
        y1 <- binir*90+80
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
         sr.layer$setLimits(qrect(range(xpos),c(0,cutbin*90+80)))
      }}
  }
  sr.layer <- qlayer(rootLayer,paintFun=pfun,
                     limits=qrect(range(xpos),c(0,max(log(ypos)))),
                     wheelFun=wheelZoom,
                     row=row,
                     col=col,
                     cache=FALSE)
  if(show)
    view$show()
  return(list(scene=scene,view=view,layer=sr.layer))
})




