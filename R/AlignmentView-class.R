##----------------------------------------------------------------------------##
##             For class "AlignmentView"
##----------------------------------------------------------------------------##
setClassUnion("listORNULL",c("list","NULL"))
AlignmentView.gen <- setRefClass("AlignmentView",contains="QtVisnabView",
                                 fields=list(track="listORNULL",
                                   lower="numeric",
                                   cutbin="numeric",
                                   file="character",
                                   model="GenomicRanges"))

##----------------------------------------------------------------------##
##             "AlignmentView" constructor
##----------------------------------------------------------------------##

AlignmentView <- function(file=NULL,
                          model=NULL,
                          lower=NULL,
                          cutbin=NULL,
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
                          ...){

  if(is.null(title))
    title <- deparse(substitute(bam))
  if(is.null(seqname)){
    seqname <- as.character(seqnames(model))[1]
  }
  pars <- GraphicPars(seqname = seqname)
  obj <- AlignmentView.gen$new(track=NULL,model=model,title=title,file=file,
                               row=row,col=col, rowSpan = rowSpan, colSpan = colSpan,
                               scene=NULL,view=NULL,rootLayer=NULL,
                               pars=pars, lower=lower,cutbin=cutbin)
  obj$pars$seqnameChanged$connect(function(){
    obj$rootLayer$close()
    obj$rootLayer <- qlayer(obj$scene,geometry=qrect(0,0,800,600),row=obj$row)
    obj$view$resetTransform()
    obj$createView()
    gc()
    ## obj$show()
  })
  obj$createView()
  obj
}


##---------------------------------------------------------##
##             print method
##---------------------------------------------------------##

AlignmentView.gen$methods(createView = function(seqname = NULL){
  if(is.null(scene)){
    scene <<- qscene()
    view <<- qplotView(scene,rescale="none")
    view$setDragMode(Qt$QGraphicsView$ScrollHandDrag)
    rootLayer <<- qlayer(scene,geometry=qrect(0,0,800,600))
  }
  if(!is.null(seqname))
    pars$seqname <<- seqname
  seqname <- pars$seqname
  start <- 0
  end <- max(end(model[seqnames(model)==seqname]))
  pars$xlimZoom <<- c(start,end)
  gr <- GRanges(seqnames=seqname,ranges=IRanges(start=start,end=end))
  
  message("Loading bam file... for",seqname)
  bam <- scanBam(file, param=ScanBamParam(which = gr))
  track <<- bam[[1]]
  message("Processing")
  bgcol <- pars$bgColor
  bgalpha <- pars$alpha
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

  keyPressEvent <- function(layer,event){
    if(event$modifiers() == Qt$Qt$ControlModifier){
      if(event$key() == Qt$Qt$Key_Equal)
        view$scale(1.5,1)
      if(event$key() == Qt$Qt$Key_Minus)
        view$scale(1/1.5,1)
      if(event$key() == Qt$Qt$Key_0)
        view$resetTransform()
      ## if(event$key() == Qt$Qt$Key_u)
      ##    viewInUCSC(obj)
    }}
  ## preset the level
  zoomLevel.sr <- c(5000)
  zoomLevel.cov <- c(5000)
  ## FIXME: need to fix this bug, naming in Rsamtools
  if(nchar(seqname)>1)
    seqname <- as.numeric(substr(seqname,4,4))
  ## precessing data
  bam <- bam[[1]]
  ir <- data.frame(start=bam$pos,width=width(bam$seq),
                   strand=bam$strand)
  ir <- GRanges(seqnames=seqname,
                ranges=IRanges(start=ir$start, width=ir$width),
                strand=ir$strand)

  ir.pos <- ir[strand(ir)=="+"]
  ir.neg <- ir[strand(ir)=="-"]
  covg <- coverage(ranges(ir))
  
  ir.v <- slice(covg,lower=10)
  xpos <- viewWhichMaxs(ir.v)
  ypos <- viewMaxs(ir.v)
  
  y.cut <- quantile(ypos,0.5)
  y.cut <- ifelse(y.cut>=10,y.cut,10)
  
  ## postive
  cov.pos <- coverage(ranges(ir.pos))
  idx <- runValue(cov.pos)>y.cut
  cov.pos.val <- runValue(cov.pos)
  cov.pos.valt <- rep(cov.pos.val,each=2)
  cov.pos.len <- runLength(cov.pos)
  cov.pos.xpos <- cumsum(cov.pos.len)
  cov.pos.xpost <- sort(c(cov.pos.xpos-cov.pos.len+1,cov.pos.xpos),decreasing=FALSE)
  cov.pos.valt <- cov.pos.valt[rep(idx,each=2)]
  cov.pos.xpost <- cov.pos.xpost[rep(idx,each=2)]

  ## negative
  cov.neg <- coverage(ranges(ir.neg))
  idx <- runValue(cov.neg)>y.cut
  cov.neg.val <- runValue(cov.neg)
  cov.neg.valt <- rep(cov.neg.val,each=2)
  cov.neg.len <- runLength(cov.neg)
  cov.neg.xpos <- cumsum(cov.neg.len)
  cov.neg.xpost <- sort(c(cov.neg.xpos-cov.neg.len+1,cov.neg.xpos),decreasing=FALSE)
  cov.neg.valt <- cov.neg.valt[rep(idx,each=2)]
  cov.neg.xpost <- cov.neg.xpost[rep(idx,each=2)]

  pfunAlign <- function(layer,painter,exposed){
    xlimZoom <- as.matrix(exposed)[,1]
    pars$xlimZoom <<- xlimZoom
    if(diff(xlimZoom)>zoomLevel[1]){
      ## ## qdrawSegment(painter,xpos,log(ypos),xpos,0)
      ## sr.layer$setLimits(qrect(range(xpos),c(0,max(log(ypos)))))
    }
    if(diff(xlimZoom)<=zoomLevel.sr[1]){
      idxs <- findOverlaps(ranges(ir),IRanges(xlimZoom[1],xlimZoom[2]))
      idxs <- idxs@matchMatrix[,1]
      idxs <- unique(idxs)
      if(length(idxs)>1){
        ir <- ir[idxs]
        binir <- disjointBins(ranges(ir))
        binmx <- max(binir*90+80)
        x0 <- start(ir)
        y0 <- -binir*90
        x1 <- end(ir)
        y1 <- -(binir*90+80)
        idxp <- as.logical(strand(ir)=="+")
        idxn <- as.logical(strand(ir)=="-")
        ## draw title
        ## qdrawText(painter,"ShortRead Alighment",sum(xlimZoom)/2,
        ##       0,"center","botom",color=pars$textColor)

        if(sum(idxp)>0){
          qdrawRect(painter,x0[idxp],y0[idxp],x1[idxp],y1[idxp],
                    stroke=rgb(0,0,1,0.8),fill=rgb(0,0,1,0.8))
        }
        if(sum(idxn)>0){
          qdrawRect(painter,x0[idxn],y0[idxn],x1[idxn],y1[idxn],
                    stroke=rgb(0,1,0,0.8),fill=rgb(0,1,0,0.8))
        }
      }}
  }
  
  pfunCov <- function(layer,painter,exposed){
    xlimZoom <- as.matrix(exposed)[,1]
    pars$xlimZoom <<- xlimZoom
    ## ## positive coverage
    if(diff(xlimZoom)>zoomLevel.cov[1])
    qdrawLine(painter,cov.pos.xpost,log10(cov.pos.valt),stroke="green")
    ## ## negative coverage    
    qdrawLine(painter,cov.neg.xpost, log10(cov.neg.valt),stroke="blue")
    ## draw title
    qdrawText(painter,"Coverage",sum(xlimZoom)/2,
              max(log10(ypos)),"center","bottom",color=pars$textColor)

  }
  rlayer <- qlayer(rootLayer, 
                   row=row,
                   col=col,
                   rowSpan=rowSpan,
                   colSpan=colSpan)
  cov.layer <- qlayer(rlayer,paintFun=pfunCov,
                      limits=qrect(range(xpos),c(0,max(log10(ypos)))),
                      wheelFun=wheelZoom,keyPressFun = keyPressEvent)
  sr.layer <- qlayer(rlayer,paintFun=pfunAlign,row=1L,
                     limits=qrect(range(xpos),c(-(cutbin*90+80),0)),
                     cache=FALSE,
                     wheelFun=wheelZoom)
  rlayer$setGeometry(0,0,600,150)
  layout <- rlayer$gridLayout()
  layout$setRowPreferredHeight(0,30)
  layout$setRowPreferredHeight(1,120)
})




AlignmentView.gen$methods(show = function(){
  view$show()
})

setMethod("print","AlignmentView",function(x,..){
  x$show()
})

