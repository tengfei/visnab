##----------------------------------------------------------------------------##
##             For class "AlignmentView"
##----------------------------------------------------------------------------##
AlignmentView.gen <- setRefClass("AlignmentView",contains="QtVisnabView",
                                 fields=list(track="list",
                                   lower="numeric",
                                   cutbin="numeric"))

##----------------------------------------------------------------------##
##             "AlignmentView" constructor
##----------------------------------------------------------------------##

AlignmentView <- function(bam=NULL,
                          lower=10L,
                          cutbin=20L,
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
    seqname <- unlist(strsplit(names(bam),":"))[1]
    start <- 0
    end <- as.numeric(unlist(strsplit(unlist(strsplit(names(bam),":"))[2],"-"))[2])
  }
  xlimZoom <- c(start,end)
  if(is.null(scene)){
    scene=qscene()
    view = qplotView(scene,rescale="none")
    rootLayer = qlayer(scene,geometry=qrect(0,0,800,600))
  }
  pars <- GraphicPars(xlimZoom = xlimZoom, seqname = seqname)
  obj <- AlignmentView.gen$new(track=bam,title=title,
                               row=row,col=col, rowSpan = rowSpan, colSpan = colSpan,
                               scene=scene,view=view,rootLayer=rootLayer,
                               pars=pars, lower=lower,cutbin=cutbin)
  obj$createView()
  obj
}


##---------------------------------------------------------##
##             print method
##---------------------------------------------------------##

AlignmentView.gen$methods(createView = function(seqname = NULL){
  if(!is.null(seqname))
    pars$seqname <<- seqname
  seqname <- pars$seqname
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
  zoomLevel <- c(5000)
  ## need to fix this bug
  if(nchar(seqname)>1)
    seqname <- as.numeric(substr(seqname,4,4))
  ## precessing data
  bam <- track[[seqname]]
  ir <- data.frame(start=bam$pos,width=width(bam$seq),
                   strand=bam$strand)
  ir <- GRanges(seqnames=seqname,
                ranges=IRanges(start=ir$start, width=ir$width),
                strand=ir$strand)

  ir.pos <- ir[strand(ir)=="+"]
  ir.neg <- ir[strand(ir)=="-"]
  covg <- coverage(ranges(ir))
  ir.v <- slice(covg,lower=lower)
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
    if(diff(xlimZoom)<=zoomLevel[1]){
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

