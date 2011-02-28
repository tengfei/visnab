##=================================================================##
##           Define all the methods for 'StackedView'
##=================================================================##

##-----------------------------------------------------------------##
##                  FOR class 'StackedView'
##-----------------------------------------------------------------##
setClass("StackedView",contains="GraphicPars",
         representation(track="GenomicRanges",
                        species="characterOrNULL"))
## This one is used for bird-eye overview
## Cytobands is not quite useful and also reduce the mapping speed.

## not support cytoband temoprarily.
StackedView <- function(species=NULL,cytoband=FALSE,subchr=NULL,...){
  pars <- GraphicPars(...)@pars
  gr <- getIdeogram(species,subchr=subchr)
  ## if(!cytoband)
  ##   gr <- reduceChr(gr)
  mx <- max(end(gr))
  pars$mx <- mx
  pars$width <- wids <- 12
  pars$scale <- scs <- 400
  pars$mars <- mars <- c(50,50,40,20)
  pars$skip.factor <- spf <- 25
  pars$bg.col <- 'white'
  pars$chrText.width <- chw <- 40
  pars$bg.alpha <- 1
  refMap <- lapply(1:length(gr),function(i){
    x0 <- chw+mars[2]
    x1 <- end(gr[i])/mx*scs+mars[2]+chw
    y0 <- mars[3]+spf*i
    y1 <- mars[3]+spf*i+wids
    data.frame(x0=x0,x1=x1,y0=y0,y1=y1)
  })
  pars$refMap <- do.call('rbind',refMap)
  obj <- new('StackedView',track=gr,species=as.character(species),pars=pars)
  obj
}

setMethod('visplot','StackedView',function(obj,..){
  scene <- qscene()
  gr <- obj@track
  mx <- max(end(gr))
  spf <- obj@pars$skip.factor
  scs <- obj@pars$scale
  wids <- obj@pars$width
  mars <- obj@pars$mars
  chw <- obj@pars$chrText.width
  ## define local variables
  pos.hover <- NULL
  pos.press <- NULL
  pos.move <- NULL
  pos.right <- NULL
  refMap <- obj@pars$refMap
  ## function utils
    pos2gene <- function(pos){
    (pos-chw-mars[2])/scs*mx
  }
  isInsideChrom <- function(pos){
    if(!is.null(pos)){
    isinside <- sapply(1:nrow(refMap),function(i) {
      pos[1]<=refMap[i,'x1']&&
      pos[1]>=refMap[i,'x0']&&
      pos[2]<=refMap[i,'y1']&&
      pos[2]>=refMap[i,'y0']
    })
    if(isin <- any(isinside)){
      chrom <- which(isinside)
      return(data.frame(isin=isin, chrom=chrom))
    }else{
      return(data.frame(isin=isin))
    }
  }
  }
  ## painter
  pfun <- function(layer,painter){
    chrs <- seqnames(gr)
   for(i in 1:length(chrs)){
      qdrawText(painter,chrs[i],mars[2],mars[3]+spf*i+wids/2)
      qdrawRect(painter,0+chw+mars[2],mars[3]+spf*i,
                mars[2]+chw+end(gr[seqnames(gr)==chrs[i]])/mx*scs,
                mars[3]+wids+spf*i)
  }}

  hotlinePfun <- function(layer,painter){
    if(!is.null(pos.hover)){
      isin <- isInsideChrom(pos.hover)
      if(isin$isin){
        idx <- isin$chrom
        qdrawSegment(painter,pos.hover[1],refMap[idx,'y0'],pos.hover[1],refMap[idx,'y1'],stroke="red")
        qdrawText(painter,as.integer(pos2gene(pos.hover[1])),pos.hover[1],refMap[idx,'y0'],valign="bottom")
      }
    }
  }
  selectedChromPfun <- function(layer,painter){
    if(!is.null(pos.right)){
      isin <- isInsideChrom(pos.right)
      if(isin$isin){
        chrom <- isin$chrom
        qdrawRect(painter,refMap[chrom,'x0']-5,refMap[chrom,'y0']-spf/4,
                  refMap[chrom,'x1']+5,refMap[chrom,'y1']+spf/4,stroke="blue")
      }
    }
  }
  
  hotRegionPfun <- function(layer,painter){
    if(!is.null(pos.press)){
      isin <- isInsideChrom(pos.press)
      if(isin$isin){
        chrom<-isin$chrom
        topY <- refMap[chrom,'y1']
        bottomY <- refMap[chrom,'y0']
        qdrawRect(painter,pos.press[1],topY,pos.move[1],bottomY,fill=rgb(0,0,1,0.5))
    }
  }}
  ## events
  ## mouseHover for hotline
  mouseHover <- function(layer,event){
    pos.hover <<- as.numeric(event$pos())
    qupdate(hotlineLayer)
  }
  hotRegionMove <- function(layer,event){

    pos.press <<- as.numeric(event$buttonDownPos(Qt$Qt$LeftButton))
    pos.move <<- as.numeric(event$pos())
    qupdate(hotRegionLayer)

}
  hotRegionRelease <- function(layer,event){
    pos.release <<- as.numeric(event$pos())[1]
    qupdate(hotRegionLayer)
  }
  rightSelectFun <- function(layer,event){
    if(event$buttons()==2){
    pos.right <<- as.numeric(event$buttonDownPos(Qt$Qt$RightButton))
    qupdate(selectedChromLayer)
  }
  }

  lmts <- qrect(0,0,mars[2]+chw+scs+mars[4],
                mars[3]+mars[1]+
                spf*length(seqnames(gr))+wids)
  ## basic layer
  rootLayer <- qlayer(scene,geometry=qrect(0,0,600,400))
  ## layer for drawing chromosomes
  chromLayer <- qlayer(rootLayer,paintFun=pfun,
                       cache=TRUE,
                       limits=lmts)
  ## layer for coordinate when mouse hover
  hotlineLayer <- qlayer(rootLayer,paintFun=hotlinePfun,
                         hoverMoveFun=mouseHover,cache=FALSE,limits=lmts)
  ## layer for range selection
  hotRegionLayer <- qlayer(rootLayer,paintFun=hotRegionPfun,
                           mouseMoveFun=hotRegionMove,
                           mouseReleaseFun=hotRegionRelease,
                           mousePressFun=rightSelectFun,
                           cache=FALSE,limits=lmts)
      ## layer for selected chromosome
  selectedChromLayer <- qlayer(rootLayer,paintFun=selectedChromPfun,
                               limits=lmts,cache=FALSE)
  view <- qplotView(scene)
  view$show()
})


##     l.line<<-qlayer(bird.root,paintFun=hotLine,hoverMoveFun=mouseHover)
##     l.hot <<- qlayer(bird.root,paintFun=hotSpot2,
##                      mouseDoubleClickFun=hotDBclick,
##                      mousePressFun=mouseRightClick,
##                       mouseMoveFun=hotRegionMove,
##                       mouseReleaseFun=hotRegionRelease,
##                      cache=TRUE)
##     l.region<<-qlayer(bird.root,paintFun=hotRegionPaint)
##   if(FALSE){
##     l.den<<-qlayer(bird.root,paintFun=widerange,cache=FALSE)
##   }
##   s.bird$setBackgroundBrush(bgc)
##   view.bird <<- qplotView(s.bird,rescale='transform')
## })


## ## for wideRange layer
## wideRange <- function(layer,painter){
##   df <- as.data.frame(pframe)
##   ## need to sort them first
##   den.cutoff <- min(as.numeric(as.character(pframe$den[pframe$isSelected])))
##   dfsub <- subset(df,df$only)       #leave only interesting region
##   dfsub$logics <- as.integer(dfsub$den>den.cutoff)
##   widerange <- sparseby(dfsub,dfsub$chromosome,function(x){
##     diffs <- diff(c(0,x$logics))
##     x$diff <- diffs
##     start <- x$start[x$diff==1]
##     end <- x$end[which(x$diff==-1)-1]
##     if(length(start)==length(end)+1){
##       end <- c(max(x$end),end)
##     }
##     ir <- IRanges(start=start,end=end)
##     irr <- IRanges::reduce(ir)
##     start <- start(irr)
##     end <- end(irr)
##     nstart <- length(start)
##     data.frame(chromosome=rep(unique(x$chromosome)[1],nstart),start=start,end=end)

##    })
##   widerange <- widerange[,-1]
##   hotrange <- apply(widerange,1,chr2loc.cb)
##   mapTopLeftX <- hotrange[1,]
##   mapBottomRightX <- hotrange[3,]
##   mapTopLeftY <- hotrange[2,]
##   mapBottomRightY <- hotrange[4,]
##   if(filter.num>1){
##       hotden<<-NULL
##   }else{
##   hotden<<-data.frame(topLeftX=mapTopLeftX,
##                       topLeftY=mapTopLeftY,
##                       bottomRightX=mapBottomRightX,
##                       bottomRightY=mapBottomRightY,
##                       chromosome=widerange$chromosome,
##                       start=widerange$start,
##                       end=widerange$end)
## }
## }

## ##----------------------------------------------------------------##
## ##         Events
## ##----------------------------------------------------------------##


## hotDBclick <- function(layer,event){
##   pframe<-scatterObj@mutaframe
##   pos <- as.numeric(event$pos())
##   if(is.null(hotden)){
##     if(any(pframe$isSelected)){
##       iscloseto <- isCloseToHot(pos,hotRegion,isshift=isshift)
##       if(any(iscloseto)){
##         hits <- which(iscloseto)[1]
##         chr <- pframe$chromosome[hits]
##         zoomObj <<- genZoomBrowserDb('Mus musculus',chr,
##                                      region_start=pframe$start[hits],
##                                      region_end=pframe$end[hits])
##         scatterObj@mutaframe$isHighlighted<<-FALSE
##         scatterObj@mutaframe$isHighlighted[hits]<<-TRUE
##         scatterObj@mutaframe$isHoverSelected<<-FALSE
##         scatterObj@mutaframe$isHoverSelected[hits]<<-TRUE
##         viewZoomTrack$resetTransform()
##         qupdate(viewZoomTrack)
##         qupdate(viewZoomChrom)
##         qupdate(layerIsland)
##        qupdate(l.red)
##       }}
##   }else{
##     iscloseto.den <- isCloseToHot.den(pos,hotden,isshift=FALSE)
##     if(any(iscloseto.den)){
##       hits <- which(iscloseto.den)[1]
##       chr <- hotden$chromosome[hits]
##       zoomObj <<- genZoomBrowserDb('Mus musculus',chr,
##                                    region_start=hotden$start[hits],
##                                    region_end=hotden$end[hits])
##       qupdate(viewZoomTrack)
##       qupdate(viewZoomChrom)
##       ##    qupdate(layerIsland)
##       ##chipSeqPlot.den(hotden,r1,r2,id,lower=gpars$lower)
##     }
##   }
## }



## isCloseToHot <- function(pos,hotrg,mar=c(0,5,0,5),isshift=isshift){
##   hot <- hotrg
##   width <- birdObj@pars$width
##   if(!isshift){
##     hot[1,] <- hot[1,]-mar[2]
##     hot[2,] <- hot[2,]-mar[3]
##     hot[3,] <- hot[3,]+mar[4]
##     hot[4,] <- hot[4,]+mar[1]
##   }else{
##     hot[1,] <- hot[1,]-mar[2]
##     hot[2,] <- hot[2,]-mar[3]-width
##     hot[3,] <- hot[3,]+mar[4]
##     hot[4,] <- hot[4,]+mar[1]-width
##   }
##   isclose <- apply(hot,2,function(x){
##     isInside(pos,c(x[1],x[2]),c(x[3],x[4]))
##   })
##   id <- isclose&pframe$isSelected
##   id
## }


## isCloseToHot.den <- function(pos,hotrg,mar=c(0,0,0,0),isshift=isshift){
##   hot <- hotrg
##   width <- birdObj@pars$width
##   if(!isshift){
##     hot[,1] <- hot[,1]-mar[2]
##     hot[,2] <- hot[,2]-mar[3]
##     hot[,3] <- hot[,3]+mar[4]
##     hot[,4] <- hot[,4]+mar[1]
##   }else{
##     hot[,1] <- hot[,1]-mar[2]
##     hot[,2] <- hot[,2]-mar[3]-width
##     hot[,3] <- hot[,3]+mar[4]
##     hot[,4] <- hot[,4]+mar[1]-width
##   }
##   isclose <- sparseby(hot,1:nrow(hot),function(x){
##     isInside(pos,c(x$topLeftX,x$topLeftY),c(x$bottomRightX,x$bottomRightY))
##   })
##   id <- isclose[,2]
##   id
## }



