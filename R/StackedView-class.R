##=================================================================##
##           Define all the methods for 'StackedView'
##=================================================================##

##-----------------------------------------------------------------##
##                  FOR class 'StackedView'
##-----------------------------------------------------------------##
StackedView.gen <- setRefClass("StackedView",contains="QtVisnabView",
                               fields=list(track="MutableGRanges",
                                 pos.hover="numericORNULL",
                                 pos.press="numericORNULL",
                                 pos.move="numericORNULL",
                                 pos.right="numericORNULL",
                                 pos.release="numericORNULL"))

## This one is used for bird-eye overview
## Cytobands is not quite useful and also reduce the mapping speed.

## not support cytoband temoprarily.
StackedView <- function(mr,species=NULL,cytoband=FALSE,subchr=NULL,
                        scene=NULL,seqname=NULL,...){
  if(extends(class(mr),"GRanges"))
    mr <- as(mr,"MutableGRanges")
  if(is.null(scene)){
    scene <- qscene()
    view <- qplotView(scene,rescale="none")
    rootLayer <- qlayer(scene,geometry=qrect(0,0,800,600))
  }
  if(is.null(seqname))
    seqname <- as.character(unique(as.character(seqnames(mr)))[1])
  pars <- GraphicPars(seqname=seqname)
  obj <- StackedView.gen$new(track=mr,pars=pars,
                             scene=scene,view=view,rootLayer=rootLayer)
  obj$createView()
  obj
}

StackedView.gen$methods(createView = function(seqname=NULL){
  bgcol <- pars$bgColor
  bgalpha <- pars$alpha
  qcol <- col2qcol(bgcol,bgalpha)
  scene$setBackgroundBrush(qbrush(qcol))
  mr <- track
  mx <- max(end(mr))
  width <- wids <- 12
  scale <- scs <- 400
  mars <- mars <- c(50,50,40,20)
  skip.factor <- spf <- 25
  bg.col <- 'white'
  chrText.width <- chw <- 40
  bg.alpha <- 1
  refMap <- lapply(1:length(mr),function(i){
    x0 <- chw+mars[2]
    x1 <- end(mr[i])/mx*scs+mars[2]+chw
    y0 <- mars[3]+spf*i
    y1 <- mars[3]+spf*i+wids
    data.frame(x0=x0,x1=x1,y0=y0,y1=y1)
  })
  refMap <- do.call('rbind',refMap)
  ## define local variables
  pos.hover <<- NULL
  pos.press <<- NULL
  pos.move <<- NULL
  pos.right <<- NULL
  pos.release <<- NULL
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
        pars$seqname <<- paste("chr",chrom,sep="")
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
  ## ## basic layer
  ## rootLayer <- qlayer(scene,geometry=qrect(0,0,600,400))
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

})

StackedView.gen$methods(show = function(){
  view$show()
})

setMethod("print","StackedView",function(x,..){
  x$show()
})



