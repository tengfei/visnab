##=================================================================##
##           Define all the methods for 'StackedView'
##=================================================================##

##-----------------------------------------------------------------##
##                  FOR class 'StackedView'
##-----------------------------------------------------------------##

StackedView.gen <- setRefClass("StackedView",contains="QtVisnabView",
                               fields=c(track="MutableGRanges",
                                 pos.hover="numericORNULL",
                                 pos.press="numericORNULL",
                                 pos.move="numericORNULL",
                                 pos.right="numericORNULL",
                                 pos.release="numericORNULL",
                                 signalingField("hotspot",
                                                "MutableGRanges"),
                                 signalingField("hotspotColor","character")))

StackedView <- function(track,species=NULL,cytoband=FALSE,subchr=NULL,
                        scene=NULL,seqname=NULL,
                        hotspot=NULL,hotspotColor="red",...){
  if(is(track,"GRanges"))
    track <- as(track,"MutableGRanges")
  if(is.null(hotspot))
    hotspot <- MutableGRanges()
  if(is(hotspot,"GRanges"))
    hotspot <- as(hotspot,"MutableGRanges")
  if(is.null(scene)){
    scene <- qscene()
    view <- qplotView(scene,rescale="none")
    rootLayer <- qlayer(scene,geometry=qrect(0,0,800,600))
  }
  if(is.null(seqname))
    seqname <- as.character(unique(as.character(seqnames(track)))[1])
  pars <- GraphicPars(seqname = seqname, cpal = blackred_pal(),
                      dpal = brewer_pal(),
                      view = "StackedView")
  obj <- StackedView.gen$new(track=track,pars=pars,scene=scene,
                             view=view,rootLayer=rootLayer,
                             hotspot=hotspot,
                             hotspotColor=hotspotColor)
  ## connected events
  obj$hotspotChanged$connect(function(){qupdate(obj$scene)})
  obj$hotspotColorChanged$connect(function(){qupdate(obj$scene)})
  obj$pars$seqnameChanged$connect(function(){qupdate(obj$scene)})
  obj$createView()
  obj
}


StackedView.gen$methods(createView = function(seqname=NULL){
  bgcol <- pars$bgColor
  bgalpha <- pars$alpha
  qcol <- col2qcol(bgcol,bgalpha)
  scene$setBackgroundBrush(qbrush(qcol))
  mx <- max(end(track))
  width <- wids <- 12
  scale <- scs <- 400
  mars <- mars <- c(50,50,40,20)
  skip.factor <- spf <- 25
  bg.col <- 'white'
  chrText.width <- chw <- 40
  bg.alpha <- 1
  refMap <- lapply(1:length(track),function(i){
    x0 <- chw+mars[2]
    x1 <- end(track[i])/mx*scs+mars[2]+chw
    y0 <- mars[3]+spf*i
    y1 <- mars[3]+spf*i+wids
    data.frame(x0=x0,x1=x1,y0=y0,y1=y1)
  })
  refMap <- as.data.frame(do.call('rbind',refMap))
  refMap$seqname <- as.character(seqnames(track))
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
  ## need some transformation function here
  chromToLayer <- function(x){
    pos <- mars[2]+chw+x/mx*scs
  }
  pfun <- function(layer,painter){
    chrs <- seqnames(gr)
    ## FIXME: need to be vectorized
    for(i in 1:length(chrs)){
      qdrawText(painter,chrs[i],mars[2],mars[3]+spf*i+wids/2)
      qdrawRect(painter, refMap$x0, refMap$y0, refMap$x1, refMap$y1)
    }}
  ## hot spot pfun, could be replaced and defined by user
  hotspotPfun <- function(layer,painter){
    if(length(hotspot)>0){
    sts <- start(hotspot)
    sts <- chromToLayer(sts)
    eds <- end(hotspot)
    eds <- chromToLayer(eds)
    chrs <- as.character(seqnames(hotspot))
    if((as.character(hotspotColor)%in% names(elementMetadata(hotspot)))){
      cols.value <- elementMetadata(hotspot)[[hotspotColor]]
      if(is.numeric(cols.value)){
        cols <- cscale(cols.value, pars$cpal)
      }else{
        cols <- dscale(factor(cols.value), pars$dpal)
      }}else{
        cols <- hotspotColor
        cols <- rep(cols,length(hotspot))
      }
      idx <- match(chrs,as.character(refMap$seqname))
      idx.naomit <- na.omit(idx)
      idx.na <- is.na(idx)
    cols <- cols[!idx.na]
    cat(unique(chrs[idx.na]),"cannot be mapped and ignored\n")
    qdrawRect(painter,sts[!idx.na],refMap[idx.naomit,'y0']-2,eds[!idx.na],
              refMap[idx.naomit,'y1']+2,stroke=NA,fill=cols)
  }
  }
  
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
    chrom <- pars$seqname
    ref <- refMap[refMap$seqname==chrom,]
    qdrawRect(painter,ref[,"x0"]-5,ref[,'y0']-spf/4,
              ref[,'x1']+5,ref[,'y1']+spf/4,stroke="blue")
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
    }
  }
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
      if(!is.null(pos.right)){
        isin <- isInsideChrom(pos.right)
        if(isin$isin){
          chrom <- isin$chrom
          pars$seqname <<- paste("chr",chrom,sep="")
        }
      }
      ## qupdate(selectedChromLayer)
    }
  }

  lmts <- qrect(0,0,mars[2]+chw+scs+mars[4],
                mars[3]+mars[1]+
                spf*length(seqnames(gr))+wids)
  ## layer for drawing chromosomes
  chromLayer <- qlayer(rootLayer,paintFun=pfun,
                       cache=TRUE,
                       limits=lmts)
    ## layer for coordinate when mouse hover
  hotlineLayer <- qlayer(rootLayer,paintFun=hotlinePfun,
                         hoverMoveFun=mouseHover,cache=FALSE,limits=lmts)

    ## hotspot
  hotspotLayer <- qlayer(rootLayer,paintFun=hotspotPfun,
                         cache=FALSE,limits=lmts)

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




## show supported geoms
setMethod("Geom","StackedView",function(x,...){
  geoms <- options("BioC")$bioc$visnab$StackedView$geom
  if(!is.null(geoms))
    print(geoms)
  else
    message("No supported geom is found for this object")
})


##
StackedView.gen$methods(regSignal = function(){
  pars$strokeChanged$connect(function(){
    qupdate(scene)
  })
  pars$fillChanged$connect(function(){
    values(.self@track)$.color <- .self@pars$fill
  })
  pars$geomChanged$connect(function(){
    qupdate(scene)
  })
  pars$bgColorChanged$connect(function(){
    bgcol <- .self@pars$bgColor
    bgalpha <- .self@pars$alpha
    qcol <- col2qcol(bgcol,bgalpha)
    scene$setBackgroundBrush(qbrush(qcol))
  })
  pars$ylimChanged$connect(function(){
    rootLayer[0,0]$setLimits(qrect(pars$xlim[1], pars$ylim[1],
                                   pars$xlim[2], pars$ylim[2]))
  })
  pars$seqnameChanged$connect(function(){
    start <- 0
    end <- max(end(ranges(track[seqnames(track)==pars$seqname])))
    pars$xlimZoom <<- c(start,end)
    rootLayer$close()
    rootLayer <<- qlayer(scene,geometry=qrect(0,0,800,600),row=row)
    view$resetTransform()
    .self$createView()
  })
})




