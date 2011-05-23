##=================================================================##
##           Define all the methods for 'StackedView'
##=================================================================##

##-----------------------------------------------------------------##
##                  FOR class 'StackedView'
##-----------------------------------------------------------------##

StackedView.gen <- setRefClass("StackedView",contains = "QtVisnabView",
                               fields=list(track = "MutableGRanges",
                                 pos.hover = "numericORNULL",
                                 pos.press = "numericORNULL",
                                 pos.move = "numericORNULL",
                                 pos.right = "numericORNULL",
                                 pos.release = "numericORNULL",
                                 selectedRange = "MutableGRanges"))

StackedView <- function(track,species = NULL, seqname = NULL, geom = "blank",
                        scene=NULL, view = NULL, rootLayer = NULL, thisLayer = NULL,
                        selectedRangesModel=NULL,selectedRangesModelColor="red",
                        selectedRange = NULL,...){
  if(is(track,"GRanges"))
    track <- as(track,"MutableGRanges")
  if(is.null(selectedRangesModel))
    selectedRangesModel <- MutableGRanges()
  if(is(selectedRangesModel,"GRanges"))
    selectedRangesModel <- as(selectedRangesModel,"MutableGRanges")
  if(is.null(seqname))
    seqname <- as.character(unique(as.character(seqnames(track)))[1])
  pars <- GraphicPars(seqname = seqname, geom = geom, view = "StackedView")
  obj <- StackedView.gen$new(track = track,pars = pars,scene = scene,
                             view = view,rootLayer = rootLayer,
                             thisLayer = thisLayer,
                             selectedRangesModel=selectedRangesModel,
                             selectedRangesModelColor=selectedRangesModelColor,
                             selectedRange = MutableGRanges())
  obj$createView()
  obj$regSignal()
  obj
}


StackedView.gen$methods(createView = function(seqname=NULL){
  if(is.null(scene)){
    scene <<- qscene()
    view <<- qplotView(scene,rescale="none")
    rootLayer <<- qlayer(scene,geometry=qrect(0,0,800,600))
  }

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
    chrs <- seqnames(track)
    ## FIXME: need to be vectorized
    for(i in 1:length(chrs)){
      qdrawText(painter,chrs[i],mars[2],mars[3]+spf*i+wids/2)
      qdrawRect(painter, refMap$x0, refMap$y0, refMap$x1, refMap$y1)
    }}
  ## hot spot pfun, could be replaced and defined by user
  selectedRangesModelPfun <- function(layer,painter){
    if(length(selectedRangesModel)>0){
    sts <- start(selectedRangesModel)
    sts <- chromToLayer(sts)
    eds <- end(selectedRangesModel)
    eds <- chromToLayer(eds)
    chrs <- as.character(seqnames(selectedRangesModel))
    if((as.character(selectedRangesModelColor)%in%
        names(elementMetadata(selectedRangesModel)))){
      cols.value <- elementMetadata(selectedRangesModel)[[selectedRangesModelColor]]
      if(is.numeric(cols.value)){
        cols <- cscale(cols.value, pars$cpal)
      }else{
        cols <- dscale(factor(cols.value), pars$dpal)
      }}else{
        cols <- selectedRangesModelColor
        cols <- rep(cols,length(selectedRangesModel))
      }
      idx <- match(chrs,as.character(refMap$seqname))
      idx.naomit <- na.omit(idx)
      idx.na <- is.na(idx)
    cols <- cols[!idx.na]
    if(sum(idx.na)>0)
      cat(unique(chrs[idx.na]),"cannot be mapped and ignored\n")
    qdrawRect(painter,sts[!idx.na],refMap[idx.naomit,'y0']-2,eds[!idx.na],
              refMap[idx.naomit,'y1']+2,stroke=cols,fill=NULL)
  }
  }
  
  hotlinePfun <- function(layer,painter){
    if(!is.null(pos.hover)){
      isin <- isInsideChrom(pos.hover)
      if(isin$isin){
        idx <- isin$chrom
        qdrawSegment(painter,pos.hover[1],refMap[idx,'y0'],
                     pos.hover[1],refMap[idx,'y1'],stroke="red")
        qdrawText(painter,as.integer(pos2gene(pos.hover[1])),
                  pos.hover[1],refMap[idx,'y0'],valign="bottom")
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
          chrom <- refMap[chrom, "seqname"]
          pars$seqname <<- chrom
        }
      }
    }
  }
  ## double click on the hot range
  dbclick <- function(layer, event){
    pos <- event$pos()
    rect <- qrect(0,0,2,2)
    mat <- layer$deviceTransform(event)$inverted()
    rect <- mat$mapRect(rect)
    rect$moveCenter(pos)
    hits <- layer$locate(rect)+1
    if(length(hits)>0){
      hits <- hits[1]
      ## elementMetadata(selectedRangesModel)$.selected[hits] <- TRUE
      ## print(selectedRangesModel[hits])
      selectedRange <<- selectedRangesModel[hits]
    }
  }
  lmts <- qrect(0,0,mars[2]+chw+scs+mars[4],
                mars[3]+mars[1]+
                spf*length(seqnames(track))+wids)

  ## layer for drawing chromosomes
  chromLayer <- qlayer(rootLayer,paintFun=pfun, cache=FALSE, limits=lmts)

  ## layer for coordinate when mouse hover
  hotlineLayer <- qlayer(rootLayer,paintFun=hotlinePfun,
                         hoverMoveFun=mouseHover,cache=FALSE,limits=lmts)


  ## selectedRangesModel
  selectedRangesModelLayer <- qlayer(rootLayer,paintFun=selectedRangesModelPfun,
                                     mouseDoubleClickFun = dbclick,

                                     cache=FALSE,limits=lmts)

  ## ## layer for range selection
  ## hotRegionLayer <- qlayer(rootLayer,paintFun=hotRegionPfun,
  ##                          mouseMoveFun=hotRegionMove,
  ##                          mouseReleaseFun=hotRegionRelease,
  ##                          mousePressFun=rightSelectFun,
  ##                          cache=FALSE,limits=lmts)
  ## layer for selected chromosome
  ## selectedChromLayer <- qlayer(rootLayer,paintFun=selectedChromPfun,
  ##                              limits=lmts,cache=FALSE, mousePressFun=rightSelectFun)

})

StackedView.gen$methods(show = function(){
  view$show()
})

setMethod("print","StackedView",function(x,..){
  x$show()
})




## show supported geoms
setMethod("geom","StackedView",function(x,...){
  cat("Choosed geom: ",x$pars$geom,"\n")
  cat("---------------------\n")
  cat("Supported geoms: \n")
  geoms <- getOption("BioC")$visnab$StackedView$geom
  if(!is.null(geoms))
    cat(geoms,"\n")
  else
    message("No supported geom is found for this object")
})

setReplaceMethod("geom","StackedView", function(x,value){
                   geoms <- getOption("BioC")$visnab$StackedView$geom
                   if(!(value %in% geoms))
                     stop("Geom should be one of", geoms)
                   else
                     x$pars$geom <- value
                   x
                 })



##
StackedView.gen$methods(regSignal = function(){
  pars$geomChanged$connect(function(){
    qupdate(scene)
  })
  pars$bgColorChanged$connect(function(){
    bgcol <- pars$bgColor
    bgalpha <- pars$alpha
    qcol <- col2qcol(bgcol,bgalpha)
    scene$setBackgroundBrush(qbrush(qcol))
  })
  selectedRangesModelChanged$connect(function(){qupdate(scene)})
  selectedRangesModelColorChanged$connect(function(){qupdate(scene)})
  pars$seqnameChanged$connect(function(){qupdate(scene)})
})




