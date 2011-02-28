##----------------------------------------------------------------------------##
##                     Classes
##----------------------------------------------------------------------------##

setClass('CircularView',contains='GraphicPars',
         representation(grandShape='characterOrNULL',
                        tracks='list'),
         prototype(elementType='GenomicRanges'))

##----------------------------------------------------------------------------##
##                     Generic  Methods
##----------------------------------------------------------------------------##
##' <description>
##'
##' <details>
##' @title 
##' @param obj 
##' @param ... 
##' @return 
##' @author tengfei
##' @export
setGeneric('visplot',function(obj,...) standardGeneric('visplot'))

##----------------------------------------------------------------------------##
##               Constructor for CircularView
##----------------------------------------------------------------------------##
##' <description>
##'
##' <details>
##' @title 
##' @param grl 
##' @param model 
##' @param chrOrder 
##' @param tracksType 
##' @param tracksOrder 
##' @param isPlotted 
##' @param tracksColorTheme 
##' @param tracksWidth 
##' @param ... 
##' @return 
##' @author tengfei
##' @export
CircularView <- function(grl,
                         model,
                         chrOrder=NULL,
                         tracksType=NULL,
                         tracksOrder=NULL,
                         isPlotted=NULL,
                         tracksColorTheme=NULL,
                         tracksWidth=NULL,
                         ...){
  N <- length(grl)
  if(is.null(tracksType)){
    tracksType <- rep(NA,N)
  }else{
    if(length(tracksType)!=length(grl))
      stop('You have to specify types for each tracks')
  }
  if(is.null(tracksOrder))
    tracksOrder <- seq_len(N)
  ## reorder it based on tracksOrder
  grl <- grl[order(tracksOrder,decreasing=FALSE)]
  ##
  if(is.null(tracksColorTheme)){
    tracksColorTheme <- as.list(rep('default',N))
  }
  ## add levels to all the list
  grl <- lapply(grl,function(gr){
    addLevels(gr)
  })
  ## add high light
  tracksHighlight <- lapply(grl,function(gr){
    rep(FALSE,length(gr))
  })
  ## if it's links, we don't need levels
  if("link" %in% tracksType){
    idx <- which("link" == tracksType[order(tracksOrder,decreasing=FALSE)])
    values(grl[[idx]])['.level'] <- 1
  }
  if(is.null(chrOrder)){
    chro <- sortChr(unique(as.character(seqnames(model))))
    idx <- match(chro,as.character(seqnames(model)))
    model <- model[idx]
  }else{
    ## FIXME: do I need to sort all the chromosomes?
    chro <- chrOrder
    idx <- match(chro,as.character(seqnames(model)))
    model <- model[idx]
  }
  if(!all(unlist((lapply(grl,function(x){
    inherits(x,'GenomicRanges')
  })))))
    stop('All the track object should be a list of GenomicRanges object')
  gp1 <- GraphicPars(...)
  gp2 <- GraphicPars()
  gp2@pars$isPlotted <- isPlotted[order(tracksOrder,decreasing=FALSE)]
  if(is.null(tracksWidth))
    gp2@pars$tracksWidth <- rep(40,N)
  else
    gp2@pars$tracksWidth <- tracksWidth[order(tracksOrder,decreasing=FALSE)]
  gp2@pars$length <- 100
  gp2@pars$skip <- 3
  gp2@pars$theme <- 'default'
  gp2@pars$spaceRate <- 0.01
  gp2@pars$scale.unit <-35*1e6
  ## 50M is the unit
  gp2@pars$unit <- (360-gp2@pars$spaceRate*360*length(model))/sum(as.numeric(width(model))) #angle per base
  gp2@pars$maxlevel <- as.numeric(unlist(lapply(grl,function(gr){
    max(values(gr)$.level)
  })))
  gp2@pars$tracksColorTheme <- tracksColorTheme[order(tracksOrder,decreasing=FALSE)]
  wd <- (1+log(gp2@pars$maxlevel)*0.2)*gp2@pars$tracksWidth
  widthunit <- wd/(gp2@pars$maxlevel)
  ## small skip use skip * factor(like 0.2)
  sp <- widthunit*0.2
  trackstart <- c(gp2@pars$length,gp2@pars$length+cumsum(wd*1.2+gp2@pars$skip))[seq_along(sp)]
  trackend <- gp2@pars$length+cumsum(wd*1.2+gp2@pars$skip)+gp2@pars$skip
  gp2@pars$canvas_radius <-max(trackend)
  gp2@pars$trackstart <- trackstart
  gp2@pars$widthunit <- widthunit
  gp2@pars$chrOrder <- chro
  gp2@pars$tracksType <- tracksType[order(tracksOrder,decreasing=FALSE)]
  gp2@pars$tracksOrder <- sort(tracksOrder,decreasing=FALSE)
  gp2@pars$tracksHighlight <- tracksHighlight[order(tracksOrder,decreasing=FALSE)]
  is.highlighted <- 
    mw <- width(model)*gp2@pars$unit
  sa <- c(0,cumsum(mw)[-(length(model))])+(1:length(model)-1)*gp2@pars$spaceRate*360
  values(model)$wipeLength=mw
  values(model)$startAngle=sa
  gp2@pars$model <- model
  gp <- pushCon(gp1,gp2)
  new('CircularView',tracks=grl,pars=gp@pars)
}
##' <description>
##'
##' <details>
##' @title 
##' @param obj 
##' @param ... 
##' @return 
##' @author tengfei
##' @export
setMethod('visplot','CircularView',function(obj,...){
  env <- new.env()
  grandEOSScene <- qscene()
  bgcol <- getAttr("bg.col")
  bgalpha <- getAttr("bg.alpha")
  qcol <- col2qcol(bgcol,bgalpha)
  grandEOSScene$setBackgroundBrush(qbrush(qcol))
  spaceRate <- obj@pars$spaceRate
  sp <- spaceRate*360
  skip <- obj@pars$skip
  len <- obj@pars$canvas_radius
  unit <- obj@pars$unit
  visenv$new.view <- NULL
  lroot <- qlayer(grandEOSScene,
                  limits=qrect(c(-len,len),c(-len,len)),
                  keyPressFun=function(layer,event){
                    key <- event$key()
                    if(key==Qt$Qt$Key_PageUp)
                      visenv$new.view$scale(2,2)
                    if(key==Qt$Qt$Key_PageDown)
                      visenv$new.view$scale(1/2,1/2)
                    if(key==Qt$Qt$Key_Space)
                      visenv$new.view$resetTransform()
                    if(key==Qt$Qt$Key_Down){
                      setHoverNext(visenv$zoomobj)
                    }
                  },
                  mouseMoveFun=function(layer,event){
                    pos <- as.numeric(event$pos())
                    if(!is.null(visenv$new.view)){
                      scene.pos <- layer$mapToScene(pos[1],pos[2])
                      spos <- as.numeric(scene.pos)
                      visenv$new.view$centerOn(spos[1],spos[2])
                    }
                  },
                  wheelFun= function(layer, event) {
                    zoom_factor <- 1.5
                    if(event$delta()<0)
                      zoom_factor <- 1/1.5
                    tform <- visenv$new.view$transform()
                    tform$scale(zoom_factor,zoom_factor)
                    visenv$new.view$setTransform(tform)
                  },
                  geometry=qrect(0,0,600,600))
  lapply(1:length(obj@tracks),function(n){
    gr <- obj@tracks[[n]]
    tp <- obj@pars$tracksType[n]
    l <- obj@pars$trackstart[n]
    w <- obj@pars$tracksWidth[n]
    tracksOrder <- obj@pars$tracksOrder
    if(is.na(tp)){
      tp <- 'sector'
    }
    paintFun <- switch(tp,
                       sector=eval(pfunSector),
                       segment=eval(pfunSegment),
                       text=eval(pfunText),
                       point=eval(pfunPoint),
                       line=eval(pfunLine),
                       link=eval(pfunLink),
                       scale=eval(pfunScale)
                       )
    ## main layer
    l <- qlayer(grandEOSScene,paintFun=paintFun,
                limits=qrect(c(-len,len),c(-len,len)),
                geometry=qrect(0,0,600,600),
                hoverMoveFun=visCirHoverEvent(obj,gr,tp,n,grandEOSScene),
                cache=TRUE)
    assign(tp,l,env)
  })
  assign("root",lroot,env)
  layer <- as.list(env)
  view <- qplotView(grandEOSScene)
  view$show()
  invisible(list(scene=grandEOSScene,view=view,layer=layer))
})
##----------------------------------------------------------------------------##
##   put all the painter function into closure below
##----------------------------------------------------------------------------##

## painter function for link
pfunLink <- quote({
  m2g <- map2global4link(obj,gr)
  mw <- m2g$width
  ms <- m2g$start
  mw.o <- m2g$other.width  
  ms.o <- m2g$other.start
  x <- ms+mw/2
  x2 <- ms.o+mw.o/2
  wsub <- obj@pars$widthunit[n]
  skipsub <- wsub*0.2
  xy1 <- polar2xy(radius=l+wsub,x)
  xy2 <- polar2xy(radius=l+wsub,x2)
  paintFun <- function(layer,painter){
    paths <- lapply(1:length(gr),function(i){
      qglyphQuadCurve(as.numeric(xy1[i,]),
                      c(0,0),
                      as.numeric(xy2[i,]))
    })
    cols <- getColor(obj@pars$tracksColorTheme[[n]],length(gr),tp)
    if(is(cols,"mutaframe"))
      cols <- as.character(cols[,,drop=TRUE])
    idx <- obj@pars$tracksHighlight[[n]]
    if(sum(idx)>0){
      qdrawPath(painter,paths[!idx],stroke=alpha("gray60",0.2))
      qdrawPath(painter,paths[idx],stroke="red")
    }else{
      qdrawPath(painter,paths,stroke=cols)
    }
  }
})

## painter function for line
pfunLine <- quote({
  m2g <- map2global(obj,gr)
  mw <- m2g$width
  ms <- m2g$start
  idx <- obj@pars$isPlotted[[n]]
  x <- ms+mw/2
  y <- values(gr)[,idx]
  ylim=c(min(y)*0.8,max(y)*1.2)
  wsub <- obj@pars$widthunit[n]
  skipsub <- wsub*0.2
  s <- sign(tracksOrder[n])
  y <- y/(diff(ylim))*(wsub*0.8)+wsub*0.1
  if(s<0) y <- wsub-y
  xy <- polar2xy(l+y,x)
  ## need to rescale y
  paintFun <- function(layer,painter){
    ## drawbg(painter,obj,l+w*(n-1)+skip*(n-1)+y,w)
    m2gm <- map2global(obj,obj@pars$model)
    mwm <- m2gm$width
    msm <- m2gm$start
    ## compute the position
    paths <- lapply(1:length(obj@pars$model),function(i){
      sa <- msm[i]
      sl <- mwm[i]
      paths <- qglyphSector(0,0,length=l,
                            width=wsub,
                            startAngle=sa,sweepLength=sl)
    })
    qdrawPath(painter,paths,fill='gray80',stroke=NA)
    seqlen <- pretty(c(l,l+wsub))
    paths <- lapply(seqlen,function(r){
      qglyphArc(0,0,r=r,0,360)          
    })
    qdrawPath(painter,paths,fill=NA,stroke='white')
    sp <- as.character(seqnames(gr))
    idx <- seq_len(length(sp))
    by(idx,sp,function(idx){
      st <- x[idx]
      idx <- idx[order(st)]
      if(is(cols,"mutaframe"))
        cols <- as.character(cols[,,drop=TRUE])
      cols <- getColor(obj@pars$tracksColorTheme[[n]],length(gr),tp)
      qdrawLine(painter,xy$x[idx],xy$y[idx],stroke='black')
    })
  }})


## painter function for point
pfunPoint <- quote({
  m2g <- map2global(obj,gr)
  mw <- m2g$width
  ms <- m2g$start
  idx <- obj@pars$isPlotted[[n]]
  x <- ms+mw/2
  y <- values(gr)[,idx]
  ylim=c(min(y)*0.8,max(y)*1.2)
  wsub <- obj@pars$widthunit[n]
  skipsub <- wsub*0.2
  s <- sign(tracksOrder[n])
  y <- y/(diff(ylim))*(wsub*0.8)+wsub*0.1
  if(s<0) y <- wsub-y
  xy <- polar2xy(l+y,x)
  ## need to rescale y
  paintFun <- function(layer,painter){
    ## drawbg(painter,obj,l+w*(n-1)+skip*(n-1)+y,w)
    m2gm <- map2global(obj,obj@pars$model)
    mwm <- m2gm$width
    msm <- m2gm$start
    ## compute the position
    paths <- lapply(1:length(obj@pars$model),function(i){
      sa <- msm[i]
      sl <- mwm[i]
      paths <- qglyphSector(0,0,length=l,
                            width=wsub,
                            startAngle=sa,sweepLength=sl)
    })
    qdrawPath(painter,paths,fill='gray80',stroke=NA)
    seqlen <- pretty(c(l,l+wsub))
    paths <- lapply(seqlen,function(r){
      qglyphArc(0,0,r=r,0,360)          
    })
    qdrawPath(painter,paths,fill=NA,stroke='white')
    cols <- getColor(obj@pars$tracksColorTheme[[n]],length(gr),tp)
    if(is(cols,"mutaframe"))
      cols <- as.character(cols[,,drop=TRUE])

    qdrawCircle(painter,xy$x,xy$y,r=1.5,fill=cols,stroke=NA)
  }
})


## painter function for text
pfunText <- quote({
  ## compute the position
  m2g <- map2global(obj,gr)
  mw <- m2g$width
  ms <- m2g$start
  ## compute the position
  mp <- ms+mw/2
  lv <- values(gr)$.level
  if(tracksOrder[n]<0){
    lv <- max(lv)+1-lv
  }
  wsub <- obj@pars$widthunit[n]
  skipsub <- wsub*0.2
  xy1 <- polar2xy(radius=l+wsub*(lv-1)+skipsub*(lv-1),mp)
  paintFun <- function(layer,painter){
    idx <- !(mp>90 & mp<270)
    cols <- getColor(obj@pars$tracksColorTheme[[n]],length(mp),tp)
    if(is(cols,"mutaframe"))
      cols <- as.character(cols[,,drop=TRUE])
    ##qantialias(painter) <- FALSE
    if(length(cols)>1){
      qdrawText(painter,as.character(seqnames(gr))[idx],xy1$x[idx],xy1$y[idx],halign='left',valign='center',rot=mp[idx],color=cols[idx])
      qdrawText(painter,as.character(seqnames(gr))[!idx],xy1$x[!idx],xy1$y[!idx],halign='right',valign='center',rot=mp[!idx]-180,color=cols[!idx])
    }else{
      qdrawText(painter,as.character(seqnames(gr))[idx],xy1$x[idx],xy1$y[idx],halign='left',valign='center',rot=mp[idx],color=cols)
      qdrawText(painter,as.character(seqnames(gr))[!idx],xy1$x[!idx],xy1$y[!idx],halign='right',valign='center',rot=mp[!idx]-180,color=cols)
    }
  }
})

## painter function for segment
pfunSegment <- quote({
  ## compute the position
  m2g <- map2global(obj,gr)
  mw <- m2g$width
  ms <- m2g$start
  mp <- ms+mw/2
  lv <- values(gr)$.level
  if(tracksOrder[n]<0){
    lv <- max(lv)+1-lv
  }
  wsub <- obj@pars$widthunit[n]
  skipsub <- wsub*0.2
  xy1 <- polar2xy(radius=l+wsub*(lv-1)+skipsub*(lv-1),mp)
  xy2 <- polar2xy(radius=l+wsub*(lv-1)+skipsub*(lv-1)+wsub,mp)
  paintFun <- function(layer,painter){
    cols <- getColor(obj@pars$tracksColorTheme[[n]],nrow(xy1),tp)
    if(is(cols,"mutaframe"))
      cols <- as.character(cols[,,drop=TRUE])
    qdrawSegment(painter,xy1$x,xy1$y,xy2$x,xy2$y,stroke=cols)
  }
})

## painter for sector
pfunSector <- quote({
  m2g <- map2global(obj,gr)
  mw <- m2g$width
  ms <- m2g$start
  mp <- ms+mw/2
  lv <- values(gr)$.level
  if(tracksOrder[n]<0){
    lv <- max(lv)+1-lv
  }
  wsub <- obj@pars$widthunit[n]
  xy <- polar2xy(radius=l+wsub/2,mp)
  chr <- as.character(seqnames(gr))
  chr <- gsub('chr','',chr)
  paths <- lapply(1:length(gr),function(i){
    sa <- ms[i]
    sl <- mw[i]
    skipsub <- wsub*0.2
    paths <- qglyphSector(0,0,
                          length=l+wsub*(lv[i]-1)+skipsub*(lv[i]-1),
                          width=wsub,
                          startAngle=sa,
                          sweepLength=sl)
  })
  paintFun <- function(layer,painter){
    cols <- getColor(obj@pars$tracksColorTheme[[n]],length(paths),tp)
    if(is(cols,"mutaframe"))
      cols <- as.character(cols[,,drop=TRUE])
    qdrawPath(painter,paths,fill=cols,stroke=NA)
    if(TRUE)
      qdrawText(painter,chr,xy$x,xy$y,"center","center",rot=mp-90,color="black")
  }
})

## painter for scale
pfunScale <- quote({
  m2g <- map2global(obj,gr)
  mw <- m2g$width
  ms <- m2g$start
  me <- ms+mw
  lv <- values(gr)$.level
  if(tracksOrder[n]<0){
    lv <- max(lv)+1-lv
  }
  wsub <- obj@pars$widthunit[n]
  skipsub <- wsub*0.2
  scale.unit <- obj@pars$scale.unit
  angle.unit <- scale.unit*obj@pars$unit
  scale.lst <- lapply(seq_along(ms),function(i){
    rg <- seq(ms[i],me[i],by=angle.unit)
  })
  scale.lab <- lapply(scale.lst,function(x) {
    paste(round((seq_len(length(x))-1)*angle.unit/obj@pars$unit/scale.unit,0)*scale.unit/1e6,"M",sep="")
  })
  xy1 <- lapply(scale.lst,function(x){polar2xy(l,x)})
  xy2 <- lapply(scale.lst,function(x){polar2xy(l+wsub*0.1,x)})
  ## small scale
  small.scale.lst <- lapply(seq_along(ms),function(i){
    rg <- seq(ms[i],me[i],by=angle.unit/5)
  })
  ## 50M is the unit
  xy1.s <- lapply(small.scale.lst,function(x){polar2xy(l,x)})
  xy2.s <- lapply(small.scale.lst,function(x){polar2xy(l+wsub*0.05,x)})
  paths <- lapply(1:length(gr),function(i){
    sa <- ms[i]
    sl <- mw[i]
    wsub <- obj@pars$widthunit[n]
    skipsub <- wsub*0.2
    paths <- qglyphArc(0,0,
                       r=l+wsub*(lv[i]-1)+skipsub*(lv[i]-1),
                       startAngle=sa,
                       sweepLength=sl)
  })
  paintFun <- function(layer,painter){
    cols <- getColor(obj@pars$tracksColorTheme[[n]],length(paths),tp)
    if(is(cols,"mutaframe"))
      cols <- as.character(cols[,,drop=TRUE])
    qdrawPath(painter,paths,stroke=cols)
    ## draw scale
    lapply(seq_len(length(scale.lst)),function(i){
      rots <- scale.lst[[i]]
      idx <- rots>90 & rots<270
      ## large scale
      qdrawSegment(painter,xy1[[i]]$x,xy1[[i]]$y,
                   xy2[[i]]$x,xy2[[i]]$y,stroke=cols)
      ## small scale
      qdrawSegment(painter,xy1.s[[i]]$x,xy1.s[[i]]$y,
                   xy2.s[[i]]$x,xy2.s[[i]]$y,stroke=cols)
      qdrawText(painter,scale.lab[[i]][idx],xy2[[i]]$x[idx],
                xy2[[i]]$y[idx],"right","center",rot=rots[idx]-180)
      qdrawText(painter,scale.lab[[i]][!idx],xy2[[i]]$x[!idx],
                xy2[[i]]$y[!idx],"left","center",rot=rots[!idx])   
    })
  }
})




##----------------------------------------------------------------------------##
##             events for circular view
##----------------------------------------------------------------------------##

## this event will capture the chromosome trunks and mapback
visCirHoverEvent <- function(obj,gr,type,n,scene){
  isIn <- FALSE
  preIsIn <- NULL
  prehits <- NULL
  obj@pars$tracksColorThemeBackup <-
    lapply(seq_along(obj@pars$tracksColorTheme),function(i){
      getColor(obj@pars$tracksColorTheme[[i]],length(gr),type)
    })
  obj@pars$tracksHighlightBackup <- obj@pars$tracksHighlight
  myfun <- switch(type,
                  sector=eval(visHoverSector),
                  link=eval(visHoverLink),
                  segment=eval(visHoverNULL))
}

visHoverSector <- quote({
  function(layer,event){
    rect <- qrect(0,0,5,5)
    mat <- layer$deviceTransform(event)$inverted()
    rect <- mat$mapRect(rect)
    pos <- event$pos()
    rect$moveCenter(pos)
    hits <- layer$locate(rect)+1
    if(length(hits)>=1){
      hits <- hits[1]
      if(is.null(prehits)) prehits <<- 0 #cause no 0 for hits
      cols <- obj@pars$tracksColorThemeBackup[[n]]
      idx <- which("link"==obj@pars$tracksType)
      if(length(idx)>0){
        grlink <- obj@tracks[[idx]]
        selected <- as.character(seqnames(grlink))==as.character(seqnames(gr))[hits]|
        as.character(values(grlink)$to.chr)==as.character(seqnames(gr))[hits]
        obj@pars$tracksHighlight[[idx]][selected] <- TRUE
      }
      obj@pars$tracksColorTheme[[n]][hits] <- "red"
      if(!isIn){
        isIn <<- TRUE
      }
      if(prehits!=hits){
        qupdate(scene)
        qupdate(scene)
        prehits <<-hits
      }
    }else{
      if(isIn){
        cols <- obj@pars$tracksColorThemeBackup[[n]]
        obj@pars$tracksColorTheme[[n]] <- cols
        obj@pars$tracksHighlight <- obj@pars$tracksHighlightBackup
        qupdate(scene)
        qupdate(scene)
      }
      isIn <<-FALSE
      prehits <<- NULL
    }
  }
})

visHoverLink <- quote({
  function(layer,event){
    rect <- qrect(0,0,3,3)
    mat <- layer$deviceTransform(event)$inverted()
    rect <- mat$mapRect(rect)
    pos <- event$pos()
    rect$moveCenter(pos)
    hits <- layer$locate(rect)+1
    print(hits)
  }})

visHoverNULL <- quote({
  function(layer,event){
  }
})

