##----------------------------------------------------------------------------##
##                     Classes
##----------------------------------------------------------------------------##
CircularView.gen <- setRefClass("CircularView",contains="QtVisnabView",
                                fields=list(tracks="list",
                                  tracksType="characterORNULL",
                                  tracksOrder="numericORNULL",
                                  tracksWidth="numericORNULL",
                                  chrOrder="characterORNULL",
                                  model="GenomicRanges",
                                  .sectorText="logical"
                                  ))

##----------------------------------------------------------------------------##
##               Constructor for CircularView
##----------------------------------------------------------------------------##
## grl should be a list of GenomicRanges or MutableRanges

CircularView <- function(grl,
                         model,
                         chrOrder=NULL,
                         tracksType=NULL,
                         tracksOrder=NULL,
                         ## isPlotted=NULL,
                         tracksWidth=NULL,
                         scene=NULL,
                         view=NULL,
                         rootLayer=NULL,
                         row=0L,
                         col=0L,
                         .sectorText=TRUE,
                         rescale = "none"){
  
  ## check if list element is MutableRanges
  pars <- GraphicPars(bgColor="black",fill="gray80",fgColor="gray80")
  if(any(unlist(lapply(grl,function(gr) extends(class(gr),"GenomicRanges"))))){
    grl <- lapply(grl,function(gr) {
      gr <- as(gr,"MutableGRanges")
      ## color scheme
      gr <- addAttr(gr,.color=pars$fgColor,.brushed=FALSE)
    })
  }
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
  ## add levels to all the list
  grl <- lapply(1:length(grl),function(i){
    if(tracksType[i] == "bar"){
      grl[[i]] <- addLevels(grl[[i]])
    }else{
      values(grl[[i]])$.level <- 1
    }
    grl[[i]]
  })
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
  ## gp2@pars$isPlotted <- isPlotted[order(tracksOrder,decreasing=FALSE)]
  if(is.null(tracksWidth))
    tracksWidth <- rep(40,N)
  else
    tracksWidth <- tracksWidth[order(tracksOrder,decreasing=FALSE)]
  tracksType <- tracksType[order(tracksOrder,decreasing=FALSE)]
  tracksOrder <- sort(tracksOrder,decreasing=FALSE)

  obj <- CircularView.gen$new(tracks=grl,pars=pars,tracksType=tracksType,model=model,
                              tracksOrder=tracksOrder, tracksWidth=tracksWidth,
                              chrOrder=chro, scene=scene, row=row, col=col,
                              view=view, rootLayer=rootLayer,
                              .sectorText=.sectorText)
  obj$createView(rescale = rescale)
  obj
}


CircularView.gen$methods(createView = function(seqname=NULL, rescale = "geometry"){
  ## graphic device
  if(is.null(scene)){
    scene <<- qscene()
    view <<- qplotView(scene,rescale = rescale)
    view$setDragMode(Qt$QGraphicsView$ScrollHandDrag)
    rootLayer <<- qlayer(scene,
                  ## limits=qrect(c(-len,len),c(-len,len)),
                  keyPressFun=function(layer,event){
                    if(event$modifiers() == Qt$Qt$ControlModifier){
                      if(event$key() == Qt$Qt$Key_Equal)
                        view$scale(1.5,1.5)
                      if(event$key() == Qt$Qt$Key_Minus)
                        view$scale(1/1.5,1/1.5)
                      if(event$key() == Qt$Qt$Key_0)
                        view$resetTransform()
                      ## if(event$key() == Qt$Qt$Key_u)
                      ##    viewInUCSC(obj)
                    }},
                  ## mouseMoveFun=function(layer,event){
                  ##   pos <- as.numeric(event$pos())
                  ##   if(!is.null(visenv$new.view)){
                  ##     scene.pos <- layer$mapToScene(pos[1],pos[2])
                  ##     spos <- as.numeric(scene.pos)
                  ##     visenv$new.view$centerOn(spos[1],spos[2])
                  ##   }
                  ## },
                  wheelFun= function(layer, event) {
                    zoom_factor <- 1.5
                    if(event$delta()<0)
                      zoom_factor <- 1/1.5
                    view$scale(zoom_factor,zoom_factor)
                  },
                  geometry=qrect(0,0,700,700),cache=FALSE)

  }
  ## event
  ## background
  bgcol <- pars$bgColor
  bgalpha <- pars$alpha
  qcol <- col2qcol(bgcol,bgalpha)
  scene$setBackgroundBrush(qbrush(qcol))
  ## settings
  length <- 100
  skip <- 3
  spaceRate <- 0.01
  scale.unit <-35*1e6
  ## 50M is the unit
  unit <- (360-spaceRate*360*length(model))/sum(as.numeric(width(model)))
  maxlevel <- unlist(lapply(tracks,function(gr){
    as.numeric(max(values(gr)$.level))
  }))
  ## wd <- (1+log(maxlevel)*0.2)*tracksWidth
  wd <- tracksWidth
  widthunit <- wd/(maxlevel)
  ## small skip use skip * factor(like 0.2)
  sp <- widthunit*0.2
  ## trackstart <- c(length,length+cumsum(wd*1.2+skip))[seq_along(sp)]
  ## trackend <- length+cumsum(wd*1.2+skip)+skip
  trackstart <- c(length,length+cumsum(wd*1.2+skip))[seq_along(sp)]
  trackend <- length+cumsum(wd*1.2+skip)+skip

  canvas_radius <-max(trackend)
  ## chrOrder <- chro
  mw <- width(model)*unit
  sa <- c(0,cumsum(mw)[-(length(model))])+(1:length(model)-1)*spaceRate*360
  values(model)$wipeLength <<- mw
  values(model)$startAngle <<- sa
  ## not sure it's a good way to get global attributes
  sp <- spaceRate*360
  len <- canvas_radius
  map2global <- function(gr){
    idx <- match(as.character(seqnames(gr)),as.character(chrOrder))
    st <- values(model)$startAngle[idx]+start(gr)*unit
    wd <- (end(gr)-start(gr))*unit
    return(data.frame(start=st,width=wd))
  }
  map2global4link <- function(gr){
    idx <- match(as.character(seqnames(gr)),as.character(chrOrder))
    chrwo <- as.character(seqnames(gr))[is.na(idx)]
    if(any(is.na(idx))){
      cat("You are losing informations because chromosome\n",chrwo,
              "\nis not plotted which exists in input data")
    }
    st <- values(model)$startAngle[idx]+start(gr)*unit
    wd <- (end(gr)-start(gr))*unit
    gr2 <- elementMetadata(gr)
    gr2 <- GRanges(seqnames=gr2$to.chr,
                   ranges=IRanges(start=gr2$to.start,
                     end=gr2$to.end))
    idx2 <- match(as.character(seqnames(gr2)),as.character(chrOrder))
    chrwo <- as.character(seqnames(gr2))[is.na(idx2)]
    if(any(is.na(idx2))){
      cat("You are losing informations because chromosome\n",chrwo,
              "\nis not plotted which exists in input data")
    }
    st2 <- values(model)$startAngle[idx2]+start(gr2)*unit
    wd2 <- (end(gr2)-start(gr2))*unit
    df <- data.frame(start=st,width=wd,to.start=st2,to.width=wd2)
    ## shouldn't drop NA here?
    ## df <- na.omit(df)
    return(df)
  }

############################################################
  ## All painter funciton
############################################################
  ## ====================
  ## Link
  ## ====================
  pfunLink <-function(gr,idx,paths){
    function(layer,painter){
      cols <- values(gr)$.color[idx]
      qdrawPath(painter,paths,stroke=cols)
    }}
  
  ## ===================================
  ## Sector
  ## ===================================
  ## painter for sector
  pfunSector  <- function(gr,paths,xy,chr,mp){
    function(layer,painter){
      cols <- values(gr)$.color
      qdrawPath(painter,paths,fill=cols,stroke=NA)
      if(.sectorText)
        qdrawText(painter,chr,xy$x,xy$y,"center","center",
                  rot=mp-90,color="white",cex=1.3)

    }}

  ## ===================================
  ## Points
  ## ===================================
  pfunPoint  <- function(gr,seqlen.cor,xy,paths,msm,mwm,lst.grid){
    function(layer,painter){
      qdrawPath(painter,paths,fill=pars$gridBgColor,stroke=NA)
      lapply(lst.grid,function(path) {qdrawPath(painter,path,fill=NA,stroke='white')})
      circle <- qglyphCircle(r=1.2)
      cols <- values(gr)$.color
      qdrawGlyph(painter,circle, xy$x,xy$y,fill=cols,stroke=NA)
    }}

  ## ===================================
  ## Scale
  ## ===================================
  ## painter for scale

  pfunScale <- function(gr,scale.lst,xy1,xy2,xy1.s,xy2.s,paths,scale.lab){
    function(layer,painter){
      cols <- values(gr)$.color
      qdrawPath(painter,paths,stroke=cols)
      ## draw scale
      lapply(seq_len(length(scale.lst)),function(i){
        rots <- scale.lst[[i]]
        idx <- rots>90 & rots<270
        ## large scale
        qdrawSegment(painter,xy1[[i]]$x,xy1[[i]]$y,
                     xy2[[i]]$x,xy2[[i]]$y,stroke=unique(cols)[1])
        ## small scale
        qdrawSegment(painter,xy1.s[[i]]$x,xy1.s[[i]]$y,
                     xy2.s[[i]]$x,xy2.s[[i]]$y,stroke=unique(cols)[1])
        if(sum(idx)>0)
        qdrawText(painter,scale.lab[[i]][idx],xy2[[i]]$x[idx],
                  xy2[[i]]$y[idx],"right","center",rot=rots[idx]-180,
                  color=unique(cols)[1])
        if(sum(!idx)>0)
        qdrawText(painter,scale.lab[[i]][!idx],xy2[[i]]$x[!idx],
                  xy2[[i]]$y[!idx],"left","center",
                  rot=rots[!idx],color=unique(cols)[1])   
      })
    }}

  ## ===================================
  ## Segment
  ## ===================================
  pfunSegment <- function(gr,xy1,xy2){
    function(layer,painter){
      ## compute the position
        cols <- values(gr)$.color
        qdrawSegment(painter,xy1$x,xy1$y,xy2$x,xy2$y,stroke=cols)
    }}

  ## ===================================
  ## (Stacked Bar) not really a bar chart yet(neet to be fixed)
  ## ===================================
  pfunBar <- function(gr,xy1,xy2){
    function(layer,painter){
      cols <- values(gr)$.color
      qdrawSegment(painter,xy1$x,xy1$y,xy2$x,xy2$y,stroke=cols)
    }}

  ## ===================================
  ## Lines(revised from pfunPoint)
  ## ===================================
  pfunLine  <- function(gr,seqlen.cor,xy,paths,
                        msm,mwm,lst.grid,chrgp){
    function(layer,painter){
      ## draw grid
      qdrawPath(painter,paths,fill=pars$gridBgColor,stroke=NA)
      lapply(lst.grid,function(path) {qdrawPath(painter,path,fill=NA,stroke='white')})
      ## draw line
      cols <- unique(values(gr)$.color)[1]
      by(xy,chrgp,function(chrgr){
        chrgr <- chrgr[order(chrgr$pos,decreasing=FALSE),]
        N <- nrow(chrgr)
        qdrawSegment(painter,chrgr$x[-N],chrgr$y[-N],
                     chrgr$x[-1],chrgr$y[-1],stroke=cols)        
      })
    }}

  ## rootLayer<<-qlayer(scene,geometry=qrect(0,0,700,700))
  lapply(1:length(tracks),function(n){
    gr <- tracks[[n]]
    tp <- tracksType[n]
    l <- trackstart[n]
    w <- tracksWidth[n]
    if(is.na(tp)){
      tp <- 'sector'
    }
    ## leave computation before paint function
    if(tp == "sector"){
      values(gr)$.color <- "white"
    }
    if(tp == "link"){
      values(gr)$.color <- alpha(pars$fgColor,0.5)
      m2g <- map2global4link(gr)
      idx <- apply(m2g,1,function(row){all(!is.na(row))})
      ## gr <- gr[idx]
      m2g <- na.omit(m2g)
      mw <- m2g$width
      ms <- m2g$start
      mw.o <- m2g$to.width  
      ms.o <- m2g$to.start
      x <- ms+mw/2
      x2 <- ms.o+mw.o/2
      wsub <- widthunit[n]
      skipsub <- wsub*0.2
      xy1 <- polar2xy(radius=l+wsub,x)
      xy2 <- polar2xy(radius=l+wsub,x2)
      paths <- lapply(seq_len(nrow(xy1)),function(i){
        qglyphQuadCurve(c(xy1[i,1],xy1[i,2]),
                        c(0,0),
                        c(xy2[i,1],xy2[i,2]))
      })

    }
    if(tp == "sector"){
            m2g <- map2global(gr)
      mw <- m2g$width
      ms <- m2g$start
      mp <- ms+mw/2
      ## lv <- values(gr)$.level
      ## if(tracksOrder[n]<0){
      ##   lv <- max(lv)+1-lv
      ## }
      wsub <- widthunit[n]
      skipsub <- wsub*0.2
      ## xy <- polar2xy(radius=l+wsub*(lv-1)+skipsub*(lv-1)+wsub/2,mp)
      xy <- polar2xy(radius=l+wsub/2,mp)
      chr <- as.character(seqnames(gr))
      chr <- gsub('chr','',chr)
      paths <- lapply(1:length(gr),function(i){
        sa <- ms[i]
        sl <- mw[i]
        paths <- qglyphSector(0,0,
                              ## length=l+wsub*(lv[i]-1)+skipsub*(lv[i]-1),
                              length=l,
                              width=wsub,
                              startAngle=sa,
                              sweepLength=sl)
      })
    }
    if(tp == "point" | tp == "line"){
      m2g <- map2global(gr)
      chrgp <- as.character(seqnames(gr))
      mw <- m2g$width
      ms <- m2g$start
      ## Should be more flexible
      if(TRUE)
        idx <- as.integer(which(unlist(lapply(values(gr)@listData,is,"numeric")))[1])
      x <- circle.pos.x <- ms+mw/2
      y <- values(gr)[,idx]
      ## ylim should be a plotting limits on y-axis
      ylim <- range(y)
      ## ylim <- c(min(y)-0.05*abs(min(y)),max(y)+0.05*abs(max(y)))
      mrg <- 0.05*diff(ylim)
      ylim <- c(ylim[1]-mrg,ylim[2]+mrg)
      ## change to layer coordinates
      y <- (y-min(ylim))/(diff(ylim))*w
      ## if(s<0) y <- wsub-y
      xy <- polar2xy(l+y,x)
      xy$pos <- circle.pos.x
      m2gm <- map2global(model)
      mwm <- m2gm$width
      msm <- m2gm$start
      seqlen <- pretty(ylim,n=5,h=3)
      seqlen.cor <- sapply(seqlen,function(r){
        (r-min(ylim))/diff(ylim)*w
      })
      seqlen.cor <- seqlen.cor+l
      seqlen.cor <-  subset(seqlen.cor,(seqlen.cor <= l+w)&
                           (seqlen.cor >= l))
      lst.grid <- lapply(1:length(model),function(i){
        sa <- msm[i]
        sl <- mwm[i]
        paths <- lapply(seqlen.cor,function(r){
          qglyphArc(0,0,r=r,sa,sl)          
        })
      })
      ## compute the position
      paths <- lapply(1:length(model),function(i){
        sa <- msm[i]
        sl <- mwm[i]
        paths <- qglyphSector(0,0,length=l,
                              width=w,
                              startAngle=sa,sweepLength=sl)
      })

      values(gr)$.color <- pars$stroke
    }
    
    if(tp == "segment"){
      m2g <- map2global(gr)
      mw <- m2g$width
      ms <- m2g$start
      mp <- ms+mw/2
      xy1 <- polar2xy(radius=l,mp)
      xy2 <- polar2xy(radius=l+w,mp)
      values(gr)$.color <- alpha(pars$fgColor,0.5)
    }
    if(tp == "scale"){
            m2g <- map2global(gr)
      mw <- m2g$width
      ms <- m2g$start
      me <- ms+mw
      ## lv <- values(gr)$.level
      ## if(tracksOrder[n]<0){
      ##   lv <- max(lv)+1-lv
      ## }
      wsub <- widthunit[n]
      skipsub <- wsub*0.2
      scale.unit <- scale.unit
      angle.unit <- scale.unit*unit
      scale.lst <- lapply(seq_along(ms),function(i){
        rg <- seq(ms[i],me[i],by=angle.unit)
      })
      scale.lab <- lapply(scale.lst,function(x) {
        paste(round((seq_len(length(x))-1)*angle.unit/unit/scale.unit,0)*
              scale.unit/1e6,"M",sep="")
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
        wsub <- widthunit[n]
        skipsub <- wsub*0.2
        paths <- qglyphArc(0,0,r=l,
                           startAngle=sa,
                           sweepLength=sl)
      })
    }
    if(tp == "bar"){
      m2g <- map2global(gr)
      mw <- m2g$width
      ms <- m2g$start
      mp <- ms+mw/2
      lv <- values(gr)$.level
      wsub <- widthunit[n]
      skipsub <- wsub*0.2
      xy1 <- polar2xy(radius=l+wsub*(lv-1)+skipsub*(lv-1),mp)
      xy2 <- polar2xy(radius=l+wsub*(lv-1)+skipsub*(lv-1)+wsub,mp)
    }
    paintFun <- switch(tp,
                       sector=pfunSector(gr,paths,xy,chr,mp),
                       segment=pfunSegment(gr,xy1,xy2),
                       bar=pfunBar(gr,xy1,xy2),
                       point=pfunPoint(gr,seqlen.cor,xy,paths,msm,mwm,lst.grid),
                       line=pfunLine(gr,seqlen.cor,xy,paths,msm,mwm,lst.grid,chrgp),
                       link=pfunLink(gr,idx,paths),
                       scale=pfunScale(gr,scale.lst,xy1,xy2,xy1.s,xy2.s,paths,scale.lab)
                       )
    layer <- qlayer(rootLayer,paintFun=paintFun,
                    limits=qrect(c(-len,len),c(-len,len)),cache=FALSE,
                    row=row,col=col)
    tracks[[n]]$elementMetadataChanged$connect(function(){
      qupdate(layer)
    })
  })
})


## painter function for segment



##----------------------------------------------------------------------------##
##             events for circular view
##----------------------------------------------------------------------------##

## this event will capture the chromosome trunks and mapback
visCirHoverEvent <- function(obj,gr,type,n,scene){
  isIn <- FALSE
  preIsIn <- NULL
  prehits <- NULL
  tracksColorThemeBackup <-
    lapply(seq_along(tracksColorTheme),function(i){
      getColor(tracksColorTheme[[i]],length(gr),type)
    })
  tracksHighlightBackup <- tracksHighlight
  myfun <- switch(type,
                  sector=eval(visHoverSector),
                  link=eval(visHoverLink),
                  segment=eval(visHoverNULL))
}

visHoverSector <- quote({
  function(layer,event){
    rect <- qrect(0,0,1,1)
    mat <- layer$deviceTransform(event)$inverted()
    rect <- mat$mapRect(rect)
    pos <- event$pos()
    rect$moveCenter(pos)
    hits <- layer$locate(rect)+1
    if(length(hits)>=1){
      hits <- hits[1]
      if(is.null(prehits)) prehits <<- 0 #cause no 0 for hits
      cols <- tracksColorThemeBackup[[n]]
      idx <- which("link"==tracksType)
      if(length(idx)>0){
        grlink <- obj@tracks[[idx]]
        selected <- as.character(seqnames(grlink))==as.character(seqnames(gr))[hits]|
        as.character(values(grlink)$to.chr)==as.character(seqnames(gr))[hits]
        tracksHighlight[[idx]][selected] <- TRUE
      }
      tracksColorTheme[[n]][hits] <- "red"
      if(!isIn){
        isIn <<- TRUE
      }
      if(prehits!=hits){
        qupdate(scene)
        prehits <<-hits
      }
    }else{
      if(isIn){
        cols <- tracksColorThemeBackup[[n]]
        tracksColorTheme[[n]] <- cols
        tracksHighlight <- tracksHighlightBackup
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

## utils
getColor <- function(trackColorTheme,n,types){
  if(is(trackColorTheme,'mutaframe'))
    trackColorTheme <- as.character(trackColorTheme[,,drop=TRUE])
  if(length(trackColorTheme)>1){
    cols <- trackColorTheme
    return(cols)
  }
  if(length(trackColorTheme)==1){
    if(trackColorTheme=='default'){
      cols <- switch(types,
                     segment='blue',
                     line='purple',
                     point='red',
                     text='black',
                     sector='gray90')
    }
    cols <- rep(cols,n)
  }
  return(cols)
}


polar2xy <- function(radius,angle){
  x <- radius*cos(angle/360*(2*pi))
  y <- radius*sin(angle/360*(2*pi))
  data.frame(x=x,y=y)
}



## need to plot a diagram
CircularView.gen$methods(show = function(){
  view$show()
})

setMethod("print","CircularView",function(x,..){
  x$show()
})
