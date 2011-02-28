setOldClass('mutaframe')
setClass('DiagScatter',representation(mutaframe='mutaframe'),
         contain=c('GraphicPars')
         )

##----------------------------------------------------------------##
##               For Generating Scatter Plots
##----------------------------------------------------------------##
## constructor
DiagScatter <- function(pframe,xy=NULL){
  require(plumbr)
  if(is(pframe,'data.frame')){
    pframe <- as.mutaframe(pframe)
  }
  if(!is(pframe,'mutaframe')){
    stop('Input data need to be data.frame or mutaframe')
  }
  obj <- new('DiagScatter',mutaframe=pframe)
  obj@mutaframe$isSelected <- rep(FALSE,nrow(pframe))
  obj@mutaframe$isHoverSelected <- rep(FALSE,nrow(pframe))
  obj@mutaframe$isHighlighted <- rep(FALSE,nrow(pframe))
  obj@mutaframe$isZoomedIn <- rep(FALSE,nrow(pframe))
  obj@mutaframe$fill <- rep('black',nrow(pframe))
  obj@mutaframe$id<-seq_len(nrow(pframe))
  obj@pars$width <- 12
  obj@pars$scale <- 400
  obj@pars$mar <- c(50,50,40,20)
  obj@pars$skip.factor <- 15
  if(is.null(xy)){
    xy <- names(pframe)[1:2]
    setPar(obj,'xy',xy)
    
  }else{
    setPar(obj,'xy',xy)
  }
  px <- pframe[[xy[1]]]
  py <- pframe[[xy[2]]]
  pxrange <- range(px)
  pxdiff <- diff(pxrange)
  pyrange <- range(py)
  pydiff <- diff(pyrange)
  pxmin <- min(px)-0.1*pxdiff
  pxmax <- max(px)+0.1*pxdiff
  pymin <- min(py)-0.1*pydiff
  pymax <- max(py)+0.1*pydiff
  limits <- qrect(c(pxmin-pxdiff*0.3,pxmax+pxdiff*0.3),
                  c(pymin-pydiff*0.3,pymax+pydiff*0.3))
  setPar(obj,'limits',limits)               
  obj
}


setMethod('visplot','DiagScatter',function(obj,show=TRUE,GUI=FALSE,...){
  start.pos <- end.pos<- NULL
  isPressed <- FALSE
  ##----------------------------------------------------------------##
  ##                       Painters
  ##----------------------------------------------------------------##

  scatterFun <- function(layer,painter){
    selectLogic <- !obj@mutaframe$isSelected
    xy <- getPar(obj,'xy')
    mar <- getPar(obj,'mar')
    px <- obj@mutaframe[[xy[1]]]
    py <- obj@mutaframe[[xy[2]]]
    pxrange <- range(px)
    pxdiff <- diff(pxrange)
    pyrange <- range(py)
    pydiff <- diff(pyrange)
    ## draw black points first
    if(sum(selectLogic)>0){
      qdrawCircle(painter,px[selectLogic],py[selectLogic],5,fill=obj@mutaframe$fill[selectLogic],stroke=NA)}
    ## used for axis
    pxmin <- min(px)-0.1*pxdiff
    pxmax <- max(px)+0.1*pxdiff
    pymin <- min(py)-0.1*pydiff
    pymax <- max(py)+0.1*pydiff
    limits <- qrect(c(pxmin-pxdiff*0.3,pxmax+pxdiff*0.3),
                    c(pymin-pydiff*0.3,pymax+pydiff*0.3))
    l.scatter$setLimits(limits)
    l.red$setLimits(limits)
    ##  l.zoom$setLimits(limits)
    l.rect$setLimits(limits)
    ## draw x axis
    qdrawSegment(painter,pxmin,pymin,pxmax,pymin,stroke='black')
    ## draw y axis
    qdrawSegment(painter,pxmin,pymin,pxmin,pymax,stroke='black')
    ## add label
    xskip <- round(seq(from=pxmin,to=pxmax,length=5))
    yskip <- round(seq(from=pymin,to=pymax,length=5))
    qdrawSegment(painter,xskip,rep(pymin,5),
                 xskip,rep(pymin,5)-0.015*pydiff,
                 stroke='black')
    xskip.real <- signif(xskip,2)
    yskip.real <- signif(yskip,2)
    qdrawText(painter,xskip.real,xskip,rep(pymin,5)-0.08*pydiff)
    ## xlab
    qdrawText(painter,xy[1],pxmin+(pxmax-pxmin)/2,pymin-pydiff*0.15,'center','top')
    qdrawSegment(painter,rep(pxmin,5),yskip,
                 rep(pxmin,5)-0.015*pxdiff,yskip,
                 stroke='black'
                 )
    qdrawText(painter,yskip.real,rep(pxmin,5)-0.08*pxdiff,yskip)
    ## ylab
    qdrawText(painter,xy[2],pxmin-0.15*pxdiff,pymin+(pymax-pymin)/2,'right','bottom',rot=90)
    selectLogic <- obj@mutaframe$isSelected
    if(any(selectLogic)){
      qdrawCircle(painter,px[selectLogic],py[selectLogic],5,fill=obj@mutaframe$fill[selectLogic],stroke=NA)
    }
    ##   selectLogic <- obj@isHoverSelected
    ##   if(any(selectLogic)){
    ## ##    if(sum(selectLogic)>1){browser()}
    ##     qdrawCircle(painter,px[selectLogic],py[selectLogic],5,fill='red',stroke=NA)
    ##   }
  }

  scatterFunRed <- function(layer,painter){
    selectLogic <- !obj@mutaframe$isSelected
    xy <- getPar(obj,'xy')
    mar <- getPar(obj,'mar')
    px <- obj@mutaframe[[xy[1]]]
    py <- obj@mutaframe[[xy[2]]]
    pxrange <- range(px)
    pxdiff <- diff(pxrange)
    pyrange <- range(py)
    pydiff <- diff(pyrange)
    pxmin <- min(px)-0.1*pxdiff
    pxmax <- max(px)+0.1*pxdiff
    pymin <- min(py)-0.1*pydiff
    pymax <- max(py)+0.1*pydiff
    selectLogic <- obj@mutaframe$isHoverSelected
    if(any(selectLogic)){
      qdrawCircle(painter,px[selectLogic],py[selectLogic],5,fill='red',stroke=NA)
    }
  }

  scatterFunZoom <- function(layer,painter){
    selectLogic <- !obj@mutaframe$isZoomedIn
    xy <- getPar(obj,'xy')
    mar <- getPar(obj,'mar')
    px <- obj@mutaframe[[xy[1]]]
    py <- obj@mutaframe[[xy[2]]]
    if(any(selectLogic)){
      qdrawRect(painter,px[selectLogic],py[selectLogic],5,fill='yellow',stroke=NA)
    }
  }


  selectRect <- function(layer,painter){
    qdrawRect(painter,start.pos[1],start.pos[2],end.pos[1],end.pos[2],stroke='red')
  }

  ##----------------------------------------------------------------##
  ##                           Events
  ##----------------------------------------------------------------##

  pointIdentify <- function(layer,event){
    xy <- getPar(obj,'xy')
    mar <- getPar(obj,'mar')
    px <- obj@mutaframe[[xy[1]]]
    py <- obj@mutaframe[[xy[2]]]
    pos <- event$pos()
    rect <- qrect(0,0,10,10)
    mat <- layer$deviceTransform(event)$inverted()
    rect <- mat$mapRect(rect)
    rect$moveCenter(event$pos())
    hits <- layer$locate(rect)+1
    hits <- hits[1]
    if(!is.na(hits)&length(hits)>0){
      obj@mutaframe$fill[hits] <<- 'blue'
      obj@mutaframe$isSelected[hits] <<- TRUE
      ## obj@mutaframe$isZoomedIn<<-rep(FALSE,nrow(obj@mutaframe))
      ## obj@mutaframe$isZoomedIn[hits] <<- TRUE
      ## chr <- obj@mutaframe$chromosome[hits]
      ## zoomObj <<- genZoomBrowserDb('Mus musculus',chr,
      ##                              region_start=obj@mutaframe$start[hits],
      ##                              region_end=obj@mutaframe$end[hits])
      hotRegionStarts<<-hotRegionEnds<<-NULL
      ## qupdate(l.region)
      ## viewZoomTrack$resetTransform()
      ## qupdate(viewZoomTrack)
      ## qupdate(viewZoomChrom)
      ## qupdate(layerIsland)
      qupdate(l.red)
      ## qupdate(l.hot)
    }else{
      qupdate(l.red)
      message('No point identified')
    }
    ##  qupdate(s.scatter)
    ##  qupdate(l.hot)
    ##  qupdate(l.zoom)
  }
  getID <- function(pos,x,y){
    mx1 <- matrix(c(x,y),ncol=2)
    mx2 <- matrix(pos,ncol=2)
    pts <- matchpt(mx1,mx2)
    idx <- which.min(pts[,2])
    pos.old <- as.numeric(l.scatter$mapToScene(x[idx],y[idx]))
    pos.new <- as.numeric(l.scatter$mapToScene(pos[1],pos[2]))
    dst <- matchpt(matrix(pos.old,ncol=2),matrix(pos.new,ncol=2))[,2]
    if(dst<5)
      {
        return(idx)
      } else{
        return(NULL)
      }
  }

  selectFun <- function(layer,event){
    start.pos <<-as.numeric(event$buttonDownPos(Qt$Qt$LeftButton))
    end.pos <<- as.numeric(event$pos())
    isPressed<<-TRUE
    qupdate(l.rect)
  }
  
  releaseFun <- function(layer,event){
    if(isPressed){
      if(!(is.null(end.pos)|is.null(start.pos))){
        xy <- getPar(obj,'xy')
        ## obj@mutaframe <- obj@mutaframe
        px <- obj@mutaframe[[xy[1]]]
        py <- obj@mutaframe[[xy[2]]]
        topLeft.x <- min(end.pos[1],start.pos[1])
        bottomRight.x <- max(end.pos[1],start.pos[1])
        topLeft.y <- min(start.pos[2],end.pos[2])
        bottomRight.y <- max(start.pos[2],end.pos[2])
        id <- which(px<bottomRight.x&px>topLeft.x&
                    py<bottomRight.y&py>topLeft.y)
        obj@mutaframe$fill[id] <<- 'blue'
        obj@mutaframe$isSelected[id] <<- TRUE
        start.pos<<-end.pos<<-NULL
        qupdate(l.rect)
        ## qupdate(s.scatter)
        ## qupdate(l.hot)
      }
    }
  }
  hoverFun <- function(layer,event){
    xy <- getPar(obj,'xy')
    px <- obj@mutaframe[[xy[1]]]
    py <- obj@mutaframe[[xy[2]]]
    pos <- event$pos()
    pos <- as.numeric(pos)
    rect <- qrect(0,0,10,10)
    mat <- layer$deviceTransform(event)$inverted()
    rect <- mat$mapRect(rect)
    rect$moveCenter(event$pos())
    hits <- layer$locate(rect)+1
    if(length(hits)>0){
      idx <- hits[1]
      if(idx<=nrow(obj@mutaframe)){
        obj@mutaframe$isHoverSelected<<-rep(FALSE,nrow(obj@mutaframe))
        obj@mutaframe$isHighlighted<<-rep(FALSE,nrow(obj@mutaframe))
        obj@mutaframe$isHighlighted[idx]<<-TRUE
        obj@mutaframe$isHoverSelected[idx]<<-TRUE
        qupdate(l.red)
        ## qupdate(layerIsland)
      }
    }else{
      obj@mutaframe$isHoverSelected<<-rep(FALSE,nrow(obj@mutaframe))
      qupdate(l.scatter)
    }
  }

  s.scatter<-qscene()
  l.scatter<-qlayer(s.scatter,
                     paintFun=scatterFun,
                     mouseMoveFun=selectFun,
                     mouseDoubleClickFun=pointIdentify,
                     mouseReleaseFun=releaseFun,
                     hoverMoveFun=hoverFun,
                     cache=FALSE)
  l.red<-qlayer(s.scatter,paintFun=scatterFunRed,cache=FALSE)
  limits <- getPar(obj,'limits')
  l.rect <- qlayer(s.scatter,paintFun=selectRect)
  l.scatter$setLimits(limits)
  l.red$setLimits(limits)
  l.rect$setLimits(limits)
  v.scatter <- qplotView(s.scatter)
  if(show)
    v.scatter$show()
  if(GUI){
    ## visGUI(obj,s.scatter)
    GUIfun <- visGUI(obj,s.scatter)
    GUIfun()
  }
})

