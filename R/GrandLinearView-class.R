##----------------------------------------------------------------------------##
##             For class "GrandIntervalView"
##----------------------------------------------------------------------------##

setClass("GrandLinearView",contains=c("GraphicPars"),
         representation(track="list"))

##----------------------------------------------------------------------------##
##             "GrandLinearView" constructor
##----------------------------------------------------------------------------##
GrandLinearView <- function(track,...){
  pars <- GraphicPars(...)@pars
  new("GrandLinearView",pars=pars,track=track)
}

setMethod("visplot","GrandLinearView",function(obj,
                                               seqname=NULL,
                                               cutbin=NULL,
                                               start=NULL,
                                               end=NULL,
                                               width=NA,
                                               stretchFactor=NULL){
  bgcol <- getAttr("bg.col")
  bgalpha <- getAttr("bg.alpha")
  qcol <- col2qcol(bgcol,bgalpha)
  scene <- qscene()
  lroot <- qlayer(scene,geometry=qrect(0,0,800,600))
  scene$setBackgroundBrush(qbrush(qcol))
  view <- qplotView(scene,rescale="none")
  lapply(1:length(obj@track),function(i){
    objo <- obj@track[[i]]
    cls <- class(objo)
    if(extends(cls,"IntervalView"))
      linearViewLayer(objo,chr=seqname,env=new.env(),
                      lroot=lroot,view=view,
                      row=i-1,start=start,
                      end=end)
    if(extends(cls,"SeqView"))
      genomeRefViewLayer(objo,lroot,view,seqname,start,end,width,row=i-1)
    if(extends(cls,"AlignmentView"))
      shortReadViewLayer(objo,lroot=lroot,view=view,seqname=NULL,
                         cutbin=cutbin,start=start,end=end,row=i-1)
  })
  if(!is.null(stretchFactor)){
    lapply(1:length(stretchFactor),function(i){
      lroot$gridLayout()$setRowStretchFactor(i-1,stretchFactor[i])      
    })
}
  view$show()
})

setGeneric("visplotDock",function(obj,...) standardGeneric("visplotDock"))
setMethod("visplotDock","GrandLinearView",function(obj,
                                               seqname=NULL,
                                               cutbin=NULL,
                                               start=NULL,
                                               end=NULL,
                                               width=NA,
                                               stretchFactor=NULL,
                                                   viewNames=NULL,
                                                   size=c(1400,600),
                                                   verticalTitleBar=TRUE){

  win <- Qt$QMainWindow()
  win$resize(size[1],size[2])
  bgcol <- getAttr("bg.col")
  bgalpha <- getAttr("bg.alpha")
  qcol <- col2qcol(bgcol,bgalpha)
  scene <- qscene()
  lroot <- qlayer(scene,geometry=qrect(0,0,800,600))
  scene$setBackgroundBrush(qbrush(qcol))
  xlim <- as.matrix(scene$sceneRect)[2,]
  N <- length(obj@track)
  newh <- xlim[2]/N
  lapply(1:length(obj@track),function(i){
    objo <- obj@track[[i]]
    cls <- class(objo)
     trackname <- viewNames[i]
     track <- qplotView(scene,rescale="transform")
     track$setSceneRect(0,(i-1)*newh,xlim[1],newh*i)
    dw <- Qt$QDockWidget(trackname)
    if(verticalTitleBar){
      dw$setFeatures(Qt$QDockWidget$AllDockWidgetFeatures|
                   Qt$QDockWidget$DockWidgetVerticalTitleBar)
    }else{
      dw$setFeatures(Qt$QDockWidget$AllDockWidgetFeatures)
    }
    dw$setAllowedAreas(Qt$Qt$LeftDockWidgetArea|
                       Qt$Qt$RightDockWidgetArea)
    dw$setWidget(track)
    win$addDockWidget(Qt$Qt$RightDockWidgetArea,dw,Qt$Qt$Vertical)
    if(extends(cls,"IntervalView"))
      linearViewLayer(objo,chr=seqname,env=new.env(),
                      lroot=lroot,view=track,
                      row=i-1,start=start,
                      end=end)
    if(extends(cls,"SeqView"))
      genomeRefViewLayer(objo,lroot,view=track,seqname,start,end,width,row=i-1)
    if(extends(cls,"AlignmentView"))
      shortReadViewLayer(objo,lroot=lroot,view=track,seqname=seqname,
                         cutbin=cutbin,start=start,end=end,row=i-1)
  })
  ##  lapply(1:length(obj@track),function(i){
  
  ## })
##   if(!is.null(stretchFactor)){
##     lapply(1:length(stretchFactor),function(i){
##       lroot$gridLayout()$setRowStretchFactor(i-1,stretchFactor[i])      
##     })
## }
  win$show()
})


visplotDock <- function(...,size=c(1400,600),verticalTitleBar=TRUE){
  lst <- list(...)
  win <- Qt$QMainWindow()
  win$resize(size[1],size[2])
  lapply(names(lst),function(trackname){
    track <- lst[[trackname]]
    dw <- Qt$QDockWidget(trackname)
    if(verticalTitleBar){
      dw$setFeatures(Qt$QDockWidget$AllDockWidgetFeatures|
                   Qt$QDockWidget$DockWidgetVerticalTitleBar)
    }else{
      dw$setFeatures(Qt$QDockWidget$AllDockWidgetFeatures)
    }
    dw$setAllowedAreas(Qt$Qt$LeftDockWidgetArea|
                       Qt$Qt$RightDockWidgetArea)
    dw$setWidget(track)
    win$addDockWidget(Qt$Qt$RightDockWidgetArea,dw,Qt$Qt$Vertical)
  })
  win
}
