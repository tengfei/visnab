setGeneric("print")

setClass("TracksView",contains="GraphicPars",
         representation(track="list"))
TracksView <- function(...){
    pars <- GraphicPars()@pars
    new('TracksView',track=list(...),pars=pars)
}

setMethod("print","TracksView",function(x,...){
  obj <- x
  ## grand scene
  scene <- qscene()
  ## grand layer
    wheelZoom <- function(layer, event) {
    zoom_factor <- 1.5
    if(event$delta()<0)
      zoom_factor <- 1/1.5
    tform <- view$transform()
    tform$scale(zoom_factor,1)
    view$setTransform(tform)
  }

  rootLayer <- qlayer(scene,wheelFun=wheelZoom)
    ## grand view
  view <- qplotView(scene,rescale="none")
  sapply(1:length(obj@track),function(i){
    print(obj@track[[i]],scene=scene,view=view,
            rootLayer=rootLayer,show=FALSE,
            row=i)
  })
  view$show()
})

