##------------------------------------------------------------##
## Top defined strucutrue to store fixed slots
##------------------------------------------------------------##
setGeneric("print")

setClass('VisnabView',representation("VIRTUAL",
                        show="logical",
                        viewmr="MutableGRanges",
                        idname="characterORNULL",
                        pars="GraphicPars"
                        ))


## Accessor
setGeneric("seqnames<-",
           function(x,...,value) standardGeneric("seqnames<-"))
setReplaceMethod("seqnames","VisnabView",
                 function(x,value){
                   seqnames(x@viewrange) <- value
                   x
                 })

setMethod("show","VisnabView",function(object){
  show(object@pars)
})


setMethod("addAttr","VisnabView",function(obj,...){
  addAttr(obj@track,...)
  lst <- list(...)
  obj@pars$attrs <- lst
})

setGeneric("addDefAttr",function(obj,...) standardGeneric("addDefAttr"))

setMethod("addDefAttr", "VisnabView",function(obj,...){
  addAttr(obj,.color=obj@pars$fill,.hover=FALSE,.brushed=FALSE)  
})

