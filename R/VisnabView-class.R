##------------------------------------------------------------##
## Top defined strucutrue to store fixed slots
##------------------------------------------------------------##
## FIXME: perhaps scene,layer,view needed here.
setGeneric("print")


setClass('VisnabView',contains="GraphicPars",
         representation("VIRTUAL",
                        show="logical",
                        seqnames="characterOrNULL"))

## Accessor
setGeneric("seqnames<-",
           function(x,...,value) standardGeneric("seqnames<-"))
setReplaceMethod("seqnames","VisnabView",
                 function(x,value){
                   ## FIXME: validation here
                   x@seqnames <- value
                   x
                 })


## setMethod("show","VisnabView",function(object){
##   callNextMethod()
## })
