##------------------------------------------------------------##
## Top defined strucutrue to store fixed slots
##------------------------------------------------------------##
setGeneric("print")

setClass('VisnabView',contains="GraphicPars",
         representation("VIRTUAL",
                        show="logical",
                        seqnames="characterORNULL"))


## Accessor
setGeneric("seqnames<-",
           function(x,...,value) standardGeneric("seqnames<-"))
setReplaceMethod("seqnames","VisnabView",
                 function(x,value){
                   ## FIXME: validation here
                   x@seqnames <- value
                   x
                 })


