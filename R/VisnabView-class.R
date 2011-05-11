##------------------------------------------------------------##
## Top defined strucutrue to store fixed slots
##------------------------------------------------------------##
setGeneric("print")

VisnabView.gen <- setRefClass("VisnabView",contains="VIRTUAL",
                              fields=list(
                                title="characterORNULL",
                                pars="GraphicPars"
                                ))
## Accessor
## setGeneric("seqnames<-",
##            function(x,...,value) standardGeneric("seqnames<-"))


setReplaceMethod("seqnames","VisnabView",
                 function(x,value){
                   x$pars$seqname <- value
                   x
                 })

setGeneric("geom",function(x,...) standardGeneric("geom"))
setMethod("geom","VisnabView",function(x,...){
  print(x$pars$geom)
})

setGeneric("geom<-",function(x,value) standardGeneric("geom<-"))
setReplaceMethod("geom","VisnabView",
                function(x,value){
                  x$pars$geom <- value
                  x
                })


setMethod("show","VisnabView",function(object){
  show(object$pars)
})


setGeneric("viewInUCSC",function(obj,...) standardGeneric("viewInUCSC"))

setMethod("viewInUCSC","VisnabView",function(obj,...){
  if(!(exists("session")&&extends(class(session),"BrowserSession")))
    session <- browserSession()
  genome(session) <- "hg19"
  chr <- as.character(seqnames(obj@viewrange))
  ## ir <- IRanges(start=start(obj@viewrange),end=end(obj@viewrange))
  ir <- ranges(obj@viewrange)
  targets <- GRangesForUCSCGenome("hg19",chr,ir)
  browserView(session,targets)
})


