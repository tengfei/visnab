##------------------------------------------------------------##
## Top defined strucutrue to store fixed slots
##------------------------------------------------------------##
VisnabView.gen <- setRefClass("VisnabView",contains="VIRTUAL",
                              fields=list(
                                pars="GraphicPars"
                                ))

setReplaceMethod("seqnames","VisnabView",
                 function(x,value){
                   x$pars$seqname <- value
                   x
                 })


setMethod("geom","VisnabView",function(x,...){
  print(x$pars$geom)
})

setReplaceMethod("geom","VisnabView",
                function(x,value){
                  x$pars$geom <- value
                  x
                })

## return current graphics pars
## FIXME: This should return more defined fields
setMethod("Aes", "VisnabView", function(x){
  cat("Graphic Parameters:\n")
  cat("--------------------\n")
  for(nm in ls(x$pars@.xData)){
    y <- get(nm,env=x$pars@.xData)
    if((is(y,"character"))||(is(y,"numeric"))){
      cat(nm, " = ", toString(y), "\n")
    }
  }
})

setMethod("show","VisnabView",function(object){
  show(object$pars)
})


setMethod("viewInUCSC","VisnabView",function(obj,genome){
  if(!(exists("session")&&extends(class(session),"BrowserSession")))
    session <- browserSession()
  genome(session) <- genome
  chr <- as.character(seqnames(obj@viewrange))
  ir <- ranges(obj@viewrange)
  targets <- GRangesForUCSCGenome(genome, chr, ir)
  browserView(session,targets)
})


