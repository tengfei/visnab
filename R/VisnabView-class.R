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

aes <- Aes

setMethod("show","VisnabView",function(object){
  show(object$pars)
})

setMethod("viewrange", "VisnabView", function(obj,...){
  seqname <- obj$pars$seqname
  ir <- IRanges(start = obj$pars$xlimZoom[1],
                end = obj$pars$xlimZoom[2])
  gr <- GRanges(seqnames=seqname, ir)
  return(gr)
})

setMethod("viewInBrowser","VisnabView",function(obj, genome, browser = "UCSC"){
  if(browser == "UCSC"){
    if(!(exists("session")&&extends(class(session),"BrowserSession")))
      session <- browserSession()
    genome(session) <- genome
    vr <- viewrange(obj)
    chr <- seqnames(vr)
    ir <- ranges(vr)
    targets <- GRangesForUCSCGenome(genome, chr, ir)
    browserView(session,targets)
  }
})



