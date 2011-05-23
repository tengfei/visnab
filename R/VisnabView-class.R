##------------------------------------------------------------##
## Top defined strucutrue to store fixed slots
##------------------------------------------------------------##
VisnabView.gen <- setRefClass("VisnabView",contains="VIRTUAL",
                              fields=c(
                                pars="GraphicPars",
                                signalingField("selectedRangesModel", "MutableGRanges"),
                                signalingField("selectedRangesModelColor", "character"),
                                signalingField("genome", "character"),
                                signalingField("outputRange", "numericORNULL")
                                ))


## return current graphics pars
## FIXME: This should return more defined fields
setMethod("aes", "VisnabView", function(x){
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

setMethod("range", "VisnabView", function(x,...){
  seqname <- x$pars$seqname
  ir <- IRanges(start = x$pars$xlimZoom[1],
                end = x$pars$xlimZoom[2])
  gr <- GRanges(seqnames=seqname, ir)
  return(gr)
})

setReplaceMethod("range", "VisnabView", function(x, value){
  if(is(value, "IRanges")){
    if(length(value)>1)
      stop("Viewed range can only be of length 1")
    x$pars$xlimZoom <- c(start(value), end(value))
  }
  if(is(value, "numeric")){
    if(length(value)!=2)
      stop("Please specify start and end value")
    if(diff(value)<=0)
      stop("Viewed range cannot be less than 0")
    x$pars$xlimZoom <- value
  }
  if(is(value, "character")){
    if(substr(value,1,3) != "chr")
      stop("Please follow the routine when naming the seqnames,
            with prefix 'chr',such as chr1, chrX ...")
    x$pars$seqname <- value
  }
  if(extends(class(value),"GenomicRanges")){
    if(length(value)>1)
      stop("Viewed range can only be of length 1")
    seqname <- as.character(seqnames(value))
    if(substr(as.character(seqname),1,3) != "chr")
      stop("Please follow the routine when naming the seqnames,
            with prefix 'chr',such as chr1, chrX ...")
    x$pars$seqname <- seqname
    x$pars$xlimZoom <- c(start(value), end(value))
  }
  x
})



## setReplaceMethod("range", "VisnabView", function(x, value){
##   if(is(value, "IRanges")){
##     if(length(value)>1)
##       stop("Viewed range can only be of length 1")
##     x$selectedRange <- c(start(value), end(value))
##   }
##   if(is(value, "numeric")){
##     if(length(value)!=2)
##       stop("Please specify start and end value")
##     if(diff(value)<=0)
##       stop("Viewed range cannot be less than 0")
##     x$selectedRange <- value
##   }
##   if(is(value, "character")){
##     if(substr(value,1,3) != "chr")
##       stop("Please follow the routine when naming the seqnames,
##             with prefix 'chr',such as chr1, chrX ...")
##     x$pars$seqname <- value
##   }
##   if(extends(class(value),"GenomicRanges")){
##     if(length(value)>1)
##       stop("Viewed range can only be of length 1")
##     seqname <- as.character(seqnames(value))
##     if(substr(as.character(seqname),1,3) != "chr")
##       stop("Please follow the routine when naming the seqnames,
##             with prefix 'chr',such as chr1, chrX ...")
##     x$pars$seqname <- seqname
##     x$selectedRange <- c(start(value), end(value))
##   }
##   x
## })


setMethod("selectedRangesModel", "VisnabView", function(obj, ...){
  print(obj$selectedRangesModel)
})

setReplaceMethod("selectedRangesModel", "VisnabView", function(x,value){
if(is(value, "GRanges"))
  value <- as(value, "MutableGRanges")
if(is(value, "MutableGRanges"))
  x$selectedRangesModel <- value
x
})

setMethod("selectedRangesModel", "GenomicRanges", function(obj, color = "red"){
  if(is(obj, "GRanges"))
    data <- as(obj, "MutableGRanges")
  x <- structure(list(data = data, color = color), class = "selectedRangesModel")
  invisible(x)
})

## selectedRangesModel <- function(data, color = "red"){
##   if(is(data, "GRanges"))
##     data <- as(data, "MutableGRanges")
##   structure(list(data = data, color = color), class = "selectedRangesModel")
## }

setMethod("+", "VisnabView", function(e1, e2){
  if(is(e2, "selectedRangesModel")){
    e1$selectedRangesModel <- e2$data
    e1$selectedRangesModelColor <- e2$color
    invisible(e1)
  }else{
    invisible(e1)
  }
})



setMethod("viewInBrowser","VisnabView",function(obj, genome, browser = "UCSC"){
  if(browser == "UCSC"){
    if(!(exists("session")&&extends(class(session),"BrowserSession")))
      session <- browserSession()
    genome(session) <- genome
    vr <- range(obj)
    chr <- seqnames(vr)
    ir <- ranges(vr)
    targets <- GRangesForUCSCGenome(genome, as.character(chr), ir)
    browserView(session,targets)
  }
})



