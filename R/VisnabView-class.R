##------------------------------------------------------------##
## Top defined strucutrue to store fixed slots
##------------------------------------------------------------##

VisnabView.gen <- setRefClass("VisnabView",
                              contains = c("VIRTUAL"),
                              fields = c(
                                signalingField("legend", "LegendList"),
                                signalingField("viewrange", "SimpleMutableGRanges"),
                                pars = "GraphicParameters",
                                eventTrace = "EventTrace",
                                tooltipinfo = "character",
                                viewname = "character"
                                ))


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
  cat("VisnabView object\n")
  cat("-------------------------\n")
  cat("Run show(object$pars) to read more details about graphic parameters
       associated with this object\n")
  ## show(object$pars)
})

##' \code{range} return a \code{GRanges} object that describe the
##' visualized region and chromosome.
##'
##' range method for class \code{VisnabView}
##' @title Range of viewed region
##' @param x \code{VisnabView} object.
##' @param ... 
##' @return \code{GRanges} object which indicate the visualized region.
##' @author tengfei
setMethod("range", "VisnabView", function(x,...){
  return(x$viewrange)
})

setReplaceMethod("range", "VisnabView", function(x, value){
  if(is(value, "IRanges")){
    if(length(value)>1)
      stop("Viewed range can only be of length 1")
    x$pars$xlimZoom <- c(start(value), end(value))
    ## ranges(x$viewrange) <- value
  }
  if(is(value, "numeric")){
    if(length(value)!=2)
      stop("Please specify start and end value")
    if(diff(value)<=0)
      stop("Viewed range cannot be less than 0")
    x$pars$xlimZoom <- value
    ## ranges(x$viewrange) <- IRanges(value[1], value[2])
  }
  if(is(value, "character")){
    if(substr(value,1,3) != "chr")
      stop("Please follow the routine when naming the seqnames,
            with prefix 'chr',such as chr1, chrX ...")
    ## seqnames(x$seqinfo) <- value
    seqnames(x$viewrange) <- value
  }
  if(extends(class(value),"GenomicRanges")){
    if(length(value)>1)
      stop("Viewed range can only be of length 1")
    seqname <- as.character(seqnames(value))
    if(substr(as.character(seqname),1,3) != "chr")
      stop("Please follow the routine when naming the seqnames,
            with prefix 'chr',such as chr1, chrX ...")
    ## x$pars$xlimZoomChanged$block()
    ## x$pars$seqname <- seqname
    seqnames(x$viewrange) <- seqname
    x$pars$xlimZoom <- c(start(value), end(value))
    ## x$pars$xlimZoomChanged$unblock()
  }
  x
})



## setReplaceMethod("selectedRangesModel", "VisnabView", function(x,value){
## if(is(value, "GRanges"))
##   value <- as(value, "MutableGRanges")
## if(is(value, "MutableGRanges"))
##   x$selectedRangesModel <- value
## x
## })

## setMethod("selectedRangesModel", "GenomicRanges", function(obj, color = "red"){
##   if(is(obj, "GRanges"))
##     data <- as(obj, "MutableGRanges")
##   x <- structure(list(data = data, color = color), class = "selectedRangesModel")
##   invisible(x)
## })

## selectedRangesModel <- function(data, color = "red"){
##   if(is(data, "GRanges"))
##     data <- as(data, "MutableGRanges")
##   structure(list(data = data, color = color), class = "selectedRangesModel")
## }

## setMethod("+", "VisnabView", function(e1, e2){
##   if(is(e2, "selectedRangesModel")){
##     e1$selectedRangesModel <- e2$data
##     e1$selectedRangesModelColor <- e2$color
##     invisible(e1)
##   }else{
##     invisible(e1)
##   }
## })

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


setMethod("geom","VisnabView",function(x,...){
  cat("Choosed geom: ",x$pars$geom,"\n")
  cat("---------------------\n")
  cat("Supported geoms: \n")
  geoms <- levels(x$pars$geom)
  if(!is.null(geoms))
    cat(geoms,"\n")
  else
    message("No supported geom is found for this object")
})

setReplaceMethod("geom","VisnabView", function(x,value){
  x$pars$geom <- value
  x
})
