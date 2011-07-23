######################################################################
##  Create a SimpleList class for VisanbView
##  Class "VisnabViewList"
######################################################################
## constructor

VisnabViewList.gen <- setRefClass("VisnabViewList",
                                  fields = list(listData = "mutalist",
                                    elementType = "character"))

VisnabViewList.gen$methods(initialize = function(elementType = "VisnabView", ...){
  elementType <<- elementType
  callSuper(...)
})

VisnabViewList <- function(...){
  vvlst <- mutalist(...)
  if (length(vvlst) == 1 && is(vvlst[[1L]],"mutalist"))
    vvlst <- vvlst[[1L]]
  if (length(vvlst) == 1 && is.list(vvlst[[1L]])){
    vvlst <- mutalist(vvlst[[1L]])
    vvlst <- vvlst[[1L]]
  }
  if (!all(sapply(vvlst, is, "VisnabView")))
    stop("all elements in '...' must be VisnabView objects")
  ans <- VisnabViewList.gen$new(listData = vvlst)
  ans
}

VisnabViewList.gen$methods(append = function(obj){
  if(!is(obj, "VisnabView"))
    stop("The object to be appended must be of Class VisnabView")
  listData <<- c(listData, mutalist(obj))
})



######################################################################
##     accessors
######################################################################
## elementType (works)
## as.list
## elementLengths (doesn't work)
setMethod("show", "VisnabViewList", function(object){
  callNextMethod()
  cat("Field: listData\n")
  show(object$listData)
})

setMethod("length", "VisnabViewList", function(x){
  length(x$listData)
})

setReplaceMethod("names","VisnabViewList", function(x, value){
  names(x$listData) <- value
  x
})

setMethod("names", "VisnabViewList", function(x){
  names(x$listData)
})

##----------------------------------------
## subsetting doesn't work
##----------------------------------------
## cannot make a copy
## setMethod("[[", "VisnabViewList", function(x, i, j, ...){
##   lst <- x$listData[[i]]
##   obj <- VisnabViewList(lst)
##   obj
## })

## setReplaceMethod("[[", "VisnabViewList", function(x, i, j, ..., value){
##   x$listData[[i]] <- value
##   x
## })

## coerce
setAs("VisnabViewList", "list", function(from, to){
  to <- unlist(from$listData)
  to
})

setAs("VisnabViewList", "mutalist", function(from, to){
  to <-from$listData
  to
})

setMethod("unlist", "VisnabViewList", function(x, recursive = TRUE, use.names = TRUE){
  unlist(x$listData)
})

setMethod("lapply", "VisnabViewList", function(X, FUN, ...){
  lapply(unlsit(X), FUN = FUN, ...)
})

## crate new one
setMethod("c", "VisnabViewList", function(x, ..., recursive = FALSE){
  nlst <- do.call("c", lapply(list(x,...), function(x) {x$listData}))
  obj <- VisnabViewList(nlst)
  obj
})

