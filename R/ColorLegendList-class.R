ColorLegendList.gen <- setRefClass("ColorLegendList",
                                  fields = list(listData = "mutalist",
                                    elementType = "character"))

ColorLegendList.gen$methods(initialize = function(elementType = "ColorLegend", ...){
  elementType <<- elementType
  callSuper(...)
})

ColorLegendList <- function(...){
  vvlst <- mutalist(...)
  if (length(vvlst) == 1 && is(vvlst[[1L]],"mutalist"))
    vvlst <- vvlst[[1L]]
  if (length(vvlst) == 1 && is.list(vvlst[[1L]])){
    vvlst <- mutalist(vvlst[[1L]])
    vvlst <- vvlst[[1L]]
  }
  if (!all(sapply(vvlst, is, "ColorLegend")))
    stop("all elements in '...' must be ColorLegend objects")
  ans <- ColorLegendList.gen$new(listData = vvlst)
  ans
}
## FIXME: wrong color showed, need to be fixed
ColorLegendList.gen$methods(show = function(graphics = FALSE){
  if(!graphics){
    lapply(listData, print)
  }else{
    nlist <- lapply(listData, function(x){
      length(x$colors)
    })
    nlist <- c(0, nlist)
    n <- sum(unlist(nlist))
    old <- par(pty = "s", mar = c(0, 0, 0, 0))
    on.exit(par(old))
    plot(c(-1, 2), c(1, -n), type = "n", xlab="", ylab="", axes = FALSE)
    lapply(1:length(listData), function(i){
      st <- 0+sum(unlist(nlist)[1:i])
      text(0.5, -st+1, listData[[i]]$title)
      rect(1, -(st:(st+nlist[[i+1]])), 2, -(st:(st+nlist[[i+1]]))+0.5,
           col = listData[[i]]$colors)
      text(1 - 0.5, -(st:(st+nlist[[i+1]])) + 0.5, listData[[i]]$labels)
    })
  }
})
