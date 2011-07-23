ColorLegend.gen <- setRefClass("ColorLegend",
                               fields=list(breaks = "numeric",
                                 labels = "character",
                                 colors = "character",
                                 title = "characterORNULL"))


ColorLegend <- function(title, breaks, labels, colors){
  if(missing(title))
    title <- NULL
  if(missing(breaks))
    stop("Breaks is missing")
  if(missing(labels))
    stop("Labels is missing")
  if(missing(colors))
    stop("Colors is missing")
  obj <- ColorLegend.gen$new(title = title,
                             breaks = breaks,
                             labels = labels,
                             colors = colors)
  obj
}

ColorLegend.gen$methods(show = function(graphics = FALSE){
  if(graphics){
    n <- length(colors)
    old <- par(pty = "s", mar = c(0, 0, 0, 0))
    on.exit(par(old))
    plot(c(0, 2), c(1, -n), type = "n", xlab="", ylab="", axes = FALSE)
    if(!is.null(title))
      text(0.5, 0, title)
    rect(1, -(1:n), 2, -(1:n)+0.5+0.25, col = colors)
    text(1 - 0.5, -(1:n) + 0.5, labels)
  }else{
    print(list(breaks = breaks,
               labels = labels,
               colors = colors)) 
  }
})


setMethod("print", "ColorLegend", function(x, graphics = FALSE){
  x$show(graphics)
})

## utils
genLegend <- function(gr, color, title, 
                     cscale.pal = div_gradient_pal(),
                     dscale.pal = hue_pal(),
                     breaks = pretty_breaks(),
                     labels = scientific_format()){
  if(missing(title))
    title <- deparse(substitute(color))
  if(missing(color))
    color <- I("red")
  cols <- color
  if(inherits(color, "AsIs")){
    cols <- as.character(color)
    cols <- rep(cols,length(gr))
    cbs <- list(breaks = NA,
                labels = unique(cols),
                colors = unique(cols))
  }else{
  ## check if it's in elementMetadata
  if((as.character(cols) %in% names(elementMetadata(gr)))){
    cols.value <- elementMetadata(gr)[[cols]]
    ## continuous
    if(is.numeric(cols.value)){
      cbs <- cbreaks(range(cols.value), breaks = breaks, labels = labels)
      N <- length(cbs$breaks)
      cols <- cscale(c(cbs$breaks, cols.value), cscale.pal)
      cols <- cols[-N]
      cbs$colors <- cols[1:N]
    }else{
      ## discrete
      cols <- dscale(factor(cols.value), dscale.pal)
      lst <-by(cols, cols.value, function(x){
        col <- as.character(unique(x))
        col
      })
      lst.vec <-  do.call("c", as.list(lst))
      cbs <- list(breaks = NULL, labels = names(lst.vec),
                  colors = as.character(lst.vec))
    }}else{
      stop("Please make sure you are passing a variable that
            exists in elementMetadata, otherwise if you just
            pass a single color, make sure to use I()")
    }}
  cs <- ColorLegend(title = title,
                    breaks = cbs$breaks,
                    labels = cbs$labels,
                    colors = cbs$colors)
  list(color = cols, colorLegend = cs)
}
