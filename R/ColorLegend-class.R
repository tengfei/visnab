## Legend.gen <- setRefClass("Legend",
##                           fields = list(
##                             legendLayer = "Qanviz::RLayerORNULL"
##                             ),
##                           contains = "VIRTUAL")

## ColorLegendGraphicParameters.gen <- setGraphicPars("ColorLegend",
##                                           list(
##                                                xlim = "numeric",
##                                                ylim = "numeric"
##                                                )) 

## ColorLegend.gen <- setRefClass("ColorLegend",
##                           fields= c(signalingFields(
##                             list(breaks = "numeric",
##                                  labels = "character",
##                                  colors = "character",
##                                  title = "character"),
##                             signalName = "LegendChanged"),
##                             pars = "ColorLegendGraphicParameters",
##                             colorLegendLayer = "Qanviz::RLayerORNULL"
##                             ),
##                           contains = "Legend")

## ColorLegend <- function(title, breaks, labels, colors){
##   if(missing(title))
##     title <- character()
##   if(missing(breaks))
##     stop("Breaks is missing")
##   if(missing(labels))
##     stop("Labels is missing")
##   if(missing(colors))
##     stop("Colors is missing")
##   pars <- new("ColorLegendGraphicParameters")
##   obj <- ColorLegend.gen$new(title = title,
##                              breaks = breaks,
##                              labels = labels,
##                              colors = colors,
##                              pars = pars)
##   obj$pars$xlim <- c(0, 25)
##   obj
## }

## ColorLegend.gen$methods(
##   colorLegendPainter = function(){
##     function(layer, painter){
##     N <- length(colors)
##     pars$ylim <<- c(0, N*5+10)
##     qdrawText(painter, title, 5, 5, color = "black")
##     qdrawRect(painter, 8, 10+((1:N)-1)*5, 20, 10+((1:N))*5-2,
##               stroke = colors,
##               fill = colors)
##     qdrawText(painter, labels, 5, 10+((1:N)-1)*5, color = "black",
##               halign = "right", valign = "center")
##   }
##   },
##   layer = function(parent){
##     if(missing(parent))
##       parent <- scene
##     ## colorLegendLayer$setLimits(pars$xlim, pars$ylim)
##     qlayer(parent, .self$colorLegendPainter(),
##            limits = qrect(pars$xlim[1], pars$ylim[1],
##              pars$xlim[2], pars$ylim[2]))
##   },
##   show = function(scene, graphics = FALSE){
##   if(graphics){
##     N <- length(colors)
##     pars$ylim <<- c(0, N*5+10)
##     if(missing(scene))
##       scene <- qscene()
##     colorLegendLayer <<- layer(scene)
##     colorLegendLayer$setLimits(qrect(pars$xlim[1], pars$ylim[1],
##                                      pars$xlim[2], pars$ylim[2]))
##     pars$ylimChanged$connect(function(){
##       colorLegendLayer$setLimits(qrect(pars$xlim[1], pars$ylim[1],
##                                        pars$xlim[2], pars$ylim[2]))
##     })
##     view <- qplotView(scene)
##     view$show()
##     ## old <- par(pty = "s", mar = c(0, 0, 0, 0))
##     ## on.exit(par(old))
##     ## 
##     ## plot(c(0, 2), c(1, -n), type = "n", xlab="", ylab="", axes = FALSE)
##     ## if(!is.null(title))
##     ##   text(0.5, 0, title)
##     ## rect(1, -(1:n), 2, -(1:n)+0.5+0.25, col = colors)
##     ## text(1 - 0.5, -(1:n) + 0.5, labels)
    
##   }else{
##     list(breaks = breaks,
##                labels = labels,
##                colors = colors) 
##   }
## })


## setMethod("print", "Legend", function(x, graphics = FALSE){
##   x$show(graphics)
## })


## ## utils
## genColorLegend <- function(x, title, 
##                      cscale.pal = seq_gradient_pal("yellow", "red"),
##                      dscale.pal = brewer_pal(type = "qual",
##                                                     palette = "Set3"),
##                      breaks = pretty_breaks(),
##                      labels = scientific_format()){
##   if(missing(title))
##     title <- deparse(substitute(x))

##   if(is.numeric(x)){
##     cbs <- cbreaks(range(x), breaks = breaks, labels = labels)
##     N <- length(cbs$breaks)
##     cols <- cscale(c(cbs$breaks, x), cscale.pal)
##     cbs$colors <- cols[1:N]
##   }else{
##     ## discrete
##     cols <- dscale(as.character(x), dscale.pal)
##     lst <-by(cols, x, function(x){
##       col <- as.character(unique(x))
##       col
##     })
##     lst.vec <-  do.call("c", as.list(lst))
##     cbs <- list(breaks = NULL, labels = names(lst.vec),
##                 colors = as.character(lst.vec))
##   }
##   cs <- ColorLegend(title = title,
##                     breaks = cbs$breaks,
##                     labels = cbs$labels,
##                     colors = cbs$colors)

## }



