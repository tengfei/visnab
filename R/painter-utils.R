##----------------------------------------------------------------------
##  These utils should be moved to qtpaint later
##----------------------------------------------------------------------
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param x Size of the arrow
##' @param direction Direction of the arrow
##' @examples scene <- qscene()
##' layer <- qlayer(scene, function(layer, painter){
##' arr <- qglyphArrow()
##' qdrawGlyph(painter, arr, 1:10, 10)
##' }, limits = qrect(c(0, 15),c(0,20)))
##' qplotView(scene)$show()
##' @return A \code{QPainterPath} object
##' @author tengfei
qglyphArrow <- function(x = 5, direction = c("left", "right")){
  dirs <- match.arg(direction)
  if(direction == "right")
    x = -x
  glyph <- Qt$QPainterPath()
  glyph$moveTo(x,x)
  glyph$lineTo(0,0)
  glyph$lineTo(x,-x)
  glyph
}

qpathSector <- function(x,y,length,width,startAngle,sweepLength){
  len <- (length+width)*2
  r <- length+width
  x0 <- x-r
  y0 <- y-r
  glyph <- Qt$QPainterPath()
  movex <- x+r*cos(startAngle/180*pi)
  movey <- y+r*sin(startAngle/180*pi)
  glyph$moveTo(movex,movey)
  glyph$arcTo(x0,y0,len,len,-startAngle,-sweepLength)
  r <- r-width
  x0 <- x-r
  y0 <- y-r
  glyph$arcTo(x0,y0,2*r,2*r,-(startAngle+sweepLength),sweepLength)
  glyph$closeSubpath()
  glyph
}

qpathArc <- function(x,y,r,startAngle,sweepLength){
  x0 <- x-r
  y0 <- y-r
  len <- 2*r
  glyph <- Qt$QPainterPath()
  movex <- x+r*cos(startAngle/180*pi)
  movey <- y+r*sin(startAngle/180*pi)
  glyph$moveTo(movex,movey)
  glyph$arcTo(x0,y0,len,len,-startAngle,-sweepLength)
  glyph
}

qpathQuadCurve <- function(startpoint,controlpoint,endpoint){
  glyph <- Qt$QPainterPath()
  glyph$moveTo(startpoint[1],startpoint[2])
  glyph$quadTo(controlpoint[1],controlpoint[2],endpoint[1],endpoint[2])
  glyph
}

qpathCurveBundle <- function(startpoint1,controlpoint1,endpoint1,
                              startpoint2,controlpoint2,endpoint2){
  glyph <- Qt$QPainterPath()
  glyph$moveTo(startpoint1[1],startpoint1[2])
  glyph$qCuadTo(controlpoint1[1],controlpoint1[2],endpoint1[1],endpoint1[2])
  glyph$lineTo(startpoint2[1],startpoint2[2])
  glyph$quadTo(controlpoint2[1],controlpoint2[2],endpoint2[1],endpoint2[2])
  glyph$lineTo(startpoint1[1],startpoint1[2])
  glyph$closeSubpath()
  glyph
}

##----------------------------------------------------------------------
##  own defined palletes
##----------------------------------------------------------------------
bluered_pal <- function(){
  function(x){
    x <- cscale(x,rescale_pal())
    x <- x-0.5
    idx <- x>=0
    col.red <- cscale(x[idx],rescale_pal())
    col.red <- rgb(col.red,0,0)
    col.blue <- cscale(abs(x[!idx]),rescale_pal())
    col.blue <- rgb(0,0,col.blue)
    col <- vector("numeric",length(x))
    col[idx] <- col.red
    col[!idx] <- col.blue
    col
  }
}

blackred_pal <- function(){
  function(x){
    x <- cscale(x,rescale_pal())
    x <- 1-x
    col <- rgb(x,0,0)
  }
}
## used for heatmap

div_prox_pal <- function(low = "blue", mid = "white", high = "red",
                    to = c(0, 1), proxylen = 100){
  function(x){
    if(length(x)>proxylen)
      proxylen <- 100
    else
      proxylen <- length(x)
    vals <- x
    to = c(0, 1)
    vals <- rescale_mid(vals, to = to, mid = 0)
    ## cols <- cscale(vals, div_gradient_pal("blue", "white", "red"))
    bks <- seq(0, 1, length = proxylen)
    ints <- as.character(cut(vals, bks))
    lvs <- as.list(by(vals, ints, mean))
    vls <- unname(unlist(lvs))
    cols <- cscale(vls, div_gradient_pal(low = low, mid = mid, high = high))
    colss <- cols[match(ints, names(lvs))]
  }
}

