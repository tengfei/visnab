##' <description>
##'
##' <details>
##' @title 
##' @param x 
##' @param y 
##' @param length 
##' @param width 
##' @param startAngle 
##' @param sweepLength 
##' @return 
##' @author tengfei
##' @export
qglyphSector <- function(x,y,length,width,startAngle,sweepLength){
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
  ## movex <- x+r*cos((startAngle+sweepLength)/180*pi)
  ## movey <- y+r*sin((startAngle+sweepLength)/180*pi)
  ##glyph$lineTo(movex,movey)
  glyph$arcTo(x0,y0,2*r,2*r,-(startAngle+sweepLength),sweepLength)
  glyph$closeSubpath()
  glyph
}
##' <description>
##'
##' <details>
##' @title 
##' @param x 
##' @param y 
##' @param r 
##' @param startAngle 
##' @param sweepLength 
##' @return 
##' @author tengfei
##' @export
qglyphArc <- function(x,y,r,startAngle,sweepLength){
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

##' <description>
##'
##' <details>
##' @title 
##' @param startpoint 
##' @param controlpoint 
##' @param endpoint 
##' @return 
##' @author tengfei
##' @export
qglyphQuadCurve <- function(startpoint,controlpoint,endpoint){
  glyph <- Qt$QPainterPath()
  glyph$moveTo(startpoint[1],startpoint[2])
  glyph$quadTo(controlpoint[1],controlpoint[2],endpoint[1],endpoint[2])
  glyph
}
##' <description>
##'
##' <details>
##' @title 
##' @param startpoint1 
##' @param controlpoint1 
##' @param endpoint1 
##' @param startpoint2 
##' @param controlpoint2 
##' @param endpoint2 
##' @return 
##' @author tengfei
##' @export
qglyphCurveBundle <- function(startpoint1,controlpoint1,endpoint1,
                              startpoint2,controlpoint2,endpoint2){
  glyph <- Qt$QPainterPath()
  glyph$moveTo(startpoint1[1],startpoint1[2])
  glyph$quadTo(controlpoint1[1],controlpoint1[2],endpoint1[1],endpoint1[2])
  glyph$lineTo(startpoint2[1],startpoint2[2])
  glyph$quadTo(controlpoint2[1],controlpoint2[2],endpoint2[1],endpoint2[2])
  glyph$lineTo(startpoint1[1],startpoint1[2])
  glyph$closeSubpath()
  glyph
}

## library(qtpaint)
## s <- qscene()
## paths <- list(qglyphQuadCurve(c(-120,-120),c(0,100),c(140,140)),
##              c(-120,-120),c(100,0),c(140,140))
##  myfun <- function(layer,painter){
##     qantialias(painter) <- FALSE
##     qlineWidth(painter) <- 2
##     qdrawPath(painter,paths,stroke=rainbow(length(paths)))  
##  }
##  l <- qlayer(s,myfun,limits=qrect(-140,-140,140,140))
##  v <- qplotView(s)
##  v$show()

## s <- qscene()
## paths <- list(qglyphCurveBundle(c(-120,-120),c(-85,60),c(-120,140),
##               c(-50,140),c(-85,60),c(-50,-120)))
## paths <- list(qglyphCurveBundle(c(-50,-120),c(-85,60),c(-120,140),
##               c(-50,140),c(-85,60),c(-120,-120)))
##  myfun <- function(layer,painter){
## ##    qantialias(painter) <- FALSE
##     qfillColor(painter) <- 'red'
## ##    qlineWidth(painter) <- 2
##     qdrawPath(painter,paths,stroke=rainbow(length(paths)))
##  }
##  l <- qlayer(s,myfun,limits=qrect(-140,-140,140,140))
##  v <- qplotView(s)
##  v$show()

