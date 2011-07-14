## keyPressEventZoom
keyPressEventZoom <- function(obj, view, sx = 1.5, sy = 1.5,
                              browser = TRUE, focusin = focusin){

  if(missing(obj)){
    function(layer, event){
      focusin <<- TRUE
      if(event$modifiers() == Qt$Qt$ControlModifier){
        if(event$key() == Qt$Qt$Key_Equal)
          view$scale(sx, sy)
        if(event$key() == Qt$Qt$Key_Minus)
          view$scale(1/sx, sy)
        if(event$key() == Qt$Qt$Key_0)
          view$resetTransform()
      }
      if(event$modifiers() == Qt$Qt$AltModifier){
        if(event$key() == Qt$Qt$Key_Equal)
            view$scale(sx, 1)
        if(event$key() == Qt$Qt$Key_Minus)
          view$scale(1/sx, 1)
        if(event$key() == Qt$Qt$Key_0)
          view$resetTransform()
      }
    }}else{
      function(layer, event){
        focusin <<- TRUE
        if(event$modifiers() == Qt$Qt$ControlModifier){
          if(event$key() == Qt$Qt$Key_Equal)
            view$scale(sx, sy)
          if(event$key() == Qt$Qt$Key_Minus)
            view$scale(1/sx, sy)
          if(event$key() == Qt$Qt$Key_0)
            view$resetTransform()
          if(browser){
            if(event$key() == Qt$Qt$Key_U)
              viewInBrowser(obj, obj$genome)
          }
        }
        if(event$modifiers() == Qt$Qt$AltModifier){
          if(event$key() == Qt$Qt$Key_Equal)
            view$scale(sx, 1)
          if(event$key() == Qt$Qt$Key_Minus)
            view$scale(1/sx, 1)
          if(event$key() == Qt$Qt$Key_0)
            view$resetTransform()
        }
      }
    }}

## wheel
wheelEventZoom <- function(view, sx = 2, sy = 1){
  function(layer, event){
    if (event$delta() < 0)
      sx <- 1/sx
    view$scale(sx, sy)
  }
}

## data should be MutalbeGRanges
hoverMoveEvent <- function(obj, mr){
  function(layer,event){
    rect <- qrect(0,0,1,1)
    mat <- layer$deviceTransform(event)$inverted()
    rect <- mat$mapRect(rect)
    pos <- event$pos()
    rect$moveCenter(pos)
    hits <- layer$locate(rect)+1
    if(length(hits)>=1){
      posS <- event$screenPos()
      hits <- hits[1]
      values(obj$track)$.color[hits] <<- obj$pars$hoverColor
      Qt$QToolTip$showText(posS,getTooltipInfo(mr,hits))
      obj$flag <<- TRUE
    }else{
      if(obj$flag){
        values(obj$track)$.color <<- obj$pars$fill
        obj$flag <<- FALSE
      }
    }
  }
}
