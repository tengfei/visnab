## keyPressEventZoom
keyPressEventZoom <- function(obj, view, sx = 1.5, sy = 1.5, browser = FALSE){
  function(layer, event){
    if(event$modifiers() == Qt$Qt$ControlModifier){
      if(event$key() == Qt$Qt$Key_Equal)
        view$scale(sx, sy)
      if(event$key() == Qt$Qt$Key_Minus)
        view$scale(1/sx, sy)
      if(event$key() == Qt$Qt$Key_0)
        view$resetTransform()
      if(browser)
        if(event$key() == Qt$Qt$Key_u)
          viewInUCSC(obj)
    }
  }
}
## wheel
wheelEventZoom <- function(view, sx = 2, sy = 1){
  function(layer, event){
    if (event$delta() < 0)
      sx <- 1/sx
    view$scale(sx, sy)
  }
}

