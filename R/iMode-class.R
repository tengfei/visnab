## ======================================================================
## Enum used in iMode class
## ======================================================================
setSingleEnum("TooltipMode", levels = c("Off", "Identify", "Metainfo","Text",
                               "Position"))
setSingleEnum("TooltipPos", levels = c("TopLeft", "TopRight",
                                       "BottomLeft", "BottomRight",
                                       "Float"))
setSingleEnum("BrushMode", levels = c("Transient", "Persistent", "Off"))
setSingleEnum("PointBrushMode", levels = c("Off", "ColorAndGlyph",
                                  "ColorOnly", "GlyphOnly",
                                  "Shadow", "Unshadow"))
setSingleEnum("EdgeBrushMode", levels = c("Off", "ColorAndLine",
                                 "ColorOnly", "LineOnly", "Shadow", "Unshadow"))
setSingleEnum("DragMode",  levels = c("NoDrag", "ScrollHandDrag", "RubberBandDrag"))
setGeneric("getQtEnum", function(x,...) standardGeneric("getQtEnum"))
setMethod("getQtEnum", "DragModeSingleEnum", function(x){
  val <- switch(x,
                NoDrag = Qt$QGraphicsView$NoDrag,
                ScrollHandDrag = Qt$QGraphicsView$ScrollHandDrag,
                RubberBandDrag = Qt$QGraphicsView$RubberBandDrag)
})


setSingleEnum("ZoomMode", levels = c("Vertical", "Horizontal", "Both", "Off"))

## ======================================================================
## iMode class
## ======================================================================
setClass("iMode", contains = "VIRTUAL")

setMode <- function(name, modes,
                    contains = character(),
                    where = topenv(parent.frame())){
  nm <- paste(name, "Mode", sep = "")
  ## contains <- c("PropertySet", contains)
  parsName <- paste(nm, "Parameters", sep = "")
  pars <- setPropertySet(parsName, modes)
  ## parsName <- pars$className
  setRefClass(nm, fields = list(pars = pars$className),
              contains = c("iMode",  "Item", contains),
              where = where,
              methods = list(
                initialize = function(t = as.character(class(.self)), ...){
                  .self$text <<- t 
                  callSuper(...)
                }))
}



ScaleMode.gen <- setMode("Scale",
                          list(dragMode = "DragModeSingleEnum",
                               zoomMode = "ZoomModeSingleEnum"))

ScaleMode <- function(dragMode = "ScrollHandDrag",
                      zoomMode = "Vertical",
                      ...){
  pars <- new("ScaleModeParameters", dragMode = dragMode, zoomMode = zoomMode)
  obj <- ScaleMode.gen$new(pars = pars, ...)
}


BrushMode.gen <- setMode("Brush",
                          list(brushMode = "BrushModeSingleEnum",
                               pointBrushMode = "PointBrushModeSingleEnum",
                               edgeBrushMode = "EdgeBrushModeSingleEnum"))

BrushMode <- function(brushMode = "Off",
                      pointBrushMode = "Off",
                      edgeBrushMode = "Off",
                      ...){
  pars <- new("BrushModeParameters", brushMode = brushMode,
              pointBrushMode = pointBrushMode,
              edgeBrushMode = edgeBrushMode)
  obj <- BrushMode.gen$new(pars = pars, ...)
}

setSingleEnum("Logical", levels = c("TRUE", "FALSE"))
IdentifyMode.gen <- setMode("Identify",
                            list(tooltipMode = "TooltipModeSingleEnum",
                                 tooltipPos = "TooltipPosSingleEnum",
                                 hoverMode = "LogicalSingleEnum"))

IdentifyMode <- function(tooltipMode = "Off",
                         tooltipPos = "Float",
                         hoverMode = "FALSE",
                         ...){
  pars <- new("IdentifyModeParameters",
              tooltipMode = tooltipMode,
              tooltipPos = tooltipPos,
              hoverMode = hoverMode)
  obj <- IdentifyMode.gen$new(pars = pars, ...)
}

