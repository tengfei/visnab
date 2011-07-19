## ======================================================================
## Enum used in iMode class
## ======================================================================
setSingleEnum("TooltipMode", levels = c("Off", "Identify", "Metainfo"))
setSingleEnum("TooltipPos", levels = c("TopLeft", "TopRight",
                                       "BottomLeft", "BottomRight",
                                       "Float"))
setSingleEnum("BrushMode", levels = c("Transient", "Persistent"))
setSingleEnum("PointBrushMode", levels = c("Off", "ColorAndGlyph",
                                  "ColorOnly", "GlyphOnly",
                                  "Shadow", "Unshadow"))
setSingleEnum("EdgeBrushMode", levels = c("Off", "ColorAndLine",
                                 "ColorOnly", "LineOnly", "Shadow", "Unshadow"))
setSingleEnum("DragMode",  levels = c("NoDrag", "ScrollHandDrag", "RubberBandDrag"))
setSingleEnum("ZoomMode", levels = c("Vertical", "Horizontal", "Both"))

## ======================================================================
## iMode class
## ======================================================================
setClass("iMode", contains = "VIRTUAL")
setMode <- function(name, modes,
                    contains = character(),
                    where = topenv(parent.frame())){
  nm <- paste(name, "Mode", sep = "")
  pars <- setParameters(nm, modes)
  parsName <- paste(nm, "Parameters", sep = "")
  setRefClass(nm, fields = list(pars = parsName),
              contains = c("iMode", "Item", contains),
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
BrushMode <- function(brushMode = "Transient",
                      pointBrushMode = "Off",
                      edgeBrushMode = "Off",
                      ...){
  pars <- new("BrushModeParameters", brushMode = brushMode,
              pointBrushMode = pointBrushMode,
              edgeBrushMode = edgeBrushMode)
  obj <- BrushMode.gen$new(pars = pars, ...)
}


IdentifyMode.gen <- setMode("Identify",
                            list(tooltipMode = "TooltipModeSingleEnum",
                                 tooltipPos = "TooltipPosSingleEnum",
                                 hoverMode = "logical"))
IdentifyMode <- function(tooltipMode = "Identify",
                         tooltipPos = "Float",
                         ...){
  pars <- new("IdentifyModeParameters",
              tooltipMode = tooltipMode,
              tooltipPos = tooltipPos)
  obj <- IdentifyMode.gen$new(pars = pars, ...)
}

