## new theme class
## theme doesn't specify any signal events,
## use GraphicPars to decide which to emit signals
ThemeSingleEnum <- setSingleEnum("Theme", levels = c("default", "dark"))
## class ThemeSingleEnum@className
## ClassName:: ThemeSingleEnum
setClass("Theme", contains = "VIRTUAL")
## setPropertySet("Theme", fields = properties(list(theme = "ThemeSingleEnum")))

## setTheme gives no default, help you 
## use constructor to define default
setTheme <- function(prefix, pars=list(),
                     where = topenv(parent.frame())){
  if(!is.list(pars))
    stop("Parameters passed must be list")
  themename <- paste(prefix, "Theme", sep = "")
  if(length(pars))
    pretheme.gen <- setPropertySet(themename, fields = c(pars, list(theme = "ThemeSingleEnum")),
                                   where = where, contains = c("Theme", "PropertySet"))
  else
    pretheme.gen <- setPropertySet(themename, where = where,
                                   contains = c("Theme", "PropertySet"))

  
  pretheme.gen$methods(update = function(...){
    new <- list(...)
    if(length(new)==1)
      if(is.list(new[[1]]))
        new <- new[[1]]
    nms <- names(new)
    for(nm in nms){
      assign(nm,new[[nm]],env=.self@.xData)
    }
  })
  
  pretheme.gen$methods(reset = function(themeName){
    if(!missing(themeName))
      theme@.Data <<- themeName
    themelst <- switch(theme,
                      default = .DefaultTheme(),
                      dark = .DarkTheme())
    .self$update(themelst)
  })
  return(pretheme.gen)
}


.DefaultTheme <- function(){
  ## parinfo used for showing actual name, easy to read
  parinfolst <- list(bgColor = "Background color",
                     gridBgColor = "Grid background color",
                     gridFgColor = "Grid frontground color",
                     fgColor = "Frontground color",
                     shadowColor = "Shadow Color",
                     textColor = "Text color",
                     color = "General color",
                     fill = "Fill color",
                     bin = "Histogram bin number",
                     stroke = "Stroke color",
                     ## pointSize = "Point Size",
                     alpha = "Alhpha blending(glyphs if any)",
                     bgAlpha = "Alpha blending(Background)",
                     hoverColor = "Color(when mouse hover)",
                     ## xlimZoom = "Viewport Range(x-scale)",
                     ## ylimZoom = "Viewport Range(y-scale)",
                     ## xlim = "Limits on x-scale",
                     ## ylim = "Limits on y-scale",
                     theme = "Theme",
                     geom = "Geometry",
                     cpal = "Palletes(Continuous variables)",
                     dpal = "Palletes(Discrete variables)")



  ## tooltipinfo used for showing tooltip, wihch is descriptive
  tooltipinfolst <- list(bgColor = "no tool tip defined yet",
                         gridBgColor = "Grid Background color",
                         gridFgColor = "Grid frontground color",
                         fgColor = "no tool tip defined yet",
                         shadowColor = "Shadow Color",
                         textColor = "no tool tip defined yet",
                         color = "no tool tip defined yet",
                         fill = "no tool tip defined yet",
                         bin = "PositiveInteger",
                         stroke = "no tool tip defined yet",
                         ## pointSize = "Point size",
                         alpha = "no tool tip defined yet",
                         bgAlpha = "no tool tip defined yet",
                         hoverColor = "no tool tip defined yet",
                         ## xlimZoom = "no tool tip defined yet",
                         ## ylimZoom = "no tool tip defined yet",
                         ## xlim = "no tool tip defined yet",
                         ## ylim = "no tool tip defined yet",
                         theme = "no tool tip defined yet",
                         geom = "no tool tip defined yet",
                         cpal = "no tool tip defined yet",
                         dpal = "no tool tip defined yet")
                         ## fake for GUI test
                         ## fake for GUI test


  ## exposed decide which parameters exposed to users
  exposedlst <- list(bgColor = FALSE,
                     gridBgColor = TRUE,
                     gridFgColor = FALSE,
                     fgColor = FALSE,
                     shadowColor = TRUE,
                     textColor = FALSE,
                     color = FALSE,
                     fill = TRUE,
                     stroke = TRUE,
                     ## pointSize = TRUE,
                     alpha = TRUE,
                     bgAlpha = FALSE,
                     hoverColor = TRUE,
                     bin = TRUE,
                     ## xlimZoom = FALSE,
                     ## ylimZoom = FALSE,
                     ## xlim = FALSE,
                     ## ylim = FALSE,
                     theme = TRUE,
                     geom = TRUE,
                     cpal = FALSE,
                     dpal = FALSE)

  ## default is "white", light them
  def <- list(bgColor = new("bgColorSingleEnum","white"),
              gridBgColor = new("Color", "gray"),
              gridFgColor = new("Color", "white"),
              fgColor = new("Color","black"),
              shadowColor = new("ShadowColorSingleEnum","gray"),
              textColor = new("Color","black"),
              color = I("black"),
              fill = new("Color","black"),
              stroke = new("Color","black"),
              alpha = new("NumericWithMin0Max1", 1),
              bgAlpha = new("NumericWithMin0Max1", 1),
              hoverColor = new("Color","blue"),
              bin = new("PositiveInteger",10),
              theme = new("ThemeSingleEnum", "default"),
              cpal = new("CPalSingleEnum", "identity"),
              dpal = new("DPalSingleEnum", "brewer"),
              parinfo = parinfolst,
              tooltipinfo = tooltipinfolst,
              exposed = exposedlst)


}

.defFields <- function(){
  defFields <- list(bgColor = "bgColorSingleEnum",
                    gridBgColor = "Color",
                    gridFgColor = "Color",
                    bgAlpha = "NumericWithMin0Max1",
                    shadowColor = "ShadowColorSingleEnum",
                    fgColor = "Color",
                    color = "AsIsORcharacter",
                    fill = "Color",
                    stroke = "Color",
                    ## pointSize = "PointSizeSingleEnum",
                    alpha = "NumericWithMin0Max1",
                    bin = "PositiveInteger",
                    hoverColor = "Color",
                    textColor = "Color",
                    cpal = "CPalSingleEnum",
                    dpal = "DPalSingleEnum",
                    parinfo = "list",
                    tooltipinfo = "list",
                    ## geom = "enum",
                    ## theme = "ThemeEnum",
                    exposed = "list")
                    ## fake for GUI test
}


update_opts <- function(..., data){
  new <- list(...)
  if(length(new)==1)
    if(is.list(new[[1]]))
      new <- new[[1]]
  if(!is.list(data))
    stop("data need to be a list\n")
  nms.new <- names(new)
  nms.old <- names(data)
  idx <-  nms.new %in% nms.old
  nms.diff <- nms.new[!idx]
  ## checking names
  for(i in seq_along(nms.diff)){
    cat("variable",nms.diff[i], "not exists\n")
  }
  new.exist <- new[idx]
  ## FIXME: simply replace, not checking types yet
  for(i in seq_along(new.exist)){
    nm <- names(new.exist)[i]
    data[[nm]] <- new.exist[[i]]
  }
  return(data)
}


DefaultTheme.gen <- setTheme("Default", pars = .defFields())
## Constructor
DefaultTheme <- function(...){
  lst <- .DefaultTheme()
  obj <- DefaultTheme.gen$new(theme = new("ThemeSingleEnum", "default"))
  obj$update(lst)
  obj$update(...)
  obj
}

## following theme only revise a little bit
.DarkTheme <- function(){
  lst <- .DefaultTheme()
  lst <- update_opts(list(bgColor = "black"), data = lst)
  lst
}

DarkTheme <- function(...){
  lst <- .DarkTheme()
  obj <- DefaultTheme.gen$new(theme = new("ThemeSingleEnum", "dark"))
  obj$update(lst)
  obj$update(...)
  obj
}

