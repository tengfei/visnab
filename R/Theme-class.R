## new theme class
## theme doesn't specify any signal events,
## use GraphicPars to decide which to emit signals
ThemeSingleEnum <- setSingleEnum("Theme", levels = c("default", "dark"))
setRefClass("Theme", fields = signalingFields(list(theme = "ThemeSingleEnum")),
            contains = "VIRTUAL")

## setTheme gives no default
## use constructor to define default
setTheme <- function(prefix, pars=list(),
                     where = topenv(parent.frame()),
                     signalName = "ThemeChanged"){
  if(!is.list(pars))
    stop("Parameters passed must be list")
  themename <- paste(prefix, "Theme", sep = "")
  contains <- "Theme"
  if(length(pars))
    pretheme.gen <- setRefClass(themename, fields = signalingFields(pars,
                                             signalName = signalName),
                                contains = contains,
                                where = where)
  else
    pretheme.gen <- setRefClass(themename,
                                contains = contains, where = where)
  
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
                     fgColor = "Frontground color",
                     textColor = "Text color",
                     color = "General color",
                     fill = "Fill color",
                     stroke = "Stroke color",
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
                     dpal = "Palletes(Discrete variables)",
                      ## fake for GUI test
                     fake1 = "PositiveInteger",
                     fake2 = "NonnegativeInteger",
                     fake3 = "NegativeInteger",
                     fake4 = "NonpositiveInteger",
                     fake6 = "MultipleEnum",
                     fake7 = "character")

  ## tooltipinfo used for showing tooltip, wihch is descriptive
  tooltipinfolst <- list(bgColor = "no tool tip defined yet",
                         fgColor = "no tool tip defined yet",
                         textColor = "no tool tip defined yet",
                         color = "no tool tip defined yet",
                         fill = "no tool tip defined yet",
                         stroke = "no tool tip defined yet",
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
                         dpal = "no tool tip defined yet",
                         ## fake for GUI test
                         ## fake for GUI test
                         fake1 = "PositiveInteger",
                         fake2 = "NonnegativeInteger",
                         fake3 = "NegativeInteger",
                         fake4 = "NonpositiveInteger",
                         fake6 = "MultipleEnum",
                         fake7 = "character")


  ## exposed decide which parameters exposed to users
  exposedlst <- list(bgColor = TRUE,
                     fgColor = TRUE,
                     textColor = TRUE,
                     color = TRUE,
                     fill = TRUE,
                     stroke = TRUE,
                     alpha = TRUE,
                     bgAlpha = TRUE,
                     hoverColor = TRUE,
                     ## xlimZoom = FALSE,
                     ## ylimZoom = FALSE,
                     ## xlim = FALSE,
                     ## ylim = FALSE,
                     theme = TRUE,
                     geom = TRUE,
                     cpal = TRUE,
                     dpal = TRUE,
                     ## fake for GUI test
                     fake1 = TRUE,
                     fake2 = TRUE,
                     fake3 = TRUE,
                     fake4 = TRUE,
                     fake6 = TRUE,
                     fake7 = TRUE)

  ## default is "white", light them
  
  def <- list(bgColor = new("Color","white"),
              fgColor = new("Color","black"),
              textColor = new("Color","black"),
              color = I("red"),
              fill = new("Color","black"),
              stroke = new("Color","black"),
              alpha = new("NumericWithMin0Max1", 1),
              bgAlpha = new("NumericWithMin0Max1", 1),
              hoverColor = new("Color","blue"),
              ## xlimZoom = numeric(),
              ## ylimZoom = numeric(),
              ## xlim = numeric(),
              ## ylim = numeric(),
              ## geom = new("Enum"),
              theme = new("ThemeSingleEnum", "default"),
              cpal = new("CPalSingleEnum", "identity"),
              dpal = new("DPalSingleEnum", "brewer"),
              parinfo = parinfolst,
              tooltipinfo = tooltipinfolst,
              exposed = exposedlst,
              ## fake for GUI test
              fake1 = new("PositiveInteger", 1L),
              fake2 = new("NonnegativeInteger", 0L),
              fake3 = new("NegativeInteger", -1L),
              fake4 = new("NonpositiveInteger", 0L),
              fake6 = new("MultipleEnum", levels = LETTERS[1:10], c("A", "B")),
              fake7 = "input text")
}

.defFields <- function(){
  defFields <- list(bgColor = "Color",
                    bgAlpha = "NumericWithMin0Max1",
                    fgColor = "Color",
                    color = "AsIsORcharacter",
                    fill = "Color",
                    stroke = "Color",
                    alpha = "NumericWithMin0Max1",
                    hoverColor = "Color",
                    textColor = "Color",
                    cpal = "CPalSingleEnum",
                    dpal = "DPalSingleEnum",
                    parinfo = "list",
                    tooltipinfo = "list",
                    ## geom = "enum",
                    ## theme = "ThemeEnum",
                    exposed = "list",
                    ## fake for GUI test
                    fake1 = "PositiveInteger",
                    fake2 = "NonnegativeInteger",
                    fake3 = "NegativeInteger",
                    fake4 = "NonpositiveInteger",
                    fake6 = "MultipleEnum",
                    fake7 = "character")
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
  obj <- DefaultTheme.gen$new(theme = new("ThemeEnum", "default"))
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
  obj <- DefaultTheme.gen$new(theme = new("ThemeEnum", "dark"))
  obj$update(lst)
  obj$update(...)
  obj
}


