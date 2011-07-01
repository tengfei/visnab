##-----------------------------------------------------------------##
##                Class GrahpicPars
##-----------------------------------------------------------------##
setOldClass("R::visnab::ControlPanel")

gparslst <- list(xlimZoom = "numeric",
                ylimZoom= "numeric",
                xlim = "numeric",
                ylim = "numeric",
                view = "character",
                 geom = "SingleEnumORMultipleEnum",
                 cpanel = "R::visnab::ControlPanel")

GraphicPars.gen <- setParameters("Graphic", gparslst, contains = "DefaultTheme")

##----------------------------------------------------------------##
##                Constructor for GraphicsPars
##----------------------------------------------------------------##


##' GraphicPars is the constructor for generating a set of graphic parameters which
##' could control specific view
##'
##' All the parameters stored in a GraphicPars object are signal objects, which listen
##' to user's response, when the signal object is changed, it will emit certain binded
##' function(s), for graphic parameters, it usually just update associated view(s) to
##' make sure the color or other attributes changes on the fly.
##'
##' @title Graphic parameters constructor
##' @param ... pass paramters to update the default list
##' @param view specify a default graphic parameters set for particular View Class.
##' @return a GrahpicPars object, which store all parameteres as fields.
##' @seealso signalingField
##' @author Tengfei Yin <yintengfei@gmail.com>
GraphicPars <- function(..., view = "VisnabView", theme = "default"){
  ## switch geom
  geom <- .Geoms(view)
  gp <- GraphicPars.gen$new(geom = geom)
  gp$setTheme(theme)
  gp$update(...)
  ## FIXME: check if the widget is shown or not.
  ## gp$cpanel <- ControlPanel(gp)
  ## gp$changed$connect(function(name){
  ##   vals <-gp$field(name)
  ##   gp$cpanel$setValue(name, vals)
  ## })
  return(gp)
}

setMethod("show","GraphicParameters",function(object){
  cat("Parameters stored in pars\n")
  for(nm in ls(object@.xData)){
    y <- get(nm,env=object@.xData)
    if((is(y,"character"))||(is(y,"numeric"))){
      cat(nm, " = ", toString(y), "\n")
    }
    if(is(y,"Signal")){
      cat(nm, " = ")
      show(y)
    }
    if(is(y,"list")){
      cat(nm, " = ", "\n")
      str(y)
    }
  }
})

## set back to default
GraphicPars.gen$methods(
                  reset = function(themeName){
                    'reset parameters to default
                    '
                    ## do some thing first
                    callSuper(themeName)
                  })

GraphicPars.gen$methods(
                  update = function(...){
                    'reset parameters to default
                    '
                    ## do some thing first
                    callSuper(...)
                  })


## accessors
GraphicPars.gen$methods(output = function(){
  'output a list of parameters, automatically remove signal function which
   are just used for internal signal emit. This function return a list,
   pars shows the names of parameters;
   value shows value of parameters;
   listeners shows how many signal function associagted with certain parameter;
   class shows class of those parameters.
   '
  ## need to make sure the order are the same
  flds <- getRefClass()$fields()
  idx <- !(flds %in% c("activeBindingFunction","Signal","function",
                       "functionORNULL"))
  flds <- flds[idx]
  ## hard coded exclued variables
  idx <- !(names(flds) %in% paste(".",
                                  c("view", "parinfo", "tooltipinfo",
                                    "exposed", "xlim", "ylim" ,
                                    "xlimZoom", "ylimZoom", "cpanel", "view"), sep = ""))
  flds <- flds[idx]
  idx <- !grepl("^\\.init.", names(flds))
  flds <- flds[idx]
  ## move .init
  cls <- as.character(flds)
  valnames <- gsub("\\.","",names(flds))
  names(cls) <- valnames
  vals <- sapply(valnames, .self$field)
  parinfo2 <- sapply(valnames, function(nm) parinfo[[nm]])
  tooltipinfo2 <- sapply(valnames, function(nm) tooltipinfo[[nm]])
  exposed2 <- sapply(valnames, function(nm) exposed[[nm]])
  valschanged <- paste(valnames, "Changed", sep = "")

  vsignal <- sapply(valschanged, .self$field)
  sigs <- as.numeric(unlist(lapply(vsignal, length)))
  names(sigs) <- valnames
  lst <- list(pars = valnames, value = vals,
              listeners = sigs, class = cls,
              parinfo = parinfo2, tooltipinfo = tooltipinfo2,
              exposed = exposed2
              )
  return(lst)
})

GraphicPars.gen$methods(setTheme = function(themeName){
  "set parameters based on theme name
  "
  .self$reset(themeName)
})

## .GraphicPars.DefaultTheme <- .DefaultTheme
## .GraphicPars.DarkTheme <- .DarkTheme
## GUI
GraphicPars.gen$methods(cp = function(show = TRUE){
  ## if(length(cpanel)){
  ##   print("cpa")

   ## }
     if(show){
       cpanel$show()
     }else{
       cpanel$hide()
     }
})

