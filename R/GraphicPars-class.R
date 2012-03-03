##-----------------------------------------------------------------##
##                Class GrahpicPars
##-----------------------------------------------------------------##
setOldClass("R::visnab::ControlPanel")

setClass("Parameters", contains = c("VIRTUAL"))

setPars <- function(viewname, pars = list(), prototype = list(),
                           contains = c("Parameters", "PropertySet"),
                           where = topenv(parent.frame())){
  names <- paste(viewname, "Pars", sep = "")
    setPropertySet(names, pars, prototype,
                   contains = contains)
}

.AllVisnabViews <- c("VisnabView",
                     "IntervalView",
                     "TxdbView",
                     "CoverageView",
                     "BamView",
                     "AlignmentView",
                     "ScaleView",
                     "SingleChromView",
                     "SeqView",
                     "TracksView",
                     "CircularView")

sapply(.AllVisnabViews, function(viewname){
  gparslst <- list(xlimZoom = "numeric",
                   ylimZoom= "numeric",
                   xlim = "numeric",
                   ylim = "numeric",
                   view = "character",
                   ## fix on active binding of this geom
                   geom = .GeomName(viewname))
  setPars(viewname, gparslst)
})

.GParsName <- function(viewname){
  paste(viewname, "Pars", sep = "")
}

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
  geom <- .Geom(view)
  cls <- .GParsName(view)
  gp <- new(cls, geom = geom)
  ## gp$setTheme(theme)
  ## gp$update(...)
  return(gp)
}




## reset = function(themeName){
##   'reset parameters to default
##       '
##   ## do some thing first
##   callSuper(themeName)
## },
## update = function(...){
##   'reset parameters to default
##       '
##   ## do some thing first
##   callSuper(...)
## },
## output = function(){
##   'output a list of parameters, automatically remove signal
##    function which are just used for internal signal emit.
##    This function return a list, pars shows the names of parameters;
##    value shows value of parameters; listeners shows how many
##    signal function associagted with certain parameter;
##    class shows class of those parameters.
##    '
##   .self$properties()
## },
## setTheme = function(themeName){
##   "set parameters based on theme name
##   "
##   .self$reset(themeName)
## },
## widget = function(){
##   w <- ControlPanel(.self)
##   ThemeChanged$connect(function(name){
##     vals <-.self$field(name)
##     w$setValue(name, vals)
##   })
##   w
## }


## setMethod("show","Parameters",function(object){
##   cat("Parameters stored in pars\n")
##   for(nm in ls(object@.xData)){
##     y <- get(nm,env=object@.xData)
##     if((is(y,"character"))||(is(y,"numeric"))){
##       cat(nm, " = ", toString(y), "\n")
##     }
##     if(is(y,"Signal")){
##       cat(nm, " = ")
##       show(y)
##     }
##     if(is(y,"list")){
##       cat(nm, " = ", "\n")
##       str(y)
##     }
##   }
## })
