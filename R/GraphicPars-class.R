##-----------------------------------------------------------------##
##                Class GrahpicPars
##-----------------------------------------------------------------##
setOldClass("R::visnab::ControlPanel")

setRefClass("GraphicParameters",
            contains = c("DefaultTheme", "VIRTUAL"),
            methods = list(
              reset = function(themeName){
                'reset parameters to default
                    '
                ## do some thing first
                callSuper(themeName)
              },
              
              update = function(...){
                'reset parameters to default
                    '
                ## do some thing first
                callSuper(...)
              },
              
              output = function(){
                'output a list of parameters, automatically remove signal
                 function which are just used for internal signal emit.
                 This function return a list, pars shows the names of parameters;
                 value shows value of parameters; listeners shows how many
                 signal function associagted with certain parameter;
                 class shows class of those parameters.
                 '
                ## need to make sure the order are the same
                flds <- getRefClass()$fields()
                idx <- !(flds %in% c("activeBindingFunction","Signal","function",
                                     "functionORNULL"))
                flds <- flds[idx]
                ## hard coded exclued variables
                idx <- !(names(flds) %in%
                         paste(".",  c("view", "parinfo", "tooltipinfo",
                                       "exposed", "xlim", "ylim" ,
                                       "xlimZoom", "ylimZoom", "cpanel", "view"),
                               sep = ""))
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
              },

              setTheme = function(themeName){
                "set parameters based on theme name
                "
                .self$reset(themeName)
              },
              
              cp = function(show = TRUE){
                ## if(!length(cpanel)){

                ## }
                if(show){
                  cpanel$show()
                }else{
                  cpanel$hide()
                }
              })
            )


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

setGraphicPars <- function(viewname, gparslst,
                           contains = c("GraphicParameters"),
                           signalName = "GParsChanged",
                           where = topenv(parent.frame())){
  names <- paste(viewname, "Graphic", sep = "")
  setParameters(names, gparslst,
                contains = contains,
                signalName = signalName)
}

.AllVisnabViews <- c("VisnabView",
                     "IntervalView",
                     "TxdbView",
                     "CoverageView",
                     "AlignmentView",
                     "ScaleView",
                     "SingleChromView",
                     "SeqView",
                     "TracksView")

sapply(.AllVisnabViews, function(viewname){
  gparslst <- list(xlimZoom = "numeric",
                   ylimZoom= "numeric",
                   xlim = "numeric",
                   ylim = "numeric",
                   view = "character",
                   ## fix on active binding of this geom
                   geom = .GeomName(viewname),
                   cpanel = "R::visnab::ControlPanel")
  setGraphicPars(viewname, gparslst)
})

.GParsName <- function(viewname){
  paste(viewname, "GraphicParameters", sep = "")
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
  gp$setTheme(theme)
  gp$update(...)
  gp$cpanel <- ControlPanel(gp)
  gp$ThemeChanged$connect(function(name){
    vals <-gp$field(name)
    gp$cpanel$setValue(name, vals)
  })
  ## FIXME: check if the widget is shown or not.
  return(gp)
}




