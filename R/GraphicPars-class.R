##-----------------------------------------------------------------##
##                Class Union Used
##-----------------------------------------------------------------##
## FIXME: remove NULL as possible as I can
GraphicPars.gen <- setRefClass("GraphicPars",
                         fields=c(signalingField("bgColor","character"),
                           signalingField("bgAlpha","NumericWithRange"),
                           signalingField("fgColor","character"),
                           signalingField("color","AsIsORcharacter"),
                           signalingField("fill","character"),
                           signalingField("stroke","character"),
                           signalingField("alpha","NumericWithRange"),
                           ## signalingField("gridBgColor","character"),
                           ## signalingField("gridColor","character"),
                           signalingField("hoverColor","character"),
                           signalingField("textColor","character"),
                           signalingField("xlimZoom","NumericWithRange"),
                           signalingField("ylimZoom","NumericWithRange"),
                           signalingField("xlim","NumericWithRange"),
                           signalingField("ylim","NumericWithRange"),
                           signalingField("seqname","character"),
                           signalingField("seqlength","numeric"),
                           signalingField("geom","Enum"),
                           signalingField("cpal","CPalEnum"),
                           signalingField("dpal","DPalEnum"),
                           signalingField("tipsID","character")
                           ))

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
##' @examples pars <- GraphicPars()
##' ## to return a parameters list
##' pars$output()
##' ## how to get field
##' pars$field("bgColor")
##' ## how to set field
##' pars$field("bgColor", "black")
##' pars$field("bgColor")
##' ## or
##' pars$bgColor <- "blue"
##' pars$bgColor
##' ## how to get default back
##' pars$reset()
##' pars$bgColor
##' @seealso signalingField
##' @author tengfei
GraphicPars <- function(..., view = "VisnabView"){
  bioc <- options("BioC")
  lst.def <- bioc$BioC$visnab[[view]]
  lst <- list(...)
  if(length(lst)>0){
    lst.new <- update_opts(lst, data=lst.def)
  }else{
    lst.new <- lst.def
  }
  gp <- do.call(GraphicPars.gen$new,lst.new)
  return(gp)
}

setMethod("show","GraphicPars",function(object){
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
                  reset = function(view = "VisnabView"){
                    'reset parameters to default
                    '
                    dfs <- options("BioC")$BioC$visnab[[view]]
                    nms <- names(dfs)
                    for(nm in nms){
                      assign(nm,dfs[[nm]],env=.self@.xData)
                    }
                  }
                  )


## accessors
GraphicPars.gen$methods(output = function(){
  'output a list of parameters, automatically remove signal function which
   are just used for internal signal emit. This function return a list,
   pars shows the names of parameters;
   value shows value of parameters;
   listeners shows how many signal function associagted with certain parameter;
   class shows class of those parameters.
   '
  flds <- pars$getRefClass()$fields()
  idx <- !(flds %in% c("activeBindingFunction","Signal","function",
                       "functionORNULL"))
  flds <- flds[idx]
  cls <- as.character(flds)
  valnames <- gsub("\\.","",names(flds))
  vals <- sapply(valnames, .self$field)
  valschanged <- paste(valnames, "Changed", sep = "")
  vsignal <- sapply(valschanged, pars$field)
  sigs <- as.numeric(unlist(lapply(vsignal, length)))
  lst <- list(pars = valnames, value = vals, listeners = sigs, class = cls)
  return(lst)
})

