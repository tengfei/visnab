##-----------------------------------------------------------------##
##                Class Union Used
##-----------------------------------------------------------------##
## FIXME: remove NULL as possible as I can
GraphicPars.gen <- setRefClass("GraphicPars",
                         fields=c(signalingField("bgColor","character"),
                           signalingField("bgAlpha","numeric"),
                           signalingField("fgColor","character"),
                           signalingField("color","AsIsORcharacter"),
                           signalingField("fill","character"),
                           signalingField("stroke","character"),
                           signalingField("alpha","numeric"),
                           signalingField("gridBgColor","character"),
                           signalingField("gridColor","character"),
                           signalingField("hoverColor","character"),
                           signalingField("textColor","character"),
                           signalingField("xlimZoom","numericORNULL"),
                           signalingField("ylimZoom","numericORNULL"),
                           signalingField("xlim","numericORNULL"),
                           signalingField("ylim","numericORNULL"),
                           signalingField("seqname","characterORNULL"),
                           signalingField("seqlength","numericORNULL"),
                           signalingField("geom","characterORNULL"),
                           signalingField("cpal","functionORNULL"),
                           signalingField("dpal","functionORNULL"),
                           signalingField("tipsID","characterORNULL")
                           ))

##----------------------------------------------------------------##
##                Constructor for GraphicsPars
##----------------------------------------------------------------##

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
                    dfs <- options("BioC")$BioC$visnab[[view]]
                    nms <- names(dfs)
                    for(nm in nms){
                      assign(nm,dfs[[nm]],env=.self@.xData)
                    }
                  }
                  )


## accessors
## getPars or setPars just can use $field(name) and $field(name, value)
## need a way to return valid pars, excluding function, and indicate signal or not
GraphicPars.gen$methods(output = function(){
  flds <- pars$getRefClass()$fields()
  idx <- !(flds %in% c("activeBindingFunction","Signal","function",
                       "functionORNULL"))
  ## FIXME: probably the cpal and dpal should be names
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

