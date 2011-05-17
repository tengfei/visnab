##-----------------------------------------------------------------##
##                Class Union Used
##-----------------------------------------------------------------##

GraphicPars.gen <- setRefClass("GraphicPars",
                         fields=c(signalingField("bgColor","character"),
                           signalingField("bgAlpha","numericORNULL"),
                           signalingField("fgColor","character"),
                           signalingField("fill","character"),
                           signalingField("stroke","character"),
                           signalingField("alpha","numeric"),
                           signalingField("gridBgColor","character"),
                           signalingField("gridColor","character"),
                           signalingField("attrs","list"),
                           signalingField("hoverColor","character"),
                           signalingField("textColor","character"),
                           signalingField("xlimZoom","numericORNULL"),
                           signalingField("ylimZoom","numericORNULL"),
                           signalingField("xlim","numericORNULL"),
                           signalingField("ylim","numericORNULL"),
                           signalingField("seqname","characterORNULL"),
                           signalingField("geom","characterORNULL"),
                           signalingField("cpal","functionORNULL"),
                           signalingField("dpal","functionORNULL")
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


