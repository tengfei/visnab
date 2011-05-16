##-----------------------------------------------------------------##
##                Class Union Used
##-----------------------------------------------------------------##

setClassUnion("numericORNULL", c("numeric","NULL"))
setClassUnion("characterORNULL", c("character", "NULL"))
setClassUnion("vectorORNULL", c("vector","NULL"))
setClassUnion("functionORNULL", c("function","NULL"))


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
  lst <- list(...)
  bioc <- options("BioC")
  lst.def <- bioc$BioC$visnab[[view]]
  if(length(lst)>0){
     ## update A with B
    lst.new <- update_opts(lst.def, lst)
  }else{
    lst.new <- lst.def
  }
  gp <- do.call(GraphicPars.gen$new,lst.new)
}

update_opts <- function(old, new){
  nms.new <- names(new)
  nms.old <- names(old)
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
    old[[nm]] <- new.exist[[i]]
  }
  return(old)
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
                  reset = function(){
                    dfs <- options("BioC")$visnab[["VisnabView"]]
                    nms <- names(dfs)
                    for(nm in nms){
                      assign(nm,dfs[[nm]],env=.self@.xData)
                    }
                  }
                  )


