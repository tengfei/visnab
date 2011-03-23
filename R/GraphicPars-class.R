##-----------------------------------------------------------------##
##                Class Union Used
##-----------------------------------------------------------------##

setClassUnion('numericORNULL',c('numeric','NULL'))
setClassUnion('characterORNULL',c('character','NULL'))
setClassUnion('vectorORNULL',c('vector','NULL'))
## need to be fixed later
setClassUnion('characterORlogical',c('character','logical'))



GPars.gen <- setRefClass("GraphicPars",
                         fields=c(signalingField("bgColor","character"),
                           signalingField("fgColor","character"),
                           signalingField("fill","character"),
                           signalingField("stroke","characterORlogical"),
                           signalingField("alpha","numeric"),
                           signalingField("attrs","list"),
                           signalingField("hoverColor","character"),
                           signalingField("default","list")
                           ))

##----------------------------------------------------------------##
##                Constructor for GraphicsPars
##----------------------------------------------------------------##

GraphicPars <- function(bgColor="white",
                        fgColor="black",
                        fill="black",
                        stroke="black",
                        alpha=1,
                        hoverColor="blue"){

  dfs <- list(bgColor=bgColor,
              fgColor=fgColor,
              fill=fill,
              stroke=stroke,
              alpha=alpha,
              ## attrs=list(),
              hoverColor=hoverColor)

  
  gp <- GPars.gen$new(bgColor=bgColor,
                      fgColor=fgColor,
                      fill=fill,
                      stroke=stroke,
                      alpha=alpha,
                      attrs=list(),
                      hoverColor=hoverColor,
                      default=dfs)
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

GPars.gen$methods(
                  reset = function(default=NULL){
                    if(is.null(default))
                      dfs <- .self$default
                    nms <- names(dfs)
                    for(nm in nms){
                      assign(nm,dfs[[nm]],env=.self@.xData)
                    }
                  }
                  )



