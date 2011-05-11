##-----------------------------------------------------------------##
##                Class Union Used
##-----------------------------------------------------------------##

setClassUnion('numericORNULL',c('numeric','NULL'))
setClassUnion('characterORNULL',c('character','NULL'))
setClassUnion('vectorORNULL',c('vector','NULL'))



GPars.gen <- setRefClass("GraphicPars",
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
                           signalingField("default","list")
                           ))

##----------------------------------------------------------------##
##                Constructor for GraphicsPars
##----------------------------------------------------------------##

GraphicPars <- function(bgColor="gray80",
                        fgColor="black",
                        textColor="black",
                        fill="black",
                        stroke="black",
                        alpha=1,
                        bgAlpha=1,
                        gridBgColor="gray80",
                        gridColor="white",
                        hoverColor="blue",
                        geom=NULL,
                        xlimZoom=NULL,
                        ylimZoom=NULL,
                        seqname=NULL){

  dfs <- list(bgColor=bgColor,
              bgAlpha=bgAlpha,
              fgColor=fgColor,
              textColor=textColor,
              fill=fill,
              stroke=stroke,
              alpha=alpha,
              gridBgColor=gridBgColor,
              gridColor=gridColor,
              ## attrs=list(),
              hoverColor=hoverColor,
              geom=geom)

  
  gp <- GPars.gen$new(bgColor=bgColor,
                      fgColor=fgColor,
                      textColor=textColor,
                      fill=fill,
                      stroke=stroke,
                      alpha=alpha,
                      bgAlpha=bgAlpha,
                      gridBgColor=gridBgColor,
                      gridColor=gridColor,
                      attrs=list(),
                      hoverColor=hoverColor,
                      xlimZoom=xlimZoom,
                      ylimZoom=ylimZoom,
                      seqname=seqname,
                      geom=geom,
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



