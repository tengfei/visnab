##-----------------------------------------------------------------##
##                Class Union Used
##-----------------------------------------------------------------##

setClassUnion('numericORNULL',c('numeric','NULL'))
setClassUnion('characterORNULL',c('character','NULL'))
setClassUnion('vectorORNULL',c('vector','NULL'))

GPars.gen <- setRefClass("GraphicPars",
                         fields=c(signalingField("bgcolor","character"),
                           signalingField("fgcolor","character"),
                           signalingField("fill","character"),
                           signalingField("stroke","character"),
                           signalingField("alpha","numeric")
                           ))

##----------------------------------------------------------------##
##                Constructor for GraphicsPars
##----------------------------------------------------------------##

GraphicPars <- function(bgcolor="white",
                        fgcolor="black",
                        fill="black",
                        stroke="black",
                        alpha=1){
  gp <- GPars.gen$new(bgcolor=bgcolor,
                      fgcolor=fgcolor,
                      fill=fill,
                      stroke=stroke,
                      alpha=alpha)
  return(gp)
}

setMethod("show","GraphicPars",function(object){
  cat("Parameters stored in pars\n")
  sapply(ls(object@.xData), function(x) {
    y <- get(x,env=object@.xData)
    if(!is(y,"Signal")){
    cat(x, " = ", toString(y), "\n")
  }
  })
})

