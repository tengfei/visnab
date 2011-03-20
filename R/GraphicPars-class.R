##-----------------------------------------------------------------##
##                Class Union Used
##-----------------------------------------------------------------##


setClassUnion('numericOrNULL',c('numeric','NULL'))
setClassUnion('characterOrNULL',c('character','NULL'))
setClassUnion('vectorOrNULL',c('vector','NULL'))


setClass('GraphicPars',representation(pars='environment'),
         prototype(pars=new.env()))

##----------------------------------------------------------------##
##                Methods for GraphicsPars
##----------------------------------------------------------------##

setGeneric('setPar',function(obj,...) standardGeneric('setPar'))
setMethod('setPar','GraphicPars',
          function(obj,name,value){
            assign(name,value,obj@pars)
          })

setGeneric('getPar',function(obj,...) standardGeneric('getPar'))
setMethod('getPar','GraphicPars',
          function(obj,name){
            if(!exists(name,obj@pars))
              stop(paste('No graphic parameter named',name,'could be found!'))
            get(name,obj@pars)
          })

setMethod('show','GraphicPars',function(object){
  cat("Parameters stored in pars\n")
  sapply(ls(object@pars), function(x) {
    y <- getPar(object, x)
    if (length(y) > 10)
      y <- y[1:10]
    cat(x, " = ", toString(y), "\n")
  })
  cat(names(object@pars)[11:length(object@pars)])
})

pushCon <- function(gp1, gp2) {
  if (is.null(gp1) && is.null(gp2))
    return(DisplayPars())
  if (is.null(gp1)) 
    return(gp2)
  if (is.null(gp2))
    return(gp1)
  sapply(ls(gp1@pars), function(nm) {
    setPar(gp2, nm, getPar(gp1, nm))
  })
  return(gp2)
}

##----------------------------------------------------------------##
##                Constructor for GraphicsPars
##----------------------------------------------------------------##

GraphicPars <- function(...){
  args <- list(...)
  gp <- new('GraphicPars')
  ##  i <- match(names(args),'gp')
  if(length(args)>0){
    lapply(1:length(args),function(i){
      setPar(gp,names(args)[i],args[[i]])
    })
  }
  gp
}

VisnabGraphicPars <- function(){
  GrahpicPars(.bgColor="white",
              .ftColor="black",
              .stroke="black",
              .fill="back")
}










