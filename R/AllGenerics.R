setGeneric("print")

setGeneric("geom",function(x,...) standardGeneric("geom"))

setGeneric("geom<-",function(x,value) standardGeneric("geom<-"))

setGeneric("viewInUCSC",function(obj,...) standardGeneric("viewInUCSC"))

setGeneric('addLevels',function(mr,...) standardGeneric('addLevels'))

setGeneric('removePrefix',function(gr,...) standardGeneric('removePrefix'))

setGeneric('addPrefix',function(gr,...) standardGeneric('addPrefix'))

setGeneric('validateChr',function(gr,...) standardGeneric('validateChr'))

setGeneric("sortChr",function(obj,...) standardGeneric("sortChr"))

setGeneric("orderChr",function(obj,...) standardGeneric("orderChr"))

setGeneric("replaceChr",function(obj,...) standardGeneric("replaceChr"))
