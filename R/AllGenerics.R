setGeneric("print")


## setGeneric("Geom",function(x,...) standardGeneric("Geom"))
setGeneric("geom",function(x,...) standardGeneric("geom"))
setGeneric("geom<-",function(x,value) standardGeneric("geom<-"))


setGeneric("range<-",function(x,value) standardGeneric("range<-"))
## with "s"
setGeneric("selectedRangesModel", function(obj,...)
           standardGeneric("selectedRangesModel"))
setGeneric("selectedRangesModel<-",function(x,value)
           standardGeneric("selectedRangesModel<-"))


setGeneric("aes",function(x,...) standardGeneric("aes"))
## setGeneric("aes<-", function(x, value) standardGeneric("aes<-"))

setGeneric("viewInBrowser",function(obj,...) standardGeneric("viewInBrowser"))


setGeneric("addLevels",function(obj,...) standardGeneric("addLevels"))

setGeneric("removePrefix",function(obj,...) standardGeneric("removePrefix"))
setGeneric("addPrefix",function(obj,...) standardGeneric("addPrefix"))

setGeneric("validateChr",function(obj,...) standardGeneric("validateChr"))
setGeneric("sortChr",function(obj,...) standardGeneric("sortChr"))
setGeneric("orderChr",function(obj,...) standardGeneric("orderChr"))
setGeneric("replaceChr",function(obj,...) standardGeneric("replaceChr"))

## GUI
setGeneric("visnabGUI",function(obj,...) standardGeneric("visnabGUI"))
