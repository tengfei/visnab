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

setGeneric("validateChr",function(obj,...) standardGeneric("validateChr"))
## GUI
setGeneric("visnabGUI",function(obj,...) standardGeneric("visnabGUI"))
