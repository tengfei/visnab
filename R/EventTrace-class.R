## ======================================================================
## Used for record mouse position and something
## ======================================================================
EventTrace.gen <- setPropertySet("EventTrace",
                                 fields = list(
                                   focusin = "logical",
                                   selfSignal = "logical",
                                   hoverPos = "numeric",
                                   hoverId = "integer",
                                   flag = "logical"
                                   ),
                                   prototype = list(
                                     ),
                                 methods = list(
                                   initialize = function(...){
                                     flag <<- FALSE
                                     focusin <<- FALSE
                                     selfSignal <<- FALSE
                                     callSuper(...)
                                   }
                                   ))
