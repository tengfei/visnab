## ======================================================================
## Used for record mouse position and something
## ======================================================================
EventTrace.gen <- setRefClass("EventTrace",
                              fields = signalingFields(list(
                                focusin = "logical",
                                selfSignal = "logical",
                                hoverPos = "numeric"
                                )),
                              methods = list(
                                initialize = function(...){
                                  focusin <<- FALSE
                                  selfSignal <<- FALSE
                                  callSuper(...)
                                }
                                ))
