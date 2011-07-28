## ======================================================================
## Used for record mouse position and something
## ======================================================================
EventTrace.gen <- setRefClass("EventTrace",
                              fields = signalingFields(list(
                                focusin = "logical",
                                selfSignal = "logical",
                                hoverPos = "numeric",
                                hoverId = "integer",
                                flag = "logical"
                                )),
                              methods = list(
                                initialize = function(...){
                                  flag <<- FALSE
                                  focusin <<- FALSE
                                  selfSignal <<- FALSE
                                  callSuper(...)
                                }
                                ))
