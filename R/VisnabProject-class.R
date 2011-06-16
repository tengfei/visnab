######################################################################
## TOP level class "VisnabProject"
######################################################################

VisnabProject.gen <- setRefClass("VisnabProject",
                                 fields = list(views = "VisnabViewList"))

## constructor
VisnabProject <- function(...){
  lst <- list(...)
  if(length(lst)>1 && any(lapply, is, "VisnabView"))
    vvlst <- VisnabViewList(...)
  obj <- VisnabProject.gen$new(views = vvlst)
  obj
}

VisnabProject.gen$methods(show = function(){
  view$show()
})

setMethod("print","VisnabProject",function(x,..){
  x$show()
})
