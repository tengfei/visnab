setClass("Group", contains = "VIRTUAL")
## items should be a mutalist of Item object
setGroup <- function(name, 
                     contains = character(),
                     where = topenv(parent.frame())){
  nm <- paste(name, "Group", sep = "")
  setRefClass(nm, contains = c("Group", contains),
              where = where,
            fields = c(
              defaultId = "numeric",
              signalingField("exclusive", "logical"),
              items = "SimpleItemList"
              ),
            methods = list(
              checkedId = function(){
                which(unlist(lapply(items, function(item){
                  item$checked
                })))
              },
              setCheckedForAll = function(bool = FALSE){
                sapply(seq(length(items)), function(i){
                  items[[i]]$checked <<- bool
                })
              },
              id = function(){
                which(unlist(lapply(items, function(item){
                  item$checked
                })))
              },
              setId = function(idx){
                if(exclusive){
                  .self$setCheckedForAll(FALSE)
                  items[[idx]]$checked <<- TRUE
                }else{
                  items[[idx]]$checked <<- TRUE
                }
              },
              setExclusive = function(bool = TRUE){
                exclusive <<- bool
              },
              initialize = function(...){
                exclusive <<- TRUE
                defaultId <<- 1
                ## .self$setId(defaultId)
                .self$regSignal()
                callSuper(...)
              },
              regSignal = function(){
                exclusiveChanged$connect(function(){
                  .self$setId(defaultId)
                })
              }))
}


## items <- ItemList(scale = ScaleMode(),
##                   brush = BrushMode(),
##                   identify = IdentifyMode())

IModeGroup.gen <- setGroup("IMode")
IModeGroup <- function(scaleMode = ScaleMode(),
                       brushMode = BrushMode(),
                       identifyMode = IdentifyMode(),
                       exclusive = TRUE){
  
  items <- ItemList(scaleMode = scaleMode,
                    brushMode = brushMode,
                    identifyMode = identifyMode)
  obj <- IModeGroup.gen$new(items = items, exclusive = exclusive)
}

