setRefClass("Group", contains = c("AnnotatedWidget", "VIRTUAL"))
setGroup <- function(name, 
                     contains = character(),
                     where = topenv(parent.frame())){
  nm <- paste(name, "Group", sep = "")
  setRefClass(nm, 
              properties(
                         fields = list(
                           defaultId = "numeric",
                           exclusive = "logical",
                           curId = "numeric",
                           items = "SimpleItemList"
                           )),
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
                  curId <<- id()
                },
                setExclusive = function(bool = TRUE){
                  exclusive <<- bool
                },
                widget = function(id){
                  if(missing(id))
                    id <- id()
                  if(length(id)>1){
                    warning("only support exlusive mode now, use the first")
                    id <- id[1]
                  }
                  ## cp <- Qt$QDockWidget()
                  ParametersControlPanel(items[[id]]$pars)
                  ## items$pars
                  ## par <- item$.pars
                  
                  ## widget(par)
                  ## widget(items[[id]]$pars)
                  ## w$addDockWidget(Qt$Qt$LeftDockWidgetArea, cp)
                },
                menu = function(w){
                  if(missing(w))
                    w <- Qt$QMainWindow()
                  if(length(id()) < 1) setId(defaultId)
                  ## create actions for each item in group
                  m <- Qt$QMenu(text)
                  actionList <- list()
                  length(actionList) <- length(items)
                  actionGroup <- Qt$QActionGroup(w)
                  actionGroup$setExclusive(exclusive)

                  ## put default ID in list first, then add separator
                  dID <- defaultId
                  actionList[[dID]] <- Qt$QAction(items[[dID]]$text, actionGroup)
                  actionList[[dID]]$setCheckable(TRUE)
                  actionList[[dID]]$setChecked(TRUE)
                  qconnect(actionList[[dID]], "triggered", function(){
                    setId(dID)
                  })
                  
                  actionGroup$addAction(actionList[[dID]])
                  m$addAction(actionList[[dID]])
                  m$addSeparator()

                  sapply((1:length(items))[-dID], function(i) {
                    actionList[[i]] <- Qt$QAction(items[[i]]$text, actionGroup)
                    actionList[[i]]$setCheckable(TRUE)
                    qconnect(actionList[[i]], "triggered", function(){
                      setId(i)
                    })
                    actionGroup$addAction(actionList[[i]])
                    m$addAction(actionList[[i]])
                  })
                  m
                },
                initialize = function(t = as.character(class(.self)),...){
                  exclusive <<- TRUE
                  defaultId <<- 1
                  curId <<- 1
                  ## .self$setId(defaultId)
                  ## items[[defaultId]]$checked <<- TRUE
                  .self$text <<- t
                  ## .self$setId(defaultId)
                  .self$regSignal()
                  callSuper(...)
                },
                regSignal = function(){
                  exclusiveChanged$connect(function(){
                    .self$setId(defaultId)
                  })
                }),contains = c("Group", contains),
              where = where)
}


IModeGroup.gen <- setGroup("IMode")
IModeGroup <- function(scaleMode = ScaleMode(),
                       brushMode = BrushMode(),
                       identifyMode = IdentifyMode(),
                       exclusive = TRUE,
                       text = "Interaction Group"){
  
  items <- ItemList(scaleMode = scaleMode,
                    brushMode = brushMode,
                    identifyMode = identifyMode)
  obj <- IModeGroup.gen$new(items = items, exclusive = exclusive)
  obj$setText(text)
  obj
}

