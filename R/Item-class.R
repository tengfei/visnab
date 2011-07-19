setRefClass("Item", contains = c("AnnotatedWidget", "VIRTUAL"),
            fields = list(
              checked = "logical"
              ),
            methods = list(
              setChecked = function(bool = TRUE){
                checked <<- bool
              },
              isChecked = function(){
                checked
              },
              initialize = function(...){
                checked <<- FALSE
                callSuper(...)
              }))

setClass("ItemList", representation("VIRTUAL"),
         prototype = prototype(elementType = "Item"),
         contains = "List")

setClass("SimpleItemList", contains = c("ItemList", "SimpleList"),
         prototype = prototype(elementType = "Item"))

ItemList <- function(...)
{
  items <- list(...)
  if (length(items) == 1 && is.list(items[[1L]]))
    items <- items[[1L]]
  if (!all(sapply(items, is, "Item")))
    stop("all elements in '...' must be Item objects")
  ans <- IRanges:::newSimpleList("SimpleItemList", items)
  ans
}

