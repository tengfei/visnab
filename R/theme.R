## ##' <description>
## ##'
## ##' <details>
## ##' @title 
## ##' @param theme.name
## ##' @param ... 
## ##' @return 
## ##' @author tengfei
## ##' @export
## visTheme <- function(theme.name,...){
##   newlst <- .setDefaultTheme()
##   lst <- pushTheme(newlst,...)
##   visenv$themes[[theme.name]] <- lst
## }
## ##' <description>
## ##'
## ##' <details>
## ##' @title 
## ##' @return 
## ##' @author tengfei
## ##' @export
## .setDefaultTheme <- function(){
##   newlst <- list()
##   newlst$col <- "gray70"
##   newlst$alpha <- 0.2
##   newlst$highlight.col <- "red"
##   newlst$text.col <- "gray70"
##   newlst$bg.col <- "white"
##   newlst$bg.alpha <- 1
##   return(newlst)
## }

## ##' <description>
## ##'
## ##' <details>
## ##' @title 
## ##' @param theme 
## ##' @param ... 
## ##' @return 
## ##' @author tengfei
## ##' @export
## pushTheme <- function(theme,...){
##   lst <- list(...)
##   nms <- names(lst)
##   idx <- nms %in% names(theme)
##   if(sum(!idx)>0)
##     nlst <- c(theme,lst[!idx])
##   if(sum(idx)>0){
##     nlst[[nms[idx]]] <- lst[[nms[idx]]]
##     message(paste(nms[idx],"is replaced"))
##   }
##   nlst
## }
## ##' <description>
## ##'
## ##' <details>
## ##' @title 
## ##' @param name 
## ##' @param theme 
## ##' @return 
## ##' @author tengfei
## ##' @export
## getAttr <- function(name,theme="default"){
##   visenv$themes[[theme]][[name]]
## }
