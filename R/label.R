#' A label Function
#'
#' This function allows you to get the attribute of label of a object. It is a modified version of the var_label function from the "labelled" package and allows to assign NULL value to a label.
#' @param 
#' @keywords label
#' @export
#' @examples
#' label()


label <- function(obj) {
  UseMethod("label")
}


label.default <- function(obj) {
    rtn <- attr(obj, which = "label", exact = TRUE)
    if (is.null(rtn)) {
        rtn <- ""
    }
    return(rtn)
}


`label<-` <- function(obj, value) {
  UseMethod("label<-")
}


`label<-.default` <- function(obj, value) {
    attr(obj, "label") <- value
    obj
}


## label <- function(obj){
##     rtn <- attr(obj, which="label")
##   if(is.null(rtn)) {
##     rtn <- ""
##   }
##   return(rtn)
## }
