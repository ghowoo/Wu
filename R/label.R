#' A label Function
#'
#' This function allows you to get label of a variable.
#' @param 
#' @keywords label
#' @export
#' @examples
#' label()

label <- function(obj){
  rtn <- attr(obj, which="label")
  if(is.null(rtn)) {
    rtn <- ""
  }
  return(rtn)
}




